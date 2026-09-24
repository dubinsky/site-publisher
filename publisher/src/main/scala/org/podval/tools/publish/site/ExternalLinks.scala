package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{AssetRef, Transclusion}
import org.podval.tools.publish.page.PageSource
import org.podval.xml.{Xml, XmlElement}
import scala.collection.mutable
import scala.util.control.NonFatal
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

/** Author-written `http`/`https` URLs, fetched once per generate when `check-links` is on. */
final class ExternalLinks(site: Site):
  // `None` is a live URL.
  private val fetched: mutable.Map[String, Option[(String, Option[Throwable])]] = mutable.Map.empty
  private val seen: mutable.Set[(String, String)] = mutable.Set.empty

  private lazy val http: HttpClient = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(ExternalLinks.connectTimeout)
    .build()

  def check(xml: Xml.Element, source: PageSource): Unit =
    if site.config.checkLinks then walk(xml, source)

  private def walk(element: Xml.Element, source: PageSource): Unit =
    if !Transclusion.isChrome(element) && !element.isElement(XmlElement.Code) then
      urlOf(element).foreach(consider(_, source))
      element.childElements.foreach(walk(_, source))

  private def urlOf(element: Xml.Element): Option[String] =
    val raw: Option[String] =
      if element.isA then element.getHref
      else AssetRef.resourceAttr(element).flatMap(element.get)
    raw.map(_.trim).filter(_.nonEmpty)

  private def consider(href: String, source: PageSource): Unit =
    parse(href).foreach: uri =>
      val scheme: String = Option(uri.getScheme).map(_.toLowerCase).getOrElse("")
      if (scheme == "http" || scheme == "https") && uri.getHost != null && !site.sameSiteHost(uri) then
        val key: String = cacheKey(uri)
        if seen.add((source.sourcePath.toString, key)) then
          fetch(key).foreach: (detail, cause) =>
            source.error(PageError.BrokenLink, s"'$href' ($detail)", cause)

  private def parse(href: String): Option[URI] =
    try Some(URI(href))
    catch case _: java.net.URISyntaxException => None

  private def fetch(url: String): Option[(String, Option[Throwable])] =
    fetched.getOrElseUpdate(url, request(url))

  private def request(url: String): Option[(String, Option[Throwable])] =
    site.log.debug(s"Checking $url")
    try
      val head: Int = status(url, head = true)
      val code: Int = if ExternalLinks.retryWithGet(head) then status(url, head = false) else head
      Option.when(code >= 400)((code.toString, None))
    catch
      case NonFatal(e) =>
        val detail: String = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        Some((detail, Some(e)))

  private def status(url: String, head: Boolean): Int =
    val builder: HttpRequest.Builder = HttpRequest.newBuilder(URI.create(url))
      .timeout(ExternalLinks.requestTimeout)
      .header("User-Agent", ExternalLinks.userAgent)
      .header("Accept", "*/*")
    val request: HttpRequest = if head then builder.HEAD().build() else builder.GET().build()
    http.send(request, HttpResponse.BodyHandlers.discarding()).statusCode()

  // Fragment dropped. Scheme and host compared case-insensitively. Path and query stay as authored.
  private def cacheKey(uri: URI): String =
    val scheme: String = uri.getScheme.toLowerCase
    val host: String = uri.getHost.toLowerCase
    val port: String = if uri.getPort == -1 then "" else s":${uri.getPort}"
    val user: String = Option(uri.getRawUserInfo).map(info => s"$info@").getOrElse("")
    val path: String = Option(uri.getRawPath).getOrElse("")
    val query: String = Option(uri.getRawQuery).map(q => s"?$q").getOrElse("")
    s"$scheme://$user$host$port$path$query"

object ExternalLinks:
  // A reader opening the link in a browser. Java's default agent is blocked by hosts that still serve browsers.
  private val userAgent: String =
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) " +
      "Chrome/131.0.0.0 Safari/537.36"

  private val connectTimeout: Duration = Duration.ofSeconds(5)
  private val requestTimeout: Duration = Duration.ofSeconds(10)

  // Some hosts reject HEAD and still serve GET.
  private def retryWithGet(status: Int): Boolean = status == 403 || status == 405 || status == 501
