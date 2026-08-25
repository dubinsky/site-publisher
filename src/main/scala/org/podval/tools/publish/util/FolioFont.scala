package org.podval.tools.publish.util

import scala.util.control.NonFatal
import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.concurrent.ConcurrentHashMap

/** TTF/OTF bytes for folio stamping. Chromium print embeds Google Fonts WOFF2 as Type3
  * (Skia cannot dump WOFF2/variable/CFF into a PDF Type0 font); there is no flag to
  * change that, and the browser cache is those WOFF2 files. The CSS API serves a
  * TrueType file of the same family when asked with a `wget` user-agent — the same
  * host Chromium already used for the page, so CI needs no system fonts. */
object FolioFont:
  private val cache: ConcurrentHashMap[(String, Int, Boolean), Option[Array[Byte]]] =
    ConcurrentHashMap()

  private lazy val http: HttpClient = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(5))
    .build()

  def googleTtf(family: String, fontWeight: Int, italic: Boolean): Option[Array[Byte]] =
    val key: (String, Int, Boolean) = (family, fontWeight, italic)
    cache.computeIfAbsent(key, _ => downloadGoogleTtf(family, fontWeight, italic))

  def isSfnt(bytes: Array[Byte]): Boolean =
    bytes.length >= 4 && (
      (bytes(0) == 0 && bytes(1) == 1 && bytes(2) == 0 && bytes(3) == 0) ||
      (bytes(0) == 'O' && bytes(1) == 'T' && bytes(2) == 'T' && bytes(3) == 'O') ||
      (bytes(0) == 't' && bytes(1) == 'r' && bytes(2) == 'u' && bytes(3) == 'e')
    )

  private def downloadGoogleTtf(family: String, fontWeight: Int, italic: Boolean): Option[Array[Byte]] =
    val weight: Int =
      if fontWeight >= 600 then 700
      else if fontWeight >= 500 then 500
      else 400
    val spec: String = s"${URLEncoder.encode(family, StandardCharsets.UTF_8)}:$weight${if italic then "italic" else ""}"
    val cssUrl: String = s"https://fonts.googleapis.com/css?family=$spec"
    get(cssUrl, "text/css").flatMap: css =>
      fontUrl(String(css, StandardCharsets.UTF_8)).flatMap: url =>
        get(url, "font/").filter(isSfnt)

  private val srcUrl =
    """url\(\s*['"]?([^'")\s]+)['"]?\s*\)""".r

  private def fontUrl(css: String): Option[String] =
    srcUrl.findAllMatchIn(css).map(_.group(1)).find: url =>
      val lower: String = url.toLowerCase
      lower.contains(".ttf") || lower.contains(".otf") || css.contains("format('truetype')") ||
        css.contains("format(\"truetype\")") || css.contains("format('opentype')")

  private def get(url: String, accept: String): Option[Array[Byte]] =
    try
      val request: HttpRequest = HttpRequest.newBuilder(URI.create(url))
        .timeout(Duration.ofSeconds(15))
        .header("User-Agent", "wget")
        .header("Accept", accept)
        .GET()
        .build()
      val response: HttpResponse[Array[Byte]] = http.send(request, HttpResponse.BodyHandlers.ofByteArray())
      val body: Array[Byte] = response.body
      Option.when(response.statusCode == 200 && body != null && body.length >= 16 && body.length <= 8_000_000)(body)
    catch case NonFatal(_) => None
