package org.podval.tools.publish.markup

import org.podval.tools.publish.page.Page
import org.podval.tools.publish.site.{PageError, PageErrorReporter, Path}
import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.{HtmlClass, Xml}

/** File refs on media IR (`img@src`, `video`/`audio`/`source@src`, `object@data`).
  * Not page links: no title-walk, backlinks, or tips. */
object AssetRef:
  object UnresolvedClass extends HtmlClass("unresolved-asset")

  private val WikiEmbedAttr: String = "data-wiki-embed"

  def markWikiEmbed(element: Xml.Element): Xml.Element =
    if resourceAttr(element).isDefined then element.set(WikiEmbedAttr, "true")
    else element.transform: el =>
      if resourceAttr(el).isDefined then el.set(WikiEmbedAttr, "true") else el

  def resourceAttr(element: Xml.Element): Option[String] =
    element.getName match
      case "img" | "video" | "audio" | "source" => Some("src")
      case "object" => Some("data")
      case _ => None

  def resolve(
    element: Xml.Element,
    from: Page,
    errorReporter: PageErrorReporter,
    reportMissing: Boolean
  ): Xml.Element =
    resourceAttr(element).flatMap(attr => element.get(attr).map(_.trim).filter(_.nonEmpty).map(attr -> _)) match
      case None => element
      case Some((attr, ref)) =>
        val (pathStringRaw: String, fragment: Option[String]) = Strings.splitFirst(ref, '#')
        val pathString: String = pathStringRaw.trim
        val isWiki: Boolean = element.get(WikiEmbedAttr).contains("true")
        val stripped: Xml.Element = element.set(WikiEmbedAttr, "")
        if pathString.isEmpty || !from.site.isInternalLink(pathString, errorReporter) then stripped
        else if !isAssetPath(pathString) then stripped
        else lookup(pathString, from, isWiki) match
          case None =>
            if reportMissing then
              errorReporter.error(PageError.MissingAsset, s"missing asset '$ref'")
            stripped.add(UnresolvedClass)
          case Some(page) =>
            val url: String = page.path.toString + fragment.fold("")(f => s"#$f")
            stripped.set(attr, url)

  private def isAssetPath(pathString: String): Boolean =
    Files.nameAndExtension(
      pathString.split('/').map(_.trim).filterNot(_.isEmpty).lastOption.getOrElse(pathString)
    )._2.exists(Media.isAsset)

  private def lookup(pathString: String, from: Page, isWiki: Boolean): Option[Page] =
    val path: Path =
      if isWiki && !pathString.startsWith("/") && pathString.contains("/")
      then from.path.resolveFrom("/" + pathString)
      else from.path.resolveFrom(pathString)
    from.site.pages.find(path, isAbsolute = true, kind = None).orElse:
      if !isWiki || pathString.contains('/') then None
      else
        val parsed: Path = Path.fromHref(pathString)
        from.site.pages.findByFileName(parsed.fileName, parsed.extension) match
          case Seq(one) => Some(one)
          case _ => None
