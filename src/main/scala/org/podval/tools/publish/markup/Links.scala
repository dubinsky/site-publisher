package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter, Site}
import org.podval.tools.publish.page.OriginalMarkupPage
import org.podval.tools.publish.util.{Files, IdGenerator, Media, Strings}
import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml}
import zio.blocks.chunk.Chunk
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec

// for wiki links syntax, see https://obsidian.md/help/links
object Links:
  private object WikiLinkClass extends HtmlClass("wiki-link")

  private object TranscludeClass extends HtmlClass("transclude")

  object InternalLinkClass extends HtmlClass("internal-link")

  // TODO remove; add/check classes directly
  def isTranscluded(element: Xml.Element): Boolean = element.has(TranscludeClass)

  def wikiLink(
    transclude: Boolean,
    ref: String,
    title: Option[String]
  ): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(WikiLinkClass)
    .add(Option.when(transclude)(TranscludeClass))
    .set(HtmlAttribute.Href, Option.when(ref.nonEmpty)(ref))
    .setText(Wiki.wikiLinkText(transclude, title.getOrElse(ref)))

  private object Wiki:
    val startTransclusion: String = "![["
    val startLink: String = "[["
    val end: String = "]]"

    def wikiLinkStart(transclude: Boolean): String = if transclude then startTransclusion else startLink

    def wikiLinkText(transclude: Boolean, text: String) = s"${wikiLinkStart(transclude)}$text$end"

  def linkText(element: Xml.Element, text: String): String =
    if element.has(WikiLinkClass)
    then Wiki.wikiLinkText(isTranscluded(element), text)
    else text

  def convertWikiLinks(element: Xml.Element): Option[Xml.Element] =
    Option.when(!element.isA)(
      // TODO move to XmlUtils
      element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(convertWikiLinks(Chunk.empty, _))))
    )

  @tailrec
  private def convertWikiLinks(result: Chunk[Xml.Node], text: String): Xml.Nodes =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(Wiki.startTransclusion)
      val startLink: Int = text.indexOf(Wiki.startLink)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(Wiki.end, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + Wiki.wikiLinkStart(transclude).length, end).trim
        val after: String = text.substring(end + Wiki.end.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)
  
        val wikiLink: Xml.Element = Links.wikiLink(
          transclude,
          ref,
          title
        )
        
        convertWikiLinks(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(wikiLink),
          after
        )

  def setAnchorId(element: Xml.Element, ids: IdGenerator): Option[Xml.Element] =
    Option.when(element.isA && element.getId.isEmpty)(
      element.setId(ids.generate())
    )
    
  def convertInternalLink(
    element: Xml.Element,
    site: Site,
    errorReporter: PageErrorReporter
  ): Option[Xml.Element] =
    if !element.isA then None else
      element.getHref.flatMap: href =>
        // TODO verify that external link is not broken if the Site is so configured
        val isInternal: Boolean =
          try
            val uri: URI = URI(href)
            if site.isSelf(uri) then errorReporter.error(PageError.SelfLink, href)
            uri.getScheme == null
          catch case e: URISyntaxException => true

        Option.when(isInternal)(
          element.add(Links.InternalLinkClass)
        )

  def resolveInternalLinks(
    element: Xml.Element,
    page: OriginalMarkupPage,
    errorReporter: PageErrorReporter
  ): Option[Xml.Element] =
    Option.when(element.isA && element.has(Links.InternalLinkClass))(
      element.getHref.fold(element)(ref => Links.resolveInternalLinks(element, ref, page, errorReporter))
    )
    
  private def resolveInternalLinks(
    element: Xml.Element,
    ref: String,
    page: OriginalMarkupPage,
    errorReporter: PageErrorReporter
  ): Xml.Element =
    val kind: Option[LinkKind] = LinkKind.of(element)
    Link.resolve(ref, kind, page) match
      case None =>
        errorReporter.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element")
        element.addClass("unresolved-link") // TODO move into Links
      case Some(linkTo) =>
        // TODO transclude
        val result: Xml.Element = element.setHref(linkTo.url)

        if result.getText != Links.linkText(element, ref)
        then result
        else result.setText(Links.linkText(element, linkTo.title))

  def embedWikiLink(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.isA && isTranscluded(element))(
      element.getHref.fold(element)(embedWikiLink(element, _).getOrElse(element))
    )

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embedWikiLink(element: Xml.Element, ref: String): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.fold(None): extension =>
      if Media.isImage(extension) then
        val (width: Option[Int], height: Option[Int]) =
          // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
          (None, None)

        Some(Xml
          .element("img")
          .set("src", ref)
          .set("alt", s"Image: $ref")
          .set("width", width.map(_.toString))
          .set("height", height.map(_.toString))
        )
      else if Media.isAudio(extension) then Some(Xml
        .element("audio")
        .set("src", ref)
        .set("controls", true.toString)
      )
      else if extension == "pdf" then
        // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
        None
      else
        None
