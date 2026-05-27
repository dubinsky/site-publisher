package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, IdGenerator, Media, Strings}
import org.podval.xml.{Html, Xml, XmlAst}
import zio.blocks.chunk.Chunk
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec

object Markup:
  val all: List[Markup] = List(
    Markdown,
    HtmlLike.Html
  )

  // TEI org/person/place, facsimile, etc.
  private object LinkKindClassPrefix extends Xml.ClassNamePrefix("ref-kind")

  private object InternalLinkClass extends Xml.ClassName("internal-link")
  private object WikiLinkClass extends Xml.ClassName("wiki-link")
  private object TranscludeClass extends Xml.ClassName("transclude")
  private object WikiBlockClass extends Xml.ClassName("wiki-block")

  private object WikiLink:
    val startTransclusion = "![["
    val startLink = "[["
    val end = "]]"
    def start(transclude: Boolean): String = if transclude then startTransclusion else startLink
    def text(transclude: Boolean, text: String) = s"${start(transclude)}$text$end"

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  final def parseAndPreProcess(
    content: String,
    errorReporter: PageError.Reporter,
    siteUrl: String
  ): Xml.Element =
    val xmlRaw: Xml.Element = parse(content, errorReporter)
    val idGenerator: IdGenerator = IdGenerator()
    
    Xml.transform(xmlRaw, stop(Xml), element =>
      var result: Xml.Element = element

      // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
      // but I do it manually and uniformly for HTML, TEI etc.
      if isSectionElement(result) && Xml.Id.get(result).isEmpty then
        result = Xml.Id.set(result, sectionTitle(result).fold(idGenerator.generate())(Xml.Id.toId))
      
      if recognizeBlocks then
        result = setBlockId(result, errorReporter)

      result = convertLinks(result)

      if recognizeWikiLinks && !Xml.A.is(result) then
        result = Xml.setChildren(result, Xml.children(result).flatMap(xml =>
          Xml.asText(xml).fold(Seq(xml))(convertWikiLinks(Seq.empty, _))
        ))

      if Xml.A.is(result) then
        if Xml.Id.get(result).isEmpty then
          result = Xml.Id.set(result, idGenerator.generate())

        if isInternalLink(result, errorReporter, siteUrl) then
          result = Markup.InternalLinkClass.add(result)

      result
    )

  final def toc(
    xml: Xml.Element,
    errorReporter: PageError.Reporter
  ): Toc = Toc(
    sections = sections(xml, errorReporter),
    ids = Xml.gather(xml, stop(Xml), Xml.Id.get),
    blocks = if !recognizeBlocks then Seq.empty else
      Xml.gather(xml, stop(Xml), element =>
        if !Markup.WikiBlockClass.has(element) then None else Xml.Id.get(element) match
          case None => errorReporter.error(PageError.NoId, s"Defect: No id on block $element", None)
          case Some(id) => Some(Fragment.Block(id))
      )
  )

  final def backLinks(
    xml: Xml.Element,
    page: MarkupPage
  ): Seq[BackLinks.BackLink] = Xml.gatherWithParents(
    element = xml,
    stop = stop(Xml),
    gatherElement = backLink(_, _, page)
  )

  final def htmlContent(
    xml: Xml.Element,
    toc: Toc,
    errorReporter: PageError.Reporter,
    page: MarkupPage
  ): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = Xml.transform(xml, stop(Xml), element =>
      var result: Xml.Element = element

      if Xml.A.is(result) then
        if Markup.InternalLinkClass.has(element) then
          result = resolveInternalLinks(result, page, errorReporter)

        result = embed(result)

      result
    )

    // Convert to HTML and add TOC
    Html.transform(Html.fromXml(xmlResult), stop(Html), element =>
      if !Toc.isKramdownTocMarker(element) then element else toc.html
    )

  def parse(content: String, errorReporter: PageError.Reporter): Xml.Element

  // TODO XmlWriter should stop at the same elements!
  protected def stop(xml: XmlAst)(element: xml.Element): Boolean

  protected def recognizeWikiLinks: Boolean

  protected def recognizeBlocks: Boolean

  protected def isSectionElement(element: Xml.Element): Boolean

  protected def sectionTitle(element: Xml.Element): Option[String]

  protected def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section]

  // This is where TEI link elements like `persName` get converted into HTML `a` elements
  protected def convertLinks(element: Xml.Element): Xml.Element
  
  // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
  // (e.g., a list) by putting it after the block, with empty lines before and after;
  // I'll deal with this later...
  private def setBlockId(element: Xml.Element, errorReporter: PageError.Reporter): Xml.Element =
    val children: Chunk[Xml.Xml] = Xml.children(element)
    if children.isEmpty then element else Xml.asText(children.last).fold(element): text =>
      val (before: String, id: Option[String]) = Strings.split(text, '^')
      id.fold(element): id =>
        if before.nonEmpty && !Character.isWhitespace(before.last) then element else
          val result: Xml.Element = Xml.setChildren(element,
            children.init ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq
          )
          Xml.Id.get(result) match
            case Some(idExisting) =>
              errorReporter.error(PageError.NoId, s"Block id '$id' conflicts with existing id '$idExisting'", result)
            case None => Markup.WikiBlockClass.add(Xml.Id.set(result, id))

  // see https://obsidian.md/help/links
  @tailrec
  private def convertWikiLinks(result: Seq[Xml.Xml], text: String): Seq[Xml.Xml] =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(Markup.WikiLink.startTransclusion)
      val startLink: Int = text.indexOf(Markup.WikiLink.startLink)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(Markup.WikiLink.end, start)
      if end == -1 then result ++ Seq(Xml.mkText(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + Markup.WikiLink.start(transclude).length, end).trim
        val after: String = text.substring(end + Markup.WikiLink.end.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)

        var wikiLink: Xml.Element = Xml.element(Xml.A.elementName)
        wikiLink = Markup.WikiLinkClass.add(wikiLink)
        if transclude then wikiLink = Markup.TranscludeClass.add(wikiLink)
        if ref.nonEmpty then wikiLink = Xml.Href.set(wikiLink, ref)
        wikiLink = Xml.setText(wikiLink, Markup.WikiLink.text(transclude, title.getOrElse(ref)))

        convertWikiLinks(
          result ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(wikiLink),
          after
        )

  // TODO verify that external link is not broken if the Site is so configured
  private def isInternalLink(
    element: Xml.Element,
    errorReporter: PageError.Reporter,
    siteUrl: String
  ): Boolean = Xml.Href.get(element).fold(false): ref =>
    try
      val uri: URI = URI(ref)
      if uri.getScheme != null && uri.getHost == siteUrl
      then errorReporter.error(PageError.SelfLink, ref, None)
      uri.getScheme == null
    catch case e: URISyntaxException => true

  private def resolveInternalLinks(element: Xml.Element, page: Page, errorReporter: PageError.Reporter): Xml.Element =
    Xml.Href.get(element).fold(element): ref =>
      Link.resolve(ref, page) match
        case None =>
          errorReporter.error(PageError.Unresolved, s"unresolved internal link ref='$ref': $element}'", element)
          val result = Xml.ClassName.add(element, "unresolved-link")
          result
        case Some(linkTo) =>
          val isWikiLink: Boolean = Markup.WikiLinkClass.has(element)
          val transclude: Boolean = Markup.TranscludeClass.has(element)
          // TODO transclude
          var result: Xml.Element = Xml.Href.set(element, linkTo.url)

          def linkText(text: String): String =
            if !isWikiLink then text else Markup.WikiLink.text(transclude, text)

          if Xml.toString(result) == linkText(ref) then
            result = Xml.setText(result, linkText(linkTo.title))

          result

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embed(element: Xml.Element): Xml.Element = Xml.Href.get(element).fold(element): ref =>
    if !Markup.TranscludeClass.has(element) then element else
      val embedded: Option[Xml.Element] = Files.nameAndExtension(ref)._2.fold(None): extension =>
        if Media.isImage(extension) then
          val (width: Option[Int], height: Option[Int]) =
            // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
            (None, None)

          var result: Xml.Element = Xml.element("img")
          result = Xml.setAttribute(result, "src", ref)
          result = Xml.setAttribute(result, "alt", s"Image: $ref")
          result = width.fold(result)(width => Xml.setAttribute(result, "width", width.toString))
          result = height.fold(result)(height => Xml.setAttribute(result, "height", height.toString))
          Some(result)
        else if Media.isAudio(extension) then
          var result: Xml.Element = Xml.element("audio")
          result = Xml.setAttribute(result, "src", ref)
          result = Xml.setAttribute(result, "controls", true.toString)
          Some(result)
        else if extension == "pdf" then
          // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
          None
        else
          None

      embedded.getOrElse:
        // TODO! can not transclude external links
        element

  // Note: Obsidian expands the context to the source level, which is good for searching - but doesn't look great
  // when there are non-wiki links in there;
  // I am going with just text, so the non-wiki links are not going to be visible...
  // Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
  private def backLink(element: Xml.Element, parents: Seq[Xml.Element], from: MarkupPage): Option[BackLinks.BackLink] =
    if !Xml.A.is(element) || !Markup.InternalLinkClass.has(element) then None else
      for
        ref <- Xml.Href.get(element)
        to <- Link.resolve(ref, from)
        id <- Xml.Id.get(element)
      yield
        val toId: Option[Link.ToId] = from.source.flatMap(_.cached.toc.resolveId(id))
        val toFrom: Link = Link(from, fragment = toId, intrapage = false)
        val parent: Xml.Element = parents.head
        // TODO go back to `ne`
        val (before: Chunk[Xml.Xml], tail) = Xml.children(parent).span(
          Xml.asElement(_).fold(true)(element => !Xml.Href.get(element).contains(ref))
        )
        val it: Xml.Element = Xml.asElement(tail.head).get
        val after: Chunk[Xml.Xml] = tail.tail

        BackLinks.BackLink(
          to = to,
          from = from,
          transclude = Markup.TranscludeClass.has(element),
          kind = Markup.LinkKindClassPrefix.get(element).headOption,
          context = BackLinks.Context(
            url = toFrom.url,
            before = shortenContext(isBefore  = true, Xml.toString(before)),
            element = Xml.toString(it),
            after = shortenContext(isBefore  = false, Xml.toString(after))
          )
        )
  
  private val contextLengthHalf: Int = 60

  private def shortenContext(isBefore: Boolean, string: String): String =
    if string.length <= contextLengthHalf then string else if isBefore then
      val result = string.substring(string.length - contextLengthHalf)
      val prefix = /*if result.startsWith(" ") then "" else*/ "..."
      prefix + result.trim
    else
      val result = string.substring(0, contextLengthHalf)
      val suffix = /*if result.endsWith(" ") then "" else*/ "..."
      result.trim + suffix
