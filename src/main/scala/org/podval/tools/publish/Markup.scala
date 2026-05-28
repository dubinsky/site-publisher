package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, XmlAst}
import zio.blocks.chunk.Chunk
import java.net.{URI, URISyntaxException}

object Markup:
  val all: List[Markup] = List(
    Markdown,
    HtmlLike.Html,
    Tei
  )

  private object InternalLinkClass extends Xml.ClassName("internal-link")
  object TranscludeClass extends Xml.ClassName("transclude")

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(content: String, errorReporter: PageError.Reporter): Xml.Element

  // TODO XmlWriter should stop at the same elements!
  protected def stop(xml: XmlAst)(element: xml.Element): Boolean

  protected def recognizeWikiLinks: Boolean

  protected def recognizeBlocks: Boolean

  protected def isSectionElement(element: Xml.Element): Boolean

  protected def sectionTitle(element: Xml.Element): Option[String]

  protected def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section]

  protected def linkKind(element: Xml.Element): Option[Link.Kind]

  protected def toHtml(element: Xml.Element): Xml.Element

  final def parseAndPreProcess(
    content: String,
    errorReporter: PageError.Reporter,
    siteUrl: String
  ): Xml.Element =
    val xmlRaw: Xml.Element = parse(content, errorReporter)
    val idGenerator: IdGenerator = IdGenerator()

    Xml.transform(xmlRaw, stop(Xml), element =>
      var result: Xml.Element = toHtml(element)

      // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
      // but I do it manually and uniformly for HTML, TEI etc.
      if isSectionElement(result) && Xml.Id.get(result).isEmpty then
        result = Xml.Id.set(result, sectionTitle(result).fold(idGenerator.generate())(Xml.Id.toId))

      if recognizeBlocks then
        result = Markdown.setBlockId(result, errorReporter)

      if recognizeWikiLinks && !Xml.A.is(result) then
        result = Xml.setChildren(result, Xml.children(result).flatMap(xml =>
          Xml.asText(xml).fold(Seq(xml))(Markdown.convertWikiLinks(Seq.empty, _))
        ))

      if Xml.A.is(result) then
        if Xml.Id.get(result).isEmpty then
          result = Xml.Id.set(result, idGenerator.generate())

        Xml.Href.get(result).foreach: href =>
          // TODO verify that external link is not broken if the Site is so configured
          val isInternal: Boolean =
            try
              val uri: URI = URI(href)
              if uri.getScheme != null && uri.getHost == siteUrl
              then errorReporter.error(PageError.SelfLink, href, None)
              uri.getScheme == null
            catch case e: URISyntaxException => true
          if isInternal then
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
        if !Markdown.WikiBlockClass.has(element) then None else Xml.Id.get(element) match
          case None => errorReporter.error(PageError.NoId, s"Defect: No id on block $element", None)
          case Some(id) => Some(Fragment.Block(id))
      )
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

      if Xml.A.is(result) then Xml.Href.get(result).foreach: href =>
        if Markup.InternalLinkClass.has(result) then
          result = resolveInternalLinks(result, href, page, errorReporter)

        if Markup.TranscludeClass.has(result) then
          result = Markdown.embed(result, href)

      result
    )

    // Convert to HTML and add TOC
    Html.transform(Html.fromXml(xmlResult), stop(Html), element =>
      if !Toc.isKramdownTocMarker(element) then element else toc.html
    )
  
  private def resolveInternalLinks(
    element: Xml.Element,
    ref: String,
    page: Page,
    errorReporter: PageError.Reporter
  ): Xml.Element =
    val kind: Option[Link.Kind] = linkKind(element)
    Link.resolve(ref, kind, page) match
      case None =>
        errorReporter.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element", element)
        val result = Xml.ClassName.add(element, "unresolved-link")
        result
      case Some(linkTo) =>
        // TODO transclude
        var result: Xml.Element = Xml.Href.set(element, linkTo.url)

        def linkText(text: String): String =
          if Markdown.WikiLinkClass.has(element)
          then Markdown.WikiLink.text(Markup.TranscludeClass.has(element), text)
          else text

        if Xml.toString(result) == linkText(ref) then
          result = Xml.setText(result, linkText(linkTo.title))

        result

  final def backLinks(
    xml: Xml.Element,
    page: MarkupPage
  ): Seq[BackLinks.BackLink] = Xml.gatherWithParents(
    element = xml,
    stop = stop(Xml),
    gatherElement = backLink(_, _, page)
  )

  // Note: Obsidian expands the context to the source level, which is good for searching - but doesn't look great
  // when there are non-wiki links in there;
  // I am going with just text, so the non-wiki links are not going to be visible...
  // Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
  private def backLink(element: Xml.Element, parents: Seq[Xml.Element], from: MarkupPage): Option[BackLinks.BackLink] =
    if !Xml.A.is(element) || !Markup.InternalLinkClass.has(element) then None else
      for
        ref <- Xml.Href.get(element)
        to <- Link.resolve(ref, kind = None, from)
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
          kind = linkKind(element),
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
