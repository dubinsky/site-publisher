package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, HtmlClass, HtmlXmlDialect, Xml, Xml2Html, XmlAst, XmlAttribute, XmlElement}
import zio.blocks.chunk.Chunk
import java.net.{URI, URISyntaxException}

object Markup:
  // TODO disambiguate XML: TEI, Entity, Store...
  def of(sourcePath: Path): Option[Markup] = sourcePath.extension.flatMap: extension =>
    all.find(_.isExtension(extension))

  private val all: List[Markup] = List(
    MarkdownMarkup,
    HtmlMarkup,
    Tei
  )

  private object InternalLinkClass extends HtmlClass("internal-link")
  object TranscludeClass extends HtmlClass("transclude")

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(content: String, errorReporter: PageError.Reporter): Xml.Element

  final protected def transform[Element: XmlAst](
    element: Element,
    transformElement: Element => Element
  ): Element = HtmlXmlDialect.transform(
    element,
    transformElement
  )

  protected def recognizeMarkdownWikiLinks: Boolean

  protected def recognizeMarkdownFootnotes: Boolean

  protected def recognizeMarkdownBlocks: Boolean

  protected def isSectionElement(element: Xml.Element): Boolean

  protected def sectionTitle(element: Xml.Element): Option[String]

  protected def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section]

  protected def linkKind(element: Xml.Element): Option[Link.Kind]

  protected def toHtml(element: Xml.Element): Xml.Element

  protected def setFootnoteCorrelationIds(element: Xml.Element): Xml.Element

  protected def isFootnotesContainer(element: Xml.Element): Boolean

  final def parseAndPreProcess(
    content: String,
    errorReporter: PageError.Reporter,
    siteUrl: String
  ): Xml.Element =
    val ids: IdGenerator = IdGenerator("_generated_id")

    var xml: Xml.Element = parse(content, errorReporter)

    xml = transform(xml, element =>
      var result: Xml.Element = toHtml(element)

      // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
      // but I do it manually and uniformly for HTML, TEI etc.
      if isSectionElement(result) && result.get(XmlAttribute.Id).isEmpty then
        result = result.set(XmlAttribute.Id, sectionTitle(result).fold(ids.generate())(XmlAttribute.Id.toId))

      if recognizeMarkdownBlocks then
        result = MarkdownMarkup.setBlockId(result, errorReporter)

      if !result.isElement(HtmlXmlDialect.A) then
        if recognizeMarkdownWikiLinks then
          result = convertText(result, MarkdownMarkup.convertWikiLinks)
        if recognizeMarkdownFootnotes then
          result = convertText(result, MarkdownMarkup.convertFootnotes)

      if result.isElement(HtmlXmlDialect.A) then
        if result.get(XmlAttribute.Id).isEmpty then
          result = result.set(XmlAttribute.Id, ids.generate())

        result.get(HtmlXmlDialect.Href).foreach: href =>
          // TODO verify that external link is not broken if the Site is so configured
          val isInternal: Boolean =
            try
              val uri: URI = URI(href)
              if uri.getScheme != null && uri.getHost == siteUrl
              then errorReporter.error(PageError.SelfLink, href, None)
              uri.getScheme == null
            catch case e: URISyntaxException => true
          if isInternal then
            result = result.add(Markup.InternalLinkClass)

      result
    )

    // Footnotes

    // Set footnote correlation ids
    xml = setFootnoteCorrelationIds(xml)

    // Retrieve footnote bodies
    val footnoteBodies: Map[String, Chunk[Xml.Node]] = HtmlXmlDialect.gather(xml, element =>
      if !element.has(Footnotes.BodyClass) then None else
        element.get(Footnotes.CorrelationId).map(_ -> element.getChildren)
    ).toMap

    // Replace footnotes with link stubs
    xml = transform(xml, element =>
      element.get(Footnotes.CorrelationId).fold(element)(Footnotes.linkStub)
    )

    // Remove body stubs
    xml = transform(xml, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          child.has(Footnotes.BodyClass) ||
          // FlexMark FootnotesExtension footnotes 'div'
          child.getName == "div" && child.has(HtmlClass("footnotes"))
        ))
      )
    )

    // Number the footnotes
    val footnoteNumbers: IdGenerator = IdGenerator("")
    var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

    xml = transform(xml, element => element.get(Footnotes.CorrelationId).fold(element): correlationId =>
      val footnoteNumber: String = footnoteNumbers.generate()
      // TODO error when not found:
      footnoteBodies.get(correlationId).foreach: footnoteBody =>
        footnotesToAdd = footnotesToAdd.appended(Footnotes.body(footnoteNumber, footnoteBody))
      Footnotes.link(footnoteNumber)
    )

    // Add footnotes 'div'
    if footnotesToAdd.nonEmpty then
      var footnotesAdded: Boolean = false
      xml = transform(xml, element =>
        if footnotesAdded || !isFootnotesContainer(element) then element else
          footnotesAdded = true
          val footnotesDiv: Xml.Element = Xml
            .element(XmlElement("div"))
            .add(HtmlClass("footnotes"))
            .setChildren(footnotesToAdd)
          element.setChildren(element.getChildren :+ footnotesDiv)
      )

    xml

  private def convertText(
    element: Xml.Element,
    converter: String => Seq[Xml.Node]
  ): Xml.Element =
    element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(converter)))

  final def toc(
    xml: Xml.Element,
    errorReporter: PageError.Reporter
  ): Toc = Toc(
    sections = sections(xml, errorReporter),
    ids = HtmlXmlDialect.gather(xml, _.get(XmlAttribute.Id)),
    blocks = if !recognizeMarkdownBlocks then Seq.empty else
      HtmlXmlDialect.gather(xml, element =>
        if !element.has(MarkdownMarkup.WikiBlockClass) then None else element.get(XmlAttribute.Id) match
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
    val xmlResult: Xml.Element = transform(xml, element =>
      var result: Xml.Element = element

      if result.isElement(HtmlXmlDialect.A) then result.get(HtmlXmlDialect.Href).foreach: href =>
        if result.has(Markup.InternalLinkClass) then
          result = resolveInternalLinks(result, href, page, errorReporter)

        if result.has(Markup.TranscludeClass) then
          MarkdownMarkup.embed(result, href).foreach: embedded =>
            result = embedded

      result
    )

    // Convert to HTML and add TOC
    transform(Xml2Html.fromXml(xmlResult), element =>
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
        element.add(HtmlClass("unresolved-link"))
      case Some(linkTo) =>
        // TODO transclude
        var result: Xml.Element = element.set(HtmlXmlDialect.Href, linkTo.url)

        def linkText(text: String): String =
          if element.has(MarkdownMarkup.WikiLinkClass)
          then MarkdownMarkup.WikiLink.text(element.has(Markup.TranscludeClass), text)
          else text

        if result.getText == linkText(ref) then
          result = result.setText(linkText(linkTo.title))

        result

  final def backLinks(
    xml: Xml.Element,
    page: MarkupPage
  ): Seq[BackLinks.BackLink] = HtmlXmlDialect.gatherWithParents(
    element = xml,
    gatherElement = backLink(_, _, page)
  )

  // Note: Obsidian expands the context to the source level, which is good for searching - but doesn't look great
  // when there are non-wiki links in there;
  // I am going with just text, so the non-wiki links are not going to be visible...
  // Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
  private def backLink(element: Xml.Element, parents: Seq[Xml.Element], from: MarkupPage): Option[BackLinks.BackLink] =
    if !element.isElement(HtmlXmlDialect.A) || !element.has(Markup.InternalLinkClass) then None else
      for
        ref <- element.get(HtmlXmlDialect.Href)
        to <- Link.resolve(ref, kind = None, from)
        id <- element.get(XmlAttribute.Id)
      yield
        val toId: Option[Link.ToId] = from.source.flatMap(_.cached.toc.resolveId(id))
        val toFrom: Link = Link(from, fragment = toId, intrapage = false)
        val parent: Xml.Element = parents.head
        // TODO go back to `ne`
        val (before: Xml.Nodes, tail) = parent.getChildren.span(
          _.asElement.fold(true)(element => !element.get(HtmlXmlDialect.Href).contains(ref))
        )
        val it: Xml.Element = tail.head.asElement.get
        val after: Xml.Nodes = tail.tail

        BackLinks.BackLink(
          to = to,
          from = from,
          transclude = element.has(Markup.TranscludeClass),
          kind = linkKind(element),
          context = BackLinks.Context(
            url = toFrom.url,
            before = shortenContext(isBefore  = true, Xml.toString(before)),
            element = it.getText,
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
