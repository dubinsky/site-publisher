package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{Bibliography, Footnote, Glossary, Ids, Link, LinkKind, Section, Tip, Toc, WikiBlocks, WikiLink}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.PageError
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html, XmlDialect, XmlUtil}
import zio.blocks.chunk.Chunk

final class PageContent private(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  val xml: Xml.Element,
  val toc: Toc,
  val ids: Ids,
  val blocks: WikiBlocks,
  val footnotes: Map[String, Footnote],
  val glossaryDefinitions: Map[String, Xml.Nodes]
):
  private val tips: Seq[Tip] = Seq(Glossary.tip, Footnote.tip)

  def markupContent(
    sectionId: Option[String],
    isTerminal: Boolean
  ): Html.Element =
    val isChunked: Boolean = sectionId.isDefined || !isTerminal

    val xmlDialect: XmlDialect = source.markup.xmlDialect

    var result: Xml.Element = xml

    // Select XML to include
    result = toc.select(
      xml = result,
      sectionId = sectionId,
      isTerminal = isTerminal,
      xmlDialect = xmlDialect
    )

    // Add bodies of the footnotes referenced in the selected XML
    // TODO footnotes placed at the end of elements like table, not the overall end?
    // TODO how do multi-level footnotes look?
    val toAdd: Chunk[Footnote] = PageContent.footnoteLinks(result, xmlDialect).map(footnotes)

    if toAdd.nonEmpty then
      val footnotesDiv: Xml.Element = Xml
        .element("div")
        .addClass("footnotes")
        .setChildren(toAdd.map(_.body))
      result = result.setChildren(result.getChildren :+ footnotesDiv)

    result = resolveCitations(result, isChunked)

    // resolveLinks used to be a transform;
    // this is after Grok did glossary tooltips... can they be merged?
    result = resolveLinks(result, isChunked, attachTips = true)
    result = xmlDialect.transform(result, element =>
      element.setChildren(XmlUtil.convertElements(element.getChildren,
        element => tips.map(_.wrapRef(element)).collectFirst { case Some(nodes) => nodes }
      ))
    )

    // Convert to HTML
    var html: Html.Element = Xml2Html.fromXml(result)

    // Add TOC to HTML
    var tocAdded: Boolean = false

    def tocHtml: Html.Element = toc.html(
      sectionId = sectionId,
      tocDepth = source.page.tocDepth,
      chunkDepth = Option.when(isChunked)(source.page.chunkDepth)
    )

    html = xmlDialect.transform(html, element =>
      if tocAdded || !source.markup.isTocPlaceholder(element)
      then
        element
      else
        tocAdded = true
        tocHtml
    )

    if source.page.hasToc && !tocAdded then
      html = html.setChildren(tocHtml +: html.getChildren)

    html
  
  private lazy val bibliography: Bibliography =
    val sourceFile: java.io.File = source.page.site.sourceFile(source.sourcePath)
    Bibliography.load(
      documentDirectory = sourceFile.getParentFile,
      bibliography = frontMatter.bibliography,
      csl = frontMatter.csl,
      lang = frontMatter.lang.orElse(source.page.site.config.lang)
    )

  private def resolveCitations(xml: Xml.Element, isChunked: Boolean): Xml.Element =
    val (result: Xml.Element, unknown: Seq[String]) =
      bibliography.resolve(xml, source.markup.xmlDialect)
    PageContent.unknownCitationMessages(unknown, isChunked).foreach: message =>
      source.error(PageError.UnknownCitation, message)
    result

  private def resolveLinks(
    element: Xml.Element,
    isChunked: Boolean,
    attachTips: Boolean
  ): Xml.Element =
    var result: Xml.Element = element

    // Resolve internal links, including the ones in footnote bodies
    if Link.isInternal(result) then result.getHref.foreach: ref =>
      result = resolveInternalLink(result, ref, isChunked, attachTips)

    // Turn footnote links into footnote references
    if Footnote.isLink(result) then
      val footnote: Footnote = footnotes(Footnote.getCorrelationId(result))
      result = footnote.link
      if attachTips then
        val content: Xml.Nodes = footnote.nodes.filterNot(_.isWhitespace)
        if content.nonEmpty then
          result = Footnote.tip.attachTip(result, content)

    result.setChildren(result.getChildren.map(child =>
      child.asElement.fold(child): child =>
        resolveLinks(child, isChunked, attachTips && !tips.exists(_.isTip(child)))
    ))
  
  private def resolveInternalLink(
    element: Xml.Element,
    ref: String,
    isChunked: Boolean,
    attachTips: Boolean
  ): Xml.Element =
    val kind: Option[LinkKind] = LinkKind.of(element)
    Link.resolve(ref, kind, source.page) match
      case None =>
        // Report error for the full page only, not for chunks.
        if !isChunked then
          source.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element")
        element.add(Link.UnresolvedLinkClass)
      case Some(linkTo) =>
        // TODO transclude

        // TODO do the same with section links in Toc - and move this there?
        val href: String = if !isChunked || !linkTo.isIntrapage || linkTo.fragment.isEmpty then linkTo.url else
          val id: String = linkTo.fragment.get.id
          val sectionId: Option[String] = ids.sectionById(id)
          s"${toc.chunkName(sectionId, source.page.chunkDepth)}#$id"

        var result: Xml.Element = element.setHref(href)

        if result.getText == WikiLink.linkText(element, ref) then
          result = result.setText(WikiLink.linkText(element, linkTo.title))

        if attachTips then glossaryTip(linkTo).foreach: definition =>
          result = Glossary.tip.attachTip(result, definition)

        result

  private def glossaryTip(linkTo: Link): Option[Xml.Nodes] =
    linkTo.fragment.flatMap: fragment =>
      val from: Option[PageContent] = if linkTo.isIntrapage then Some(this) else linkTo.page.content
      from.flatMap(_.glossaryDefinitions.get(fragment.id))


object PageContent:
  /** Unknown-citation page errors; chunks skip them because the cited keys may live on another chunk. */
  private[publish] def unknownCitationMessages(labels: Seq[String], isChunked: Boolean): Seq[String] =
    if isChunked then Seq.empty
    else labels.map(label => s"unknown citation '$label'")

  private def footnoteLinks(xml: Xml.Element, xmlDialect: XmlDialect): Chunk[String] =
    xmlDialect.gather(xml, element =>
      Option.when(Footnote.isLink(element))(Footnote.getCorrelationId(element))
    )

  def apply(
    source: PageSource,
    frontMatter: FrontMatter,
    xml: Xml.Element
  ): PageContent =
    val xmlDialect: XmlDialect = source.markup.xmlDialect
    
    // Run markup-specific processors and extract title
    val (xmlProcessed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(source, xml)

    // TODO error if both front matter and content titles are present and are different.

    // Prepare to calculate Toc and backlinks.
    // Footnotes are left in the XML for PageContentResolved to resolve them;
    // they are not affected by transformations here
    // since they are neither sections nor links at this point.
    val ids: IdGenerator = IdGenerator("_generated_id")
    var result: Xml.Element = xmlDialect.transform(xmlProcessed, element =>
      // This has to happen before calculating Toc.
      var result: Xml.Element = Section.normalize(element, source.markup, ids)

      // This has to happen before calculating backlinks.
      if result.isA && result.getId.isEmpty && !Section.isPermalink(result) then
        result = result.setId(ids.generate())

      // This has to happen before calculating backlinks.
      if result.isA && !Section.isPermalink(result) then result.getHref.foreach: href =>
        if source.page.site.isInternalLink(href, source) then
          result = result.add(Link.InternalLinkClass)

      // Embed
      if result.isA && WikiLink.isTranscluded(result) then result.getHref.foreach: href =>
        WikiLink.embed(result, href).foreach: embedded =>
          result = embedded

      result
    )

    // Process footnotes
    val footnoteNumbers: Map[String, Int] = footnoteLinks(result, xmlDialect)
      .zipWithIndexFrom(1)
      .toMap

    val footnotes: Map[String, Footnote] = xmlDialect
      .gather(result, element =>
        Option.when(Footnote.isBody(element)):
          val correlationId: String = Footnote.getCorrelationId(element)
          correlationId -> Footnote(
            correlationId = correlationId,
            number = footnoteNumbers(correlationId),
            nodes = element.getChildren
          )
      )
      .toMap

    result = source.markup.xmlDialect.transform(result, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          Footnote.isBody(child) ||
          source.markup.isSpuriousFootnotesDiv(child)
        ))
      )
    )

    new PageContent(
      source = source,
      frontMatter = frontMatter,
      title = title,
      xml = result,
      toc = Toc(result, source.markup, source),
      ids = Ids(result, xmlDialect),
      blocks = WikiBlocks(result, xmlDialect, source),
      footnotes = footnotes,
      glossaryDefinitions = Glossary.definitions(result, xmlDialect)
    )
