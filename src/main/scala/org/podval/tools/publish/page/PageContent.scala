package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{AssetRef, Bibliography, BibliographyItem, Citation, EntityLists, Footnote,
  Glossary, Ids, Link, LinkKind, Section, StoreIndex, Tip, Toc, WikiBlocks, WikiLink}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.PageError
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html}
import java.io.File

/** Prepared once per document (`PageContent.apply`); resolved per chunk in `markupContent`. */
object PageContent:
  def apply(
    source: PageSource,
    frontMatter: FrontMatter,
    xml: Xml.Element
  ): PageContent =
    // Run markup-specific processors and extract title
    val (xmlProcessed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)

    (frontMatter.title, title) match
      case (Some(frontMatterTitle), Some(contentTitle))
        if frontMatterTitle.trim != contentTitle.getText.trim =>
        source.error(
          PageError.AmbiguousTitle,
          s"title from front matter ('$frontMatterTitle') differs from content title ('${contentTitle.getText}')"
        )
      case _ =>

    // Prepare to calculate Toc and backlinks.
    // Footnote *links* stay in the tree; bodies are harvested then stripped.
    val ids: IdGenerator = IdGenerator("_generated_id")
    val prepared: Xml.Element = xmlProcessed.transform(prepareElement(_, source, ids))
    val (footnotes: Map[String, Footnote], result: Xml.Element) =
      try Footnote.harvest(prepared)
      catch case e: IllegalStateException =>
        throw IllegalStateException(s"${source.sourcePath}: ${e.getMessage}", e)

    new PageContent(
      source = source,
      frontMatter = frontMatter,
      title = title,
      xml = result,
      toc = Toc(result, source),
      ids = Ids(result),
      blocks = WikiBlocks(result, source),
      footnotes = footnotes,
      glossaryDefinitions = Glossary.definitions(result),
      bibliographyDefinitions = BibliographyItem.definitions(result),
      storeIndex = source.markup.storeIndex(xml),
      entityListsIndex = source.markup.entityListsIndex(xml)
    )

  /** Section ids (before TOC), bare-anchor ids and internal-link marks (before backlinks), wiki embed. */
  private def prepareElement(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator
  ): Xml.Element =
    // This has to happen before calculating Toc.
    var result: Xml.Element = Section.normalize(element, ids)

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

final class PageContent private(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  val xml: Xml.Element,
  val toc: Toc,
  val ids: Ids,
  val blocks: WikiBlocks,
  val footnotes: Map[String, Footnote],
  val glossaryDefinitions: Map[String, Xml.Nodes],
  val bibliographyDefinitions: Map[String, Xml.Nodes],
  val storeIndex: Option[StoreIndex],
  val entityListsIndex: Option[EntityLists.Index]
):
  private val tips: Seq[Tip] = Seq(Glossary.tip, Footnote.tip, BibliographyItem.tip)

  def markupContent(
    sectionId: Option[String],
    isTerminal: Boolean
  ): Html.Element =
    val isChunked: Boolean = sectionId.isDefined || !isTerminal

    // Select XML to include
    val selected: Xml.Element = toc.select(
      xml = xml,
      sectionId = sectionId,
      isTerminal = isTerminal
    )

    // Add bodies of the footnotes referenced in the selected XML
    val withFootnotes: Xml.Element = Footnote.appendReferenced(selected, footnotes)

    // Resolve citations
    val (withCitations: Xml.Element, unknownCitations: Seq[String]) = bibliography.resolve(withFootnotes)
    // Report unknown citations on the full page only, not for chunks
    // (the cited keys may live on another chunk).
    if !isChunked then unknownCitations.foreach: label =>
      source.error(PageError.UnknownCitation, s"unknown citation '$label'")

    // After backlink harvest (Site.load walks PageContent.xml). Index → entity hrefs are display-only.
    val withLists: Xml.Element = entityListsIndex.fold(withCitations)(index =>
      EntityLists.fill(withCitations, source.page, index)
    )

    val withLinks: Xml.Element = resolveLinks(withLists, isChunked, attachTips = true)

    // Convert to HTML
    insertToc(Xml2Html.fromXml(withLinks), sectionId, isChunked)

  private lazy val bibliography: Bibliography =
    val sourceFile: File = source.page.site.sourceFile(source.sourcePath)
    Bibliography.load(
      documentDirectory = sourceFile.getParentFile,
      bibliography = frontMatter.bibliography,
      csl = frontMatter.csl,
      lang = frontMatter.lang.orElse(source.page.site.config.lang)
    )

  private def insertToc(
    html: Html.Element,
    sectionId: Option[String],
    isChunked: Boolean
  ): Html.Element =
    var tocAdded: Boolean = false
    val fullPage: Option[FullMarkupPage] = source.page.asFullMarkupPage
    def tocHtml: Html.Element = toc.html(
      sectionId = sectionId,
      tocDepth = fullPage.map(_.tocDepth).getOrElse(2),
      chunkDepth = Option.when(isChunked)(fullPage.map(_.chunkDepth).getOrElse(2))
    )
    val withPlaceholder: Html.Element = Html.transform(html)(element =>
      if tocAdded || !element.has(Toc.PlaceholderClass) then element else
        tocAdded = true
        tocHtml
    )
    if fullPage.exists(_.hasToc) && !tocAdded
    then withPlaceholder.setChildren(tocHtml +: withPlaceholder.getChildren)
    else withPlaceholder

  // Note: resolveLinks used to be a transform; this is after Grok did glossary tooltips.
  // attachTip wraps the <a> and tip as siblings in span.{prefix}-ref;
  // recurse only into the tip (attachTips = false) so definition links resolve
  // without nested tips.
  private def resolveLinks(
    element: Xml.Element,
    isChunked: Boolean,
    attachTips: Boolean
  ): Xml.Element =
    var result: Xml.Element = element

    // Resolve internal links, including the ones in footnote bodies
    if Link.isInternal(result) then result.getHref.foreach: ref =>
      result = resolveInternalLink(result, ref, isChunked, attachTips)

    result = AssetRef.resolve(
      result,
      source.page,
      source,
      reportMissing = !isChunked
    )

    // Turn footnote links into footnote references
    result = Footnote.resolveLink(result, footnotes, attachTips)

    val isRef: Boolean = tips.exists(_.isRef(result))
    result.setChildren(result.getChildren.map(child =>
      child.asElement.fold(child): child =>
        // Do not re-resolve the inner <a> of a ref wrapper; do walk the tip
        // with attachTips = false (links in definitions, no nested tips).
        if isRef && !tips.exists(_.isTip(child))
        then child
        else resolveLinks(child, isChunked, attachTips && !tips.exists(_.isTip(child)))
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
          source.page.asFullMarkupPage match
            case None => linkTo.url
            case Some(page) =>
              val id: String = linkTo.fragment.get.id
              val sectionId: Option[String] = ids.sectionById(id)
              s"${toc.chunkName(sectionId, page.chunkDepth)}#$id"

        var result: Xml.Element = element.setHref(href)
        if result.getText == WikiLink.linkText(element, ref) then
          result = result.setText(WikiLink.linkText(element, linkTo.title))
        if attachTips then
          glossaryTip(linkTo) match
            case Some(definition) =>
              result = Glossary.tip.attachTip(result, definition)
            case None =>
              bibliographyTip(linkTo).foreach: definition =>
                result = result.add(Citation.CiteClass)
                result = BibliographyItem.tip.attachTip(result, definition)
        result

  private def glossaryTip(linkTo: Link): Option[Xml.Nodes] =
    definitionFrom(linkTo, _.glossaryDefinitions)

  private def bibliographyTip(linkTo: Link): Option[Xml.Nodes] =
    definitionFrom(linkTo, _.bibliographyDefinitions)

  private def definitionFrom(
    linkTo: Link,
    defs: PageContent => Map[String, Xml.Nodes]
  ): Option[Xml.Nodes] =
    linkTo.fragment.flatMap: fragment =>
      val from: Option[PageContent] = if linkTo.isIntrapage then Some(this) else linkTo.page.content
      from.flatMap(page => defs(page).get(fragment.id))
