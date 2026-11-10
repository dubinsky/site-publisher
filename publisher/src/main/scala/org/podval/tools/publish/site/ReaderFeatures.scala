package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{Facsimile, Transclusion}
import org.podval.tools.publish.page.{DocumentContent, FullMarkupPage, LocalFeatures, MarkupPage, NodeFeatures,
  PageContent}

private[publish] final class ReaderFeatures(
  val glossary: Boolean,
  val citation: Boolean,
  val choice: Boolean,
  val transclusion: Boolean,
  val facsimile: Boolean,
  val apparatusAdd: Boolean,
  val apparatusDel: Boolean,
  val apparatusSee: Boolean
):
  def union(other: ReaderFeatures): ReaderFeatures = ReaderFeatures(
    glossary || other.glossary,
    citation || other.citation,
    choice || other.choice,
    transclusion || other.transclusion,
    facsimile || other.facsimile,
    apparatusAdd || other.apparatusAdd,
    apparatusDel || other.apparatusDel,
    apparatusSee || other.apparatusSee
  )

  def terms: Boolean = glossary || citation
  def inText: Boolean = terms || choice
  def layout: Boolean = transclusion || facsimile
  /** Menu order. Also the data-apparatus-kinds token list. Not data-apparatus. */
  def apparatus: List[String] =
    List(
      Option.when(apparatusAdd)("add"),
      Option.when(apparatusDel)("del"),
      Option.when(apparatusSee)("see")
    ).flatten

private[publish] object ReaderFeatures:
  val empty: ReaderFeatures =
    ReaderFeatures(false, false, false, false, false, false, false, false)

  def detect(site: Site): ReaderFeatures =
    var detected: ReaderFeatures = ReaderFeatures.empty
    var transclusion: Boolean = site.transclusions.all.exists(_.toPage.isDefined)
    var facsimile: Boolean = false

    def absorb(content: PageContent): Unit =
      val local: LocalFeatures = content.localFeatures
      val page: MarkupPage = content.source.page
      // A collection document publishes the cells instead of teiHeader. Anything else keeps the header.
      val collectionDocument: Boolean = page.parent.exists(_.store.exists(_.isCollection))
      val tree: NodeFeatures =
        if collectionDocument then local.published else local.published.union(local.teiHeaderOnly)
      val nodes: NodeFeatures =
        if !collectionDocument then tree
        else content.doc match
          case document: DocumentContent => tree.union(document.headerFeatures)
          case _ => tree
      val citation: Boolean = nodes.citations.exists: candidate =>
        site.pages.resolve(candidate.href, candidate.kind, page).exists: link =>
          val hostContent: Option[PageContent] =
            if link.isIntrapage then Some(content) else link.page.content
          val gloss: Boolean = link.fragment.exists: fragment =>
            hostContent.exists(_.glossaryDefinitions.contains(fragment.id))
          val bibl: Boolean = link.fragment.exists: fragment =>
            hostContent.exists(_.bibliographyDefinitions.contains(fragment.id))
          !gloss && bibl
      detected = detected.union(ReaderFeatures(
        glossary = local.glossary,
        citation = citation,
        choice = nodes.choice,
        transclusion = false,
        facsimile = false,
        apparatusAdd = nodes.apparatusAdd,
        apparatusDel = nodes.apparatusDel,
        apparatusSee = nodes.apparatusSee
      ))

    /** `transclusionsToo` is false for full-page content and for every footnote body, and true for every
      * other markup page and for inline-list prose. Footnote bodies stay false even when the tree is true:
      * one flag for the tree and its notes would count a transclusion that exists only in a footnote.
      */
    def noteTransclusions(content: PageContent, transclusionsToo: Boolean): Unit =
      val from: MarkupPage = content.source.page
      if Transclusion.hasInlined(content.xml, from, transclusionsToo) then transclusion = true
      content.footnotes.values.foreach: footnote =>
        footnote.nodes.foreach: node =>
          node.asElement.foreach: element =>
            if Transclusion.hasInlined(element, from, transclusionsToo = false) then transclusion = true

    def absorbMarkup(page: MarkupPage, transclusionsToo: Boolean): Unit =
      page.content.foreach: content =>
        absorb(content)
        noteTransclusions(content, transclusionsToo)
      page.inlineListProse.foreach: source =>
        val prose: PageContent = source.content
        absorb(prose)
        noteTransclusions(prose, transclusionsToo = true)

    site.pages.pages.foreach:
      case page: FullMarkupPage =>
        if Facsimile.needed(page) then facsimile = true
        absorbMarkup(page, transclusionsToo = false)
      case page: MarkupPage =>
        absorbMarkup(page, transclusionsToo = true)
      case _ =>

    detected.union(ReaderFeatures(
      glossary = false,
      citation = false,
      choice = false,
      transclusion = transclusion,
      facsimile = facsimile,
      apparatusAdd = false,
      apparatusDel = false,
      apparatusSee = false
    ))
