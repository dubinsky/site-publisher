package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{ChunkedMarkupPage, FullMarkupPage}
import org.podval.xml.{Html, Xml, XmlDialect, XmlUtil}
import zio.blocks.html.*
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import zio.blocks.chunk.Chunk

final class Toc(sections: Seq[Section]) extends Sections(sections):
  private val id2section: Map[String, Section] = flatten
    .map(section => section.id -> section)
    .toMap

  // id must exist within this TOC
  private def getById(id: String): Section =
    id2section(id)

  def resolveSection(names: Seq[String]): Option[Link.ToSection] =
    resolve(
      result = Seq.empty,
      names = names,
      includeNested = true
    )
      .map(Link.ToSection(_))

  // TODO TOC: DirectoryPage.fileName!
  private def tocChunkName(markupPage: FullMarkupPage): String = markupPage.path.fileName

  def chunks(page: FullMarkupPage): Seq[ChunkedMarkupPage] =
    def chunks(depth: Int, isTerminal: Boolean): Seq[ChunkedMarkupPage] =
      for section <- flatten.filter(_.depth == depth) yield ChunkedMarkupPage(
        page,
        sectionId = Some(section.id),
        isTerminal = isTerminal,
        name = section.id
      )

    Seq(ChunkedMarkupPage(page, sectionId = None, isTerminal = false, name = tocChunkName(page))) ++
    chunks(depth = page.chunkDepth-2, isTerminal = false) ++
    chunks(depth = page.chunkDepth-1, isTerminal = true)

  def chunkName(sectionId: String, chunkDepth: Option[Int]): String = chunkDepth match
    case None => ""
    case Some(chunkDepth) => chunkName(Some(sectionId), chunkDepth)

  def chunkName(sectionId: Option[String], chunkDepth: Int): String = sectionId match
    case None =>
      "" // TODO TOC Chunk!
    case Some(sectionId) =>
      var section: Section = getById(sectionId)
      while section.depth > chunkDepth - 1 do section = section.parent.get
      s"${section.id}.${HtmlMarkup.extension}"

  // Select XML
  // sectionId  isTerminal  what is it?         what is included?
  // no         yes         original            everything
  // no         no          TOC chunk           top-level preamble
  // yes        no          intermediate chunk  section preamble
  // yes        yes         terminal chunk      section
  def select(
    xml: Xml.Element,
    sectionId: Option[String],
    isTerminal: Boolean,
    xmlDialect: XmlDialect
  ): Xml.Element =
    val (element, sections) = sectionId match
      case None =>
        (xml, this)

      case Some(sectionId) =>
        (XmlUtil.elementById(xml, sectionId, xmlDialect), getById(sectionId))

    if isTerminal then element else sections.sections.headOption.map(_.id) match
      case None =>
        element
      case Some(stopAtId) =>
        element.setChildren(element.getChildren.takeWhile(
          _.asElement.fold(true)(!_.getId.contains(stopAtId))
        ))

  // TODO Toc of a chunk should not have subsections of the other chunks listed...
  def html(
    sectionId: Option[String],
    tocDepth: Int,
    chunkDepth: Option[Int]
  ): Html.Element =
    div(className := "toc",
      h3("Table of Contents"),
      toHtml(
        sections,
        sectionId,
        tocDepth,
        chunkDepth
      )
    )

  private def toHtml(
    sections: Seq[Section],
    selectedSectionId: Option[String],
    tocDepth: Int,
    chunkDepth: Option[Int]
  ): Html.Element =
    ul(className := "toc", sections.map(section =>
      val sectionId: String = section.id
      li(
        className := (if selectedSectionId.contains(sectionId) then "toc-current" else "toc-section"),
        a(href := s"${chunkName(sectionId, chunkDepth)}#$sectionId", section.title),
        Option.when(section.depth < tocDepth-1 && section.sections.nonEmpty)(
          toHtml(
            section.sections,
            selectedSectionId,
            tocDepth,
            chunkDepth
          )
        )
      )
    ))

object Toc:
  def apply(element: Xml.Element, errorReporter: PageErrorReporter): Toc =
    def sections(element: Xml.Element): Chunk[Section] =
      if !Section.is(element) then Chunk.empty else element.getId match
        case None =>
          errorReporter.error(PageError.NoId, s"Defect: No id on section $element")
          Chunk.empty
        case Some(id) =>
          val headerElement: Option[Xml.Element] = element.getChildren.flatMap(_.asElement).headOption
          // TODO ask Markup if this is, indeed, a header element
          // Same when assigning section titles, which should be centralized...
          val title: String = headerElement.map(_.getText).getOrElse(s"Untitled Section $id") // TODO error
          Chunk(Section(
            id,
            title,
            element.flatMapElements(sections)
          ))

    new Toc(element.flatMapElements(sections))
