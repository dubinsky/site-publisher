package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{ChunkedMarkupPage, DirectoryPage, FullMarkupPage}
import org.podval.xml.{Html, HtmlClass, Xml, XmlUtil}
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

  def chunks(page: FullMarkupPage): Seq[ChunkedMarkupPage] =
    val root: ChunkedMarkupPage = ChunkedMarkupPage(
      page,
      sectionId = None,
      isTerminal = false,
      name = DirectoryPage.fileName
    )
    val rest: Seq[ChunkedMarkupPage] =
      for section <- flatten if section.depth == page.chunkDepth - 2 || section.depth == page.chunkDepth - 1
      yield ChunkedMarkupPage(
        page,
        sectionId = Some(section.id),
        isTerminal = section.depth == page.chunkDepth - 1,
        name = section.id
      )
    root +: rest

  def getSection(id: String): Section = getById(id)

  def chunkName(sectionId: String, chunkDepth: Option[Int]): String = chunkDepth match
    case None => ""
    case Some(chunkDepth) => chunkName(Some(sectionId), chunkDepth)

  def chunkName(sectionId: Option[String], chunkDepth: Int): String = sectionId match
    case None =>
      s"${DirectoryPage.fileName}.${HtmlMarkup.extension}"
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
    isTerminal: Boolean
  ): Xml.Element =
    val (element, sections) = sectionId match
      case None =>
        (xml, this)

      case Some(sectionId) =>
        (XmlUtil.elementById(xml, sectionId), getById(sectionId))

    if isTerminal then element else sections.sections.headOption.map(_.id) match
      case None =>
        element
      case Some(stopAtId) =>
        element.setChildren(element.getChildren.takeWhile(
          _.asElement.fold(true)(!_.getId.contains(stopAtId))
        ))

  def html(
    sectionId: Option[String],
    tocDepth: Int,
    chunkDepth: Option[Int]
  ): Html.Element =
    val current: Option[Section] = sectionId.map(getById)
    div(className := "toc",
      h3("Table of Contents"),
      toHtml(
        sections,
        current,
        tocDepth,
        chunkDepth
      )
    )

  // Full outline when there is no current chunk (root chunk, unchunked, print).
  // Content chunks: expand ancestors and the current subtree; siblings and other branches are titles only.
  private def showChildren(section: Section, current: Option[Section], tocDepth: Int): Boolean =
    section.sections.nonEmpty && {
      current match
        case None =>
          section.depth < tocDepth - 1
        case Some(current) =>
          if current.path.init.exists(_.id == section.id) then true
          else if section.path.exists(_.id == current.id) then
            section.depth < current.depth + tocDepth - 1
          else false
    }

  private def itemClass(section: Section, current: Option[Section]): String =
    if current.exists(_.id == section.id) then "toc-section toc-current"
    else if current.exists(_.path.init.exists(_.id == section.id)) then "toc-section toc-ancestor"
    else "toc-section"

  private def toHtml(
    sections: Seq[Section],
    current: Option[Section],
    tocDepth: Int,
    chunkDepth: Option[Int]
  ): Html.Element =
    ul(className := "toc", sections.map(section =>
      val sectionId: String = section.id
      li(
        className := itemClass(section, current),
        a(href := s"${chunkName(sectionId, chunkDepth)}#$sectionId", section.title),
        Option.when(showChildren(section, current, tocDepth))(
          toHtml(
            section.sections,
            current,
            tocDepth,
            chunkDepth
          )
        )
      )
    ))

object Toc:
  object PlaceholderClass extends HtmlClass("toc-placeholder")

  def placeholder: Xml.Element = Xml.element("div").add(PlaceholderClass)

  def apply(
    element: Xml.Element,
    errorReporter: PageErrorReporter
  ): Toc =
    def sections(element: Xml.Element): Chunk[Section] =
      if !Section.is(element) then element.flatMapElements(sections) else element.getId match
        case None =>
          errorReporter.error(PageError.NoId, s"Defect: No id on section $element")
          element.flatMapElements(sections)
        case Some(id) =>
          val title: String = Section.heading(element).map(Section.headingText) match
            case None =>
              throw IllegalStateException(s"Defect: No heading on section $id")
            case Some(text) if text.isEmpty =>
              errorReporter.error(PageError.NoTitle, s"No title on section $id")
              id
            case Some(text) =>
              text
          Chunk(Section(
            id,
            title,
            element.flatMapElements(sections)
          ))

    new Toc(sections(element))
