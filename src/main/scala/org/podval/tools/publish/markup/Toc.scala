package org.podval.tools.publish.markup

import org.podval.xml.{Html, Xml, XmlDialect}
import zio.blocks.html.*
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import zio.blocks.chunk.Chunk

// TODO for chunked pages, links must be to chunked pages!!!
final class Toc(override val sections: Seq[Section]) extends Sections:
  def resolveSection(names: Seq[String]): Option[Link.ToSection] =
    resolve(
      result = Seq.empty,
      names = names,
      includeNested = true
    )
      .map(Link.ToSection(_))

  // Id must exist within this TOC
  def getById(id: String): Section =
    def loop(section: Section): Option[Section] =
      if section.id == id
      then Some(section)
      else section.sections.flatMap(loop).headOption

    sections.flatMap(loop).head

  def getNextById(id: String): Option[Section] =
    def loop(sections: Seq[Section]): Option[Option[Section]] =
      if sections.map(_.id).contains(id)
      then Some(sections.dropWhile(_.id != id).drop(1).headOption)
      else sections.flatMap(section => loop(section.sections)).headOption

    val result = loop(sections).flatten
    result

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
        (Toc.getSection(xml, sectionId, xmlDialect), getById(sectionId))

    if isTerminal then element else sections.sections.headOption.map(_.id) match
      case None =>
        element
      case Some(stopAtId) =>
        element.setChildren(element.getChildren.takeWhile(
          _.asElement.fold(true)(!_.getId.contains(stopAtId))
        ))

  def add(
    html: Html.Element,
    hasToc: Boolean,
    tocDepth: Int,
    sectionId: Option[String],
    markup: Markup
  ): Html.Element =
    // Add TOC to HTML
    var tocAdded: Boolean = false

    def tocHtml: Html.Element = this.html(
      tocDepth = tocDepth,
      selectedSectionId = sectionId
    )

    val result: Html.Element = markup.xmlDialect.transform(html, element =>
      if tocAdded || !markup.isTocPlaceholder(element)
      then
        element
      else
        tocAdded = true
        tocHtml
    )

    if hasToc && !tocAdded
    then result.setChildren(tocHtml +: result.getChildren)
    else result

  def html(tocDepth: Int, selectedSectionId: Option[String]): Html.Element =
    def html(
      sections: Seq[Section],
      depth: Int
    ): Html.Element =
      ul(sections.map(section =>
        val sectionId: String = section.id
        li(
          className := (if selectedSectionId.contains(sectionId) then "toc-section-selected" else "toc-section"),
          a(href := s"#$sectionId", section.title),
          Option.when(depth > 1 && section.sections.nonEmpty)(
            html(
              section.sections,
              depth = depth - 1
            )
          )
        )
      ))

    div(className := "toc",
      h3("Table of Contents"),
      html(
        sections,
        depth = tocDepth
      )
    )

object Toc:
  def apply(element: Xml.Element, errorReporter: PageErrorReporter): Toc =
    def sections(element: Xml.Element): Chunk[Section] =
      val isSection: Boolean = element.getName == "div" && element.has(Section.SectionClass)
      if !isSection then Chunk.empty else element.getId match
        case None =>
          errorReporter.error(PageError.NoId, s"Defect: No id on section $element")
          Chunk.empty
        case Some(id) =>
          val headerElement: Option[Xml.Element] = element.getChildren.flatMap(_.asElement).headOption
          // TODO ask Markup if this is, indeed, a header element
          val title: String = headerElement.map(_.getText).getOrElse(s"Untitled Section $id") // TODO error
          Chunk(Section(
            id,
            title,
            element.flatMapElements(sections)
          ))

    new Toc(element.flatMapElements(sections))

  def getSection(
    xml: Xml.Element,
    sectionId: String,
    xmlDialect: XmlDialect
  ): Xml.Element = xmlDialect
    .gather(xml, element => element.getId.flatMap(id => Option.when(id.contains(sectionId))(element)))
    .head
