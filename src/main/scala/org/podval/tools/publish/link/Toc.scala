package org.podval.tools.publish.link

import org.podval.xml.{Html, Xml}
import zio.blocks.html.*
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.PageError
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
  def apply(element: Xml.Element, source: PageSource): Toc =
    def sections(element: Xml.Element): Chunk[Section] =
      val isSection: Boolean = element.getName == "div" && element.has(Section.SectionClass)
      if !isSection then Chunk.empty else element.getId match
        case None =>
          source.error(PageError.NoId, s"Defect: No id on section $element")
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
