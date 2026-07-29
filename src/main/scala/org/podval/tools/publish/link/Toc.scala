package org.podval.tools.publish.link

import org.podval.xml.Html
import zio.blocks.html.*
import Fragment.Section

// TODO for chunked pages, links must be to chunked pages!!!
final class Toc(val sections: Seq[Section]):
  def resolveSection(names: Seq[String]): Option[Link.ToSection] = Toc.resolve(
    result = Seq.empty,
    sections = sections,
    names = names,
    includeNested = true
  ).map(Link.ToSection(_))

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
  private def resolve(
    result: Seq[Section],
    sections: Seq[Section],
    names: Seq[String],
    includeNested: Boolean
  ): Option[Seq[Section]] =
    def next(section: Section, includeNested: Boolean) = resolve(
      result = result :+ section,
      sections = section.sections,
      names = names.tail,
      includeNested = includeNested
    )

    if names.isEmpty then Some(result) else sections
      .find(section => section.title == names.head || section.id == names.head)
      .flatMap(section => next(section, includeNested = false))
      .orElse:
        if !includeNested then None else sections
          .flatMap(section => next(section, includeNested = true))
          .headOption
