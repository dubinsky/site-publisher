package org.podval.tools.publish

import org.podval.xml.Html
import zio.blocks.html.*
import Fragment.Section

final class Toc(
  sections: Seq[Section],
  blocks: Seq[Fragment.Block],
  ids: Seq[String],
):
  def resolveId(id: String): Option[Link.ToId] = ids.find(_ == id).map(Link.ToId(_))

  def resolveBlock(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

  def resolveSection(names: Seq[String]): Option[Link.ToSection] = Toc.resolve(
    result = Seq.empty,
    sections = sections,
    names = names,
    includeNested = true
  ).map(Link.ToSection(_))
  
  def html: Html.Element =
    div(className := "toc",
      h3("Table of Contents"),
      html(sections)
    )

  private def html(sections: Seq[Fragment.Section]): Html.Element =
    ul(sections.map(section =>
      li(
        a(href := s"#${section.id}", section.title),
        Option.when(section.sections.nonEmpty)(html(section.sections))
      )
    ))

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

  def isKramdownTocMarker(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
          child.getChildren.length == 1 &&
          child.getChildren.head.asText.fold(false): text =>
            text.endsWith("{:toc}")
