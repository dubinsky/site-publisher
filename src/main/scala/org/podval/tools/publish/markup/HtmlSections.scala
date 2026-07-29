package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.PageError
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// Sections are represented by the HTML `h` elements and are not nested.
// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, AsciiDoc, and likely Re-Structured text;
// pure XML markup formats like TEI and DocBook are different.
// Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`;
// for AsciiDoc, by setting `sectids` attribute to `true` -
// but I do it manually and uniformly for HTML, Markdown, etc.
object HtmlSections:
  private final class IdsConverter extends Converter:
    override def convert(
      element: Xml.Element,
      source: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Option[Xml.Element] =
    Option.when(element.getId.isEmpty && headerLevel(element).isDefined)(
      element.setId(element.getTextOpt.fold(ids.generate())(Xml.toId))
    )

  def processors: Seq[Processor] = Seq(
    new IdsConverter
  )

  def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = element.getName
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None

  private final class HtmlSection(
    val level: Int,
    val title: String,
    val id: String
  )

  // Note: only sections on the top level are detected;
  // sections of levels lower than the level of the first section are not allowed.
  def sections(source: PageSource, xml: Xml.Element): Seq[Section] =
    val fromHeaders: Chunk[HtmlSection] = xml
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element =>
        for
          level <- headerLevel(element)
          title <- element.getTextOpt
          id <-
            val id: Option[String] = element.getId
            if id.isEmpty then source.error(PageError.NoId, s"Defect: No id on section $element")
            id
        yield HtmlSection(
          level = level,
          title = title,
          id = id
        )
      )

    getSections(fromHeaders)

  private def getSections(sections: Chunk[HtmlSection]): Seq[Section] =
    if sections.isEmpty
    then Seq.empty
    else getSections(Seq.empty, sections.head.level, sections)

  @tailrec
  private def getSections(result: Seq[Section], level: Int, sections: Chunk[HtmlSection]): Seq[Section] =
    if sections.isEmpty then result else
      val head: HtmlSection = sections.head
      val (nested: Chunk[HtmlSection], tail: Chunk[HtmlSection]) = sections.tail.span(_.level > head.level)
      val section: Section = Section(
        id = head.id,
        title = head.title,
        sections = getSections(nested)
      )

      getSections(
        result :+ section,
        level,
        tail
      )

  def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = xml
    .getChildren
    .flatMap(_.asElement)
    .find(element => headerLevel(element).contains(1))
    .fold((xml, None)): h1 =>
      (xml.setChildren(xml.getChildren.filterNot(_ eq h1)), Some(h1))
