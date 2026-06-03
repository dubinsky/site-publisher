package org.podval.tools.publish.markup

import org.podval.tools.publish.Fragment.Section
import org.podval.tools.publish.PageError
import org.podval.xml.{HtmlXmlDialect, Xml, XmlAttribute, XmlDialect}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
object HtmlLikeMarkup:
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

// Markup that parses into HTML.
// Sections are represented by the HTML `h` elements and are not nested.
abstract class HtmlLikeMarkup extends Markup:
  import HtmlLikeMarkup.HtmlSection

  final override def xmlDialect: XmlDialect = HtmlXmlDialect

  // Note: only sections on the top level are detected;
  // sections of levels lower than the level of the first section are not allowed.
  final override def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Section] =
    val fromHeaders: Chunk[HtmlSection] = element
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element =>
        for
          level <- HtmlLikeMarkup.headerLevel(element)
          title <- element.getTextOpt
          id <-
            val id = element.get(XmlAttribute.Id)
            if id.isEmpty then errorReporter.error(PageError.NoId, s"Defect: No id on section $element", ())
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

