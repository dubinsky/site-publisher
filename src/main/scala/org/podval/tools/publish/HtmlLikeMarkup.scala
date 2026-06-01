package org.podval.tools.publish

import org.podval.xml.{Xml, XmlAttribute}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec
import Fragment.Section

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
object HtmlLikeMarkup:
  private final class HtmlSection(
    val level: Int,
    val title: String,
    val id: String
  )
  
abstract class HtmlLikeMarkup extends Markup:
  import HtmlLikeMarkup.HtmlSection
  
  final override protected def recognizeMarkdownWikiLinks: Boolean = true

  final override protected def recognizeMarkdownFootnotes: Boolean = true

  final override protected def recognizeMarkdownBlocks: Boolean = true
  
  final override protected def isSectionElement(element: Xml.Element): Boolean = headerLevel(element).isDefined

  final override protected def sectionTitle(element: Xml.Element): Option[String] = element.getTextOpt

  final override protected def linkKind(element: Xml.Element): Option[Link.Kind] = None

  private def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = element.getName
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None

  // Note: only sections on the top level are detected;
  // sections of levels lower than the level of the first section are not allowed.
  final override protected def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Section] =
    val sectionElements: Chunk[HtmlSection] = element
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element =>
        for
          level <- headerLevel(element)
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

    getSections(sectionElements)

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
  
  final override protected def setFootnoteCorrelationIds(element: Xml.Element): Xml.Element =
    element

  final override protected def isFootnotesContainer(element: Xml.Element): Boolean =
    element.getName == "div"

