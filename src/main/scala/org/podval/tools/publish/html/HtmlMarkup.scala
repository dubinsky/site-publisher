package org.podval.tools.publish.html

import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.markup.{MarkupKind, Processor}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, HtmlXmlDialect, Xml}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

object HtmlMarkup extends MarkupKind(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  def processors: Seq[Processor] = Seq(
    new HtmlSectionIdsConverter
  )

  override def pageHeader(page: MarkupPage): Html.Element = MarkupKind.pageHeader(page)

  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = xml
    .getChildren
    .flatMap(_.asElement)
    .find(element => HtmlSection.headerLevel(element).contains(1))
    .fold((xml, None)): h1 =>
      (xml.setChildren(xml.getChildren.filterNot(_ eq h1)), Some(h1))

  // Note: only sections on the top level are detected;
  // sections of levels lower than the level of the first section are not allowed.
  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] =
    val fromHeaders: Chunk[HtmlSection] = xml
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element =>
        for
          level <- HtmlSection.headerLevel(element)
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
