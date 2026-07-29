package org.podval.tools.publish.html

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.link.Toc
import org.podval.tools.publish.markup.{Converter, Markup}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.{PageError, Path, Site}
import org.podval.xml.{HtmlXmlDialect, Xml}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

object HtmlMarkup extends Markup(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  override def xmlContent(site: Site, sourcePath: Path, content: String): String =
    // Wrap HTML in a 'div' to accommodate multi-root documents.
    s"<div>$content</div>"

  override val xmlConverter: Converter = Converter.concat(
    HtmlSectionIdsConverter()
  )

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

  override def section(xml: Xml.Element, sectionId: String, toc: Toc): Xml.Element =
    if sectionId == "acknowledgements" then
      val x = 0
    val fromSection: Chunk[Xml.Node] = xml.getChildren.dropWhile(!_.asElement.flatMap(_.getId).contains(sectionId))
    val children: Chunk[Xml.Node] = toc.getNextById(sectionId).map(_.id) match
      case None => fromSection
      case Some(nextSectionId) => fromSection.takeWhile(_.asElement.fold(true)(!_.getId.contains(nextSectionId)))
    xml.setChildren(children)
