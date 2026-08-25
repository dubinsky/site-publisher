package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml}
import zio.blocks.chunk.Chunk
import java.io.File

object HtmlMarkup extends Markup(
  name = "HTML",
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  override def isSectionHeader(element: Xml.Element): Boolean = headerLevel(element).isDefined

  // Unwrap a lone leading <p> in td/li/dd (Asciidoctor and FlexMark both emit these).
  private[markup] def unwrapSpuriousParagraph(element: Xml.Element): Option[Xml.Nodes] =
    val isElementToConvert: Boolean = element.getName == "td" || element.getName == "li" || element.getName == "dd"
    if !isElementToConvert then None else
      val (init, tail) = element.getChildren.span(_.asElement.isEmpty)
      for
        head <- tail.headOption.map(_.asElement.get)
        if head.getName == "p"
      yield
        Chunk(element.setChildren(init ++ head.getChildren ++ tail.tail))

  def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = element.getName
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None

  override def xmlContent(content: String, sourceFile: File): String =
    // Wrap HTML in a 'div' to accommodate multi-root documents.
    s"<div>$content</div>"

  override def process(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    val (result: Xml.Element, title: Option[Xml.Element]) = xml
      .getChildren
      .flatMap(_.asElement)
      .find(element => headerLevel(element).contains(1))
      .fold((xml, None)): h1 =>
        (xml.setChildren(xml.getChildren.filterNot(_ eq h1)), Some(h1))

    // Nest HTML sections once the title ('h1') is removed.
    val nested: Xml.Element = result.setChildren(nestSections(result.getChildren))
    (nested.transform(element =>
      Figure.normalize(Strike.normalize(Quote.normalize(Aside.normalize(element))))
    ), title)

  // Wrap each HTML section at the top level in a 'div' with class 'section'.
  // Transplant id from the header element to the section element.
  // Permalinks and missing ids are added later on the markup-independent IR.

  // Sections are represented by the HTML `h` elements and are not nested.
  // Common for markup formats whose XML representation is actually HTML:
  // HTML itself, Markdown, AsciiDoc, and likely Re-Structured text;
  // pure XML markup formats like TEI and DocBook are different.

  // From:
  //   <h2 id="colophon">Colophon</h2>
  //   <p>...</p>
  // To:
  //   <div class="section" id="colophon">
  //     <h2>Colophon</h2>
  //     <p>...</p>
  //   </div>
  private[markup] def nestSections(nodes: Xml.Nodes): Xml.Nodes =
    val headerLevels: Chunk[Int] = nodes.flatMap(_.asElement).flatMap(HtmlMarkup.headerLevel)
    if headerLevels.isEmpty then nodes else
      val headerLevel: Int = headerLevels.head
      val (preamble: Xml.Nodes, rest: Xml.Nodes) = nodes.span(
        _.asElement.fold(true)(HtmlMarkup.headerLevel(_).isEmpty)
      )
      val (body: Xml.Nodes, tail: Xml.Nodes) = rest.tail.span(
        _.asElement.fold(true)(HtmlMarkup.headerLevel(_).fold(true)(_ > headerLevel))
      )
      val section: Xml.Element =
        // Transplant the id from the header to the section.
        val header: Xml.Element = rest.head.asElement.get.copyXmlId
        val id: Option[String] = header.getId.filter(_.nonEmpty)
        Section.mark(Xml.element("div"))
          .setId(id)
          .setChildren(Chunk(header.setId("")) ++ nestSections(body))

      preamble ++ Chunk(section) ++ nestSections(tail)
