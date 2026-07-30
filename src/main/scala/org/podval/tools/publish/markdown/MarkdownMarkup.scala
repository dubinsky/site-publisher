package org.podval.tools.publish.markdown

import org.podval.tools.publish.html.{HtmlMarkup, HtmlSectionIdsConverter}
import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.link.Toc
import org.podval.tools.publish.markup.{Converter, Markup}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

object MarkdownMarkup extends Markup(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  override def converters(
    ids: IdGenerator = IdGenerator("_generated_id"),
    source: PageSource
  ): Seq[Converter] = Seq(
    BlocksConverter(source),
    WikiLinksConverter(),
    MarkdownFootnotesConverter(),
    FlexMarkFootnoteLinksConverter(),
    FlexMarkFootnoteBodiesConverter(),
    HtmlSectionIdsConverter(ids)
  )

  override def postConverters(
    source: PageSource
  ): Seq[Converter] = Seq(
    WikiLinksPostConverter()
  )

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.hasClass("footnotes")

  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = HtmlMarkup.retrieveTitle(xml)

  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] = HtmlMarkup.sections(source, xml)

  override def section(xml: Xml.Element, sectionId: String, toc: Toc): Xml.Element = HtmlMarkup.section(xml, sectionId, toc)

  override def xmlContent(
    site: Site,
    sourcePath: Path,
    content: String
  ): String =
    // Wrap Markdown rendered as HTML in a 'div'.
    s"<div>${FlexMark.parseAndRenderMarkdown(content)}</div>"

  // Kramdown Toc Marker
  override def isTocPlaceholder(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
