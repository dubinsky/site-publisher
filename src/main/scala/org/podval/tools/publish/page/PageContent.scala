package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{Blocks, Ids, Link, Section, Toc}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlDialect}

final class PageContent private(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  val xml: Xml.Element,
  val toc: Toc,
  val ids: Ids,
  val blocks: Blocks
)

object PageContent:
  def apply(
    source: PageSource,
    frontMatter: FrontMatter,
    xml: Xml.Element
  ): PageContent =
    val xmlDialect: XmlDialect = source.markup.xmlDialect
    
    // Run markup-specific processors and extract title
    val (xmlProcessed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(source, xml)

    // TODO error if both front matter and content titles are present and are different.

    // Prepare to calculate Toc and backlinks.
    // Footnotes are left in the XML for PageContentResolved to resolve them;
    // they are not affected by transformations here
    // since they are neither sections nor links at this point.
    val ids: IdGenerator = IdGenerator("_generated_id")
    val result: Xml.Element = xmlDialect.transform(xmlProcessed, element =>
      var result: Xml.Element = element
      // Note this is done so that Toc can be calculated.
      // TODO do section titles from the first element here, not in HtmlMarkdown and TeiMarkdown.
      result = Section.setSectionId(result, ids).getOrElse(result)
      // Note: this is done so that backlinks can be calculated.
      result = Link.setAnchorId(result, ids).getOrElse(result)
      // Note: this is done so that backlinks can be calculated.
      result = Link.markInternal(result, source.page.site, source).getOrElse(result)
      result
    )

    new PageContent(
      source = source,
      frontMatter = frontMatter,
      title = title,
      xml = result,
      toc = Toc(result, source),
      ids = Ids(result, xmlDialect),
      blocks = Blocks(result, xmlDialect, source)
    )
