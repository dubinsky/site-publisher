package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.tools.publish.link.{BackLink, Fragment, Toc}
import org.podval.tools.publish.markup.{Links, Markup, MarkupKind}
import org.podval.tools.publish.util.{Date, IdGenerator}
import org.podval.xml.{Html, Xml, Xml2Html, XmlDialect}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  private var xmlVar: Xml.Element
):
  def xml: Xml.Element = xmlVar

  // Run converters
  private val ids: IdGenerator = IdGenerator("_generated_id")
  private val footnoteCorrelationIds: IdGenerator = IdGenerator("")

  xmlVar = xmlDialect.transform(xml, element =>
    markup.converters.foldLeft(element)((result, converter) =>
      converter.convertWithIds(result, this, ids, footnoteCorrelationIds)
    )
  )

  // Run transformers
  xmlVar = markup.transformers.foldLeft(xml)((result, transformer) =>
    transformer.transform(xml, this)
  )

  def page: MarkupPage = source.page
  def site: Site = page.site
  def sourcePath: Path = source.sourcePath
  def markup: Markup = source.markup
  def markupKind: MarkupKind = markup.kind
  def xmlDialect: XmlDialect = markupKind.xmlDialect

  def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = site.error(
    sourcePath,
    kind,
    message,
    cause
  )

  def entityKind: Option[EntityKind] = markupKind.entityKind(xml)
  
  // TODO take content into account:
  def author: Option[String] = frontMatter.author
  def title: Option[String] = frontMatter.title
  def description: Option[String] = frontMatter.description
  def date: Option[Date] = frontMatter.date
  def dateModified: Option[Date] = frontMatter.modifiedTime
  def lang: Option[String] = frontMatter.lang

  lazy val toc: Toc = Toc(
    sections = markupKind.sections(this),
    ids = xmlDialect.gather(xml, _.getId),
    blocks = xmlDialect.gather(xml, element =>
      if !Links.isBlock(element)
      then None
      else element
        .getId
        .map(Fragment.Block(_))
        .orElse:
          error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
  )

  def backLinks: Seq[BackLink] =
    xmlDialect.gatherWithParents(
      element = xml,
      gatherElement = BackLink(_, _, page, toc)
    )

  def toHtml: Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = xmlDialect.transform(xml, element =>
      markup.postConverters.foldLeft(element)((result, postConverter) =>
        postConverter.postConvert(result, this)
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Post-process HTML
    xmlDialect.transform(htmlResult, element =>
      markup.htmlConverters.foldLeft(element)((result, htmlConverter) =>
        htmlConverter.convertHtml(result, this)
      )
    )
