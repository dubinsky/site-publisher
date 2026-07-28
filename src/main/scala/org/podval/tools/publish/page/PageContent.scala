package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.tools.publish.link.{BackLink, Fragment, Toc}
import org.podval.tools.publish.markup.{Links, Markup, MarkupKind}
import org.podval.tools.publish.util.Date
import org.podval.xml.{Html, Xml, XmlDialect}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  private var xmlVar: Xml.Element
):
  def xml: Xml.Element = xmlVar

  // Process XML
  xmlVar = markup.processors.process(this)
  
  def page: OriginalMarkupPage = source.page
  def site: Site = page.site
  def sourcePath: Path = source.sourcePath
  def markup: Markup = source.markup
  def markupKind: MarkupKind = markup.kind
  def xmlDialect: XmlDialect = markupKind.xmlDialect

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
          source.error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
  )

  def backLinks: Seq[BackLink] =
    xmlDialect.gatherWithParents(
      element = xml,
      gatherElement = BackLink(_, _, page, toc)
    )

  def toHtml: Html.Element = markup.processors.toHtml(this)
