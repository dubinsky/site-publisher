package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.link.{BackLink, Fragment, Link, Toc}
import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.site.PageError
import org.podval.xml.{Html, Xml, Xml2Html}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  xml: Xml.Element
):
  def entityKind: Option[EntityKind] = source.markup.entityKind(xml)

  lazy val toc: Toc = Toc(source.markup.sections(source, xml))

  def resolveId(id: String): Option[Link.ToId] = ids.find(_ == id).map(Link.ToId(_))

  private lazy val ids: Seq[String] = source.xmlDialect.gather(xml, _.getId)

  def resolveBlock(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

  private lazy val blocks: Seq[Fragment.Block] = source.xmlDialect.gather(xml, element =>
    if !element.has(Links.BlockClass) then None else element
      .getId
      .map(Fragment.Block(_))
      .orElse:
        source.error(PageError.NoId, s"Defect: No id on block $element")
        None
  )

  def backLinks: Seq[BackLink] = source.xmlDialect.gatherWithParents(
    element = xml,
    gatherElement = (element, parents) => BackLink(
      element = element,
      parents = parents,
      from = source.page,
      content = this
    )
  )

  // TODO maybe make it a lazy val?
  private def xmlFinal: Xml.Element = source.markup.postProcess(source, xml)

  def toHtml(
    sectionId: Option[String],
    isTerminal: Boolean
  ): Html.Element =
    // Select XML to include
    val xmlIncluded: Xml.Element = select(
      xml = xmlFinal,
      sectionId = sectionId,
      isTerminal = isTerminal
    )

    // Convert to HTML
    val html: Html.Element = Xml2Html.fromXml(xmlIncluded)

    // Add TOC to HTML
    var tocAdded: Boolean = false

    def tocHtml: Html.Element = toc.html(
      tocDepth = source.page.tocDepth,
      selectedSectionId = sectionId
    )

    val result: Html.Element = source.xmlDialect.transform(html, element =>
      if tocAdded || !source.markup.isTocPlaceholder(element)
      then
        element
      else
        tocAdded = true
        tocHtml
    )

    if source.page.hasToc && !tocAdded
    then result.setChildren(tocHtml +: result.getChildren)
    else result

  // Select XML
  // sectionId  isTerminal  what is it?         what is included?
  // no         yes         original            everything
  // no         no          TOC chunk           top-level preamble
  // yes        no          intermediate chunk  section preamble
  // yes        yes         terminal chunk      section
  private def select(
    xml: Xml.Element,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Xml.Element = sectionId match
    case None =>
      if isTerminal
      then xml
      else takeChildrenBeforeId(xml, toc.sections.headOption.map(_.id))

    case Some(sectionId) =>
      val sectionXml: Xml.Element = source.markup.section(xml, sectionId, toc)
      if isTerminal
      then sectionXml
      else takeChildrenBeforeId(sectionXml, toc.getById(sectionId).sections.headOption.map(_.id))

  private def takeChildrenBeforeId(xml: Xml.Element, id: Option[String]): Xml.Element = id match
    case None =>
      xml
    case Some(stopAtId) =>
      xml.setChildren(xml.getChildren.takeWhile(
        _.asElement.fold(true)(!_.getId.contains(stopAtId))
      ))
