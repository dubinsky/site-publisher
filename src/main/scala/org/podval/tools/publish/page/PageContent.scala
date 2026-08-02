package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.markup.{BackLink, Block, Link, LinkKind, Links, Toc}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.PageError
import org.podval.xml.{Html, Xml, Xml2Html}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  xml: Xml.Element
):
  def entityKind: Option[EntityKind] = source.markup.entityKind(xml)

  lazy val toc: Toc = Toc(xml, source)

  def resolveId(id: String): Option[Link.ToId] = ids.find(_ == id).map(Link.ToId(_))

  private lazy val ids: Seq[String] = source.xmlDialect.gather(xml, _.getId)

  def resolveBlock(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

  private lazy val blocks: Seq[Block] = source.xmlDialect.gather(xml, element =>
    if !element.has(Links.BlockClass) then None else element
      .getId
      .map(Block(_))
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
  private def xmlFinal: Xml.Element =
    var result: Xml.Element = source.markup.postProcess(source, xml)
    result = source.xmlDialect.transform(result, element => resolveInternalLinks(element, source).getOrElse(element))
    result

  private def resolveInternalLinks(element: Xml.Element, source: PageSource): Option[Xml.Element] =
    Option.when(element.isA && element.has(Links.InternalLinkClass))(
      element.getHref.fold(element)(resolveInternalLinks(element, source, _))
    )

  private def resolveInternalLinks(
    element: Xml.Element,
    source: PageSource,
    ref: String
  ): Xml.Element =
    val kind: Option[LinkKind] = LinkKind.of(element)
    Link.resolve(ref, kind, source.page) match
      case None =>
        source.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element")
        element.addClass("unresolved-link") // TODO move into Links
      case Some(linkTo) =>
        // TODO transclude
        val result: Xml.Element = element.setHref(linkTo.url)
  
        if result.getText != Links.linkText(element, ref)
        then result
        else result.setText(Links.linkText(element, linkTo.title))

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
  ): Xml.Element =
    val (element, sections) = sectionId match
      case None =>
        (xml, toc.sections)

      case Some(sectionId) =>
        (getSection(xml, sectionId), toc.getById(sectionId).sections)

    if isTerminal then element else sections.headOption.map(_.id) match
      case None =>
        element
      case Some(stopAtId) =>
        element.setChildren(element.getChildren.takeWhile(
          _.asElement.fold(true)(!_.getId.contains(stopAtId))
        ))

  private def getSection(element: Xml.Element, sectionId: String): Xml.Element = source
    .xmlDialect
    .gather(element, element => element.getId.flatMap(id => Option.when(id.contains(sectionId))(element)))
    .head

