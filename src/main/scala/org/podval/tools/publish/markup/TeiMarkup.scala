package org.podval.tools.publish.markup

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html, XmlUtil}
import org.podval.xml.XmlUtil.*
import zio.blocks.chunk.Chunk

import java.io.File

object TeiMarkup extends Markup(
  name = "TEI",
  xmlDialect = TeiXmlDialect,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  private val facsimileSymbol: String = "⎙"

  override def rootElements: Set[String] = Set("TEI", "store", "collection") ++ EntityKind.values.map(_.element).toSet

  override def xmlContent(content: String, sourceFile: File, site: Site): String = content

  // Sections in TEI:
  //<div type="section" n="2"> // chapter", "section", "part", "subsection", etc
  //  <head>Methodology</head>
  //  <p>...</p>
  //</div>
  override def isSectionHeader(element: Xml.Element): Boolean = element.getName == "tei-head"

  override def pageHeader(page: MarkupPage): Html.Element =
    super.pageHeader(page) // TODO

  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    // TODO extract title
    val tei2Html: Xml2Html = Xml2Html("tei")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    val result: Xml.Element = xmlDialect.transform(xml, element =>
      var result: Xml.Element = element

      result = tei2Html.convert(result)

      result = result.setChildren(XmlUtil.convertElements(result.getChildren, convertFootnote(_, footnoteCorrelationIds)))

      result = convertSpecial(result)

      result
    )

    (markHeadedDivs(result), None)

  // After Xml2Html, `head` is `tei-head`. Transform is parent-first, so this is a second pass.
  private[markup] def markHeadedDivs(xml: Xml.Element): Xml.Element =
    xmlDialect.transform(xml, element =>
      if element.getName == "div" && sectionHeader(element).isDefined
      then Section.mark(element)
      else element
    )

  private def convertSpecial(element: Xml.Element): Xml.Element = element.getName match
    case "row" =>
      renameElement("tr", element)

    case "cell" =>
      renameElement("td", copyAttribute("cols", "colspan", element))

    case "graphic" =>
      renameElement("image", copyAttribute("url", "src", element))

    case "ref" | "ptr" =>
      renameElement("a", copyAttribute("target", "href", element))

    case name if EntityKind.values.exists(_.nameElement == name) =>
      // TODO turn those into As *only* if 'ref' attribute is present!
      renameElement("a", copyAttribute("ref", "href", element))

    case "pb" =>
      // TODO convert 'n' attribute?
      renameElement("a", element.setText(facsimileSymbol))

    case _ =>
      element

  // Footnotes in TEI:
  // <note place="end" n="3">Footnote body</note>
  // TODO do not ignore n?
  private def convertFootnote(element: Xml.Element, correlationIds: IdGenerator): Option[Xml.Nodes] =
    val isFootnote: Boolean = element.getName == "note" && element.get("place").contains("end")
    if !isFootnote then None else Some:
      val correlationId = correlationIds.generate()
      Chunk(
        Footnote.link(correlationId),
        Footnote.body(correlationId, element.getChildren)
      )
