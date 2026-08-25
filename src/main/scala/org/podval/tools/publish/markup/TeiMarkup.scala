package org.podval.tools.publish.markup

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.page.MarkupPage
import org.podval.tools.publish.site.PageErrorReporter
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

  override def xmlContent(content: String, sourceFile: File): String = content

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
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    // TODO extract title
    val tei2Html: Xml2Html = Xml2Html("tei")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    // Xml2Html prefixes reserved HTML attributes (`class` → `tei-class`). Convert
    // footnotes in a second pass so the Footnote IR keeps `class="footnote-link"`.
    val converted: Xml.Element = xmlDialect.transform(xml, element =>
      convertSpecial(tei2Html.convert(element))
    )
    // TODO does it really need to be a separate pass?
    val withFootnotes: Xml.Element = xmlDialect.transform(converted, element =>
      element.setChildren(XmlUtil.convertElements(element.getChildren, convertFootnote(_, footnoteCorrelationIds)))
    )

    (markHeadedDivs(withFootnotes), None)

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
