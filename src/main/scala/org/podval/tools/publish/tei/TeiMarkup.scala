package org.podval.tools.publish.tei

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.markup.{Markup, XmlMarkup}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html}
import org.podval.xml.XmlUtil.*
import java.io.File

object TeiMarkup extends Markup(
  name = "TEI",
  xmlDialect = TeiXmlDialect,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  override def rootElements: Set[String] = Set("TEI", "store", "collection") ++ EntityKind.values.map(_.element).toSet

  override def xmlContent(content: String, sourceFile: File): String = content

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    val tei2Html: Xml2Html = Xml2Html("tei")
    val result: Xml.Element = xmlDialect.transform(xml, element =>
      var result: Xml.Element = element
      result = tei2Html.convert(result)
      result = convertSpecial(result)
      result = convertEntityName(result).getOrElse(result)
      result = convertFacsimileLink(result).getOrElse(result)
      result = convertFootnote(result).getOrElse(result)
      result = convertSection(result).getOrElse(result)
      result
    )
    (result, None)

  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  private def convertEntityName(element: Xml.Element): Option[Xml.Element] =
    // TODO turn those into As *only* if 'ref' attribute is present!
    Option.when(EntityKind.values.exists(_.nameElement == element.getName))(
      renameElement("a", copyAttribute("ref", "href", element))
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

    case _ =>
      element

  private val facsimileSymbol: String = "⎙"

  private def convertFacsimileLink(element: Xml.Element): Option[Xml.Element] =
    // TODO convert 'n' attribute?
    Option.when(element.getName == "pb")(
      renameElement("a", element.setText(facsimileSymbol))
    )

  // Sections in TEI:
  //<div type="section" n="2"> // chapter", "section", "part", "subsection", etc
  //  <head>Methodology</head>
  //  <p>...</p>
  //</div>
  private def convertSection(element: Xml.Element): Option[Xml.Element] =
      Option.when(element.getName == "div" && element.getId.isEmpty)(
        element.setId(sectionTitle(element).map(Xml.toId))
      )

  private def sectionTitle(element: Xml.Element): Option[String] = element
    .getChildren
    .flatMap(_.asElement)
    .find(element => element.getName == "head")
    .flatMap(_.getTextOpt)

  // TODO split into body and link stubs - and simplify the non-markup-specific footnotes processing!
  //  private val footnoteCorrelationIds: Prefixed = Prefixed("")
  //  def footnoteCorrelationId(): String = footnoteCorrelationIds.generate()
  private def convertFootnote(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.getName == "note" && element.get("place").contains("end"))(
      element // TODO
//      Footnotes.linkAndBodyStub(element, ids.footnoteCorrelationId())
    )

  //// Replace footnotes with link stubs
  //final class FootnoteLinksTransformer(xmlDialect: XmlDialect) extends Transformer:
  //  override def transform(element: Xml.Element): Xml.Element =
  //    xmlDialect.transform(element, element =>
  //      Footnotes.getCorrelationId(element).fold(element)(Footnotes.linkStub)
  //    )
  //

  override def pageHeader(page: MarkupPage): Html.Element =
    super.pageHeader(page) // TODO

