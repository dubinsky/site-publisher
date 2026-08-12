package org.podval.tools.publish.markup

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html}
import org.podval.xml.XmlUtil.*
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
  override def isSectionHeader(element: Xml.Element): Boolean = element.getName == "head"

  override def pageHeader(page: MarkupPage): Html.Element =
    super.pageHeader(page) // TODO

  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    val tei2Html: Xml2Html = Xml2Html("tei")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")
    val result: Xml.Element = xmlDialect.transform(xml, element =>
      var result: Xml.Element = element

      result = tei2Html.convert(result)

      // TODO merge into a match on the name:
      result = convertSpecial(result)
      if EntityKind.values.exists(_.nameElement == result.getName) then
        // TODO turn those into As *only* if 'ref' attribute is present!
        result = renameElement("a", copyAttribute("ref", "href", element))

      if element.getName == "pb" then
        // TODO convert 'n' attribute?
        result = renameElement("a", element.setText(facsimileSymbol))

      if result.getName == "note" && result.get("place").contains("end") then
        // TODO generate correlationId and replace footnote element with link stub *and* body stub with the same correlationId
        ()

      if result.getName == "div" then Section.mark(result)

      result
    )
    (result, None)

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
