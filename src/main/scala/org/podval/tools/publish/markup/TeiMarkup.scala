package org.podval.tools.publish.markup

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.page.MarkupPage
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html, XmlAttribute, XmlUtil}
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

    // Xml2Html prefixes reserved HTML attributes (`class` → `tei-class`, `lang` → `tei-lang`).
    // Convert footnotes, glossary, and code in a second pass so IR `class` values are kept.
    val converted: Xml.Element = xml.transform(
      element => convertSpecial(tei2Html.convert(element)),
      stopAtCode = false
    )
    // TODO does it really need to be a separate pass?
    val withIr: Xml.Element = converted.transform(
      element =>
        var result: Xml.Element = element.setChildren(
          XmlUtil.convertElements(element.getChildren, convertFootnote(_, footnoteCorrelationIds))
        )
        result = convertGlossary(result).getOrElse(result)
        // Do not re-wrap `<code>` already inside `<pre>`.
        if result.getName != "pre" then
          result = result.setChildren(XmlUtil.convertElements(result.getChildren, convertCode))
        result,
      stopAtCode = false
    )

    (markHeadedDivs(withIr), None)

  // After Xml2Html, `head` is `tei-head`. Transform is parent-first, so this is a second pass.
  private[markup] def markHeadedDivs(xml: Xml.Element): Xml.Element =
    xml.transform(
      element =>
        if element.getName == "div" && sectionHeader(element).isDefined
        then Section.mark(element)
        else element,
      stopAtCode = false
    )

  private def convertSpecial(element: Xml.Element): Xml.Element = element.getName match
    case "row" =>
      renameElement("tr", element)

    case "cell" =>
      renameElement("td", copyAttribute("cols", "colspan", element))

    case "graphic" =>
      renameElement("image", copyAttribute("url", "src", element))

    case "ref" | "ptr" =>
      teiHref(element).fold(renameElement("a", element))(value =>
        renameElement("a", element.setHref(value))
      )

    case "term" =>
      teiHref(element).fold(element)(value => renameElement("a", element.setHref(value)))

    case name if EntityKind.values.exists(_.nameElement == name) =>
      // TODO turn those into As *only* if 'ref' attribute is present!
      renameElement("a", copyAttribute("ref", "href", element))

    case "pb" =>
      // TODO convert 'n' attribute?
      renameElement("a", element.setText(facsimileSymbol))

    case _ =>
      element

  // Xml2Html prefixes reserved HTML attributes (`target` → `tei-target`).
  private def teiHref(element: Xml.Element): Option[String] =
    element.getHref
      .orElse(element.get("ref"))
      .orElse(element.get("target"))
      .orElse(element.get("tei-target"))

  // Glossary in TEI: <list type="gloss"> (also type="glossary") of <label>/<item>.
  // Convert after Xml2Html so Glossary IR classes are not prefixed to tei-class.
  private def convertGlossary(element: Xml.Element): Option[Xml.Element] =
    val isGlossList: Boolean =
      element.getName == "list" &&
      element.get("type").exists(t => t == "gloss" || t == "glossary")
    if !isGlossList then None
    else Some:
      renameElement("dl", element)
        .setChildren(groupGlossEntries(element.getChildren))
        .add(Glossary.ListClass)

  private def groupGlossEntries(nodes: Xml.Nodes): Xml.Nodes =
    var result: List[Xml.Node] = Nil
    var pendingLabel: Option[Xml.Element] = None

    def xmlId(element: Xml.Element): Option[String] =
      element.getId.filter(_.nonEmpty).orElse(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))

    def asDt(label: Xml.Element): Xml.Element =
      Xml.element("dt").setChildren(label.getChildren.filterNot(_.isWhitespace))

    def asDd(item: Xml.Element): Xml.Element =
      Xml.element("dd").setChildren(item.getChildren.filterNot(_.isWhitespace))

    def emit(label: Xml.Element, item: Option[Xml.Element]): Unit =
      val dt: Xml.Element = asDt(label)
      val dd: Option[Xml.Element] = item.map(asDd)
      val id: Option[String] =
        xmlId(label).orElse(item.flatMap(xmlId)).orElse:
          val text: String = dt.getText.trim
          Option.when(text.nonEmpty)(Xml.toId(text))
      result = result :+ Glossary.item(id, Chunk.from(dt +: dd.toSeq))

    nodes.foreach: node =>
      node.asElement match
        case Some(label) if label.getName == "label" =>
          pendingLabel.foreach(emit(_, None))
          pendingLabel = Some(label)
        case Some(item) if item.getName == "item" =>
          pendingLabel match
            case Some(label) =>
              emit(label, Some(item))
              pendingLabel = None
            case None =>
              result = result :+ node
        case Some(_) =>
          pendingLabel.foreach(emit(_, None))
          pendingLabel = None
          result = result :+ node
        case None =>
          if !node.isWhitespace then
            pendingLabel.foreach(emit(_, None))
            pendingLabel = None
            result = result :+ node

    pendingLabel.foreach(emit(_, None))
    Chunk.from(result)

  // Code in TEI: <code lang="scala"> (tagdocs). Xml2Html leaves the element name
  // and prefixes @lang to tei-lang. Inline stays <code class="language-…">;
  // a newline means a block, wrapped in <pre>. Do not convert <eg> / <egXML>.
  private def convertCode(element: Xml.Element): Option[Xml.Nodes] =
    if element.getName != "code" then None
    else
      val lang: Option[String] =
        element.get("lang").orElse(element.get("tei-lang")).map(_.trim).filter(_.nonEmpty)
      var code: Xml.Element = element
      lang.foreach: name =>
        val cls: String = s"language-${name.toLowerCase}"
        if !code.hasClass(cls) then code = code.addClass(cls)
      val wrapped: Xml.Element =
        if code.getText.contains('\n') then Xml.element("pre").setChildren(Chunk(code))
        else code
      Some(Chunk(wrapped))

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
