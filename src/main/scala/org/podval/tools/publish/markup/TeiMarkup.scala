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
    // Convert footnotes, glossary, quotes, and code in a second pass so IR `class` values are kept.
    val converted: Xml.Element = xml.transform(
      element => convertSpecial(tei2Html.convert(element)),
      stopAtCode = false
    )
    val headerBiblIds: Set[String] = headerListBiblEntryIds(converted)
    val biblIds: Set[String] = listBiblIds(converted, headerBiblIds)
    // TODO does it really need to be a separate pass?
    val withIr: Xml.Element = converted.transform(
      element =>
        var result: Xml.Element = element.setChildren(
          XmlUtil.convertElements(element.getChildren, convertFootnote(_, footnoteCorrelationIds))
        )
        result = convertGlossary(result).getOrElse(result)
        result = convertListBibl(result, headerBiblIds)
        result = convertCite(result, biblIds)
        result = fillEmptyPointer(result, biblIds)
        result = convertBibliographyPlaceholder(result)
        result = convertQuote(result)
        result = convertFigure(result)
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
      renameElement("img", copyAttribute("url", "src", element))

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

  private def xmlId(element: Xml.Element): Option[String] =
    element.getId.filter(_.nonEmpty).orElse(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))

  // Figure in TEI: <figure> with <graphic url> (already <img>) and optional <head>/<figDesc>.
  // Convert after Xml2Html so Figure IR classes are not prefixed to tei-class.
  private def convertFigure(element: Xml.Element): Xml.Element =
    if element.getName != "figure" || Figure.is(element) then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val heads: Xml.Nodes = children.filter(isTeiCaption)
      val descs: Xml.Nodes = children.filter(isFigDesc)
      val captionSource: Xml.Nodes = if heads.nonEmpty then heads else descs
      val body: Xml.Nodes =
        children.filterNot: node =>
          heads.exists(_ eq node) || (heads.isEmpty && descs.exists(_ eq node))
      val caption: Seq[Xml.Node] = captionSource.toSeq.flatMap: node =>
        node.asElement.fold(Seq(node))(_.getChildren.filterNot(_.isWhitespace).toSeq)
      Figure.make(caption, body).setId(xmlId(element))

  private def isTeiCaption(node: Xml.Node): Boolean =
    node.asElement.exists(el => el.getName == "head" || el.getName == "tei-head")

  private def isFigDesc(node: Xml.Node): Boolean =
    node.asElement.exists(el => el.getName.equalsIgnoreCase("figDesc"))

  // Quote in TEI: <quote>, or <cit> grouping quote/q with bibl/biblStruct/ref.
  // Convert after Xml2Html so Quote IR classes are not prefixed to tei-class.
  // Bare <q> stays HTML <q> (inline). Do not invent attribution from @who/@source.
  private def convertQuote(element: Xml.Element): Xml.Element =
    element.getName match
      case "cit" => convertCit(element)
      case "quote" => convertBareQuote(element)
      case _ => element

  private def convertCit(element: Xml.Element): Xml.Element =
    val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
    val quoted: Xml.Nodes = children.filter(isQuoted)
    // cit without a quote is a bibliographic pointer, not a block quotation.
    if quoted.isEmpty then element
    else
      val body: Xml.Nodes = quoted.flatMap(unwrapQuoted)
      val attribution: Xml.Nodes = children.filter(isCitAttribution).flatMap(asAttribution)
      val id: Option[String] =
        xmlId(element).orElse(quoted.flatMap(_.asElement).find(el => xmlId(el).isDefined).flatMap(xmlId))
      Quote.make(None, attribution, body).setId(id)

  // listBibl in teiHeader / fileDesc is catalogue metadata, not the document bibliography.
  // Identity of the list element is not stable across transform copies; skip by entry xml:id.
  private def headerListBiblEntryIds(xml: Xml.Element): Set[String] =
    xml.gather(el => Option.when(el.getName == "teiHeader")(el), stopAtCode = false)
      .flatMap(header =>
        header.gather(el => Option.when(el.getName == "listBibl")(el), stopAtCode = false)
      )
      .flatMap(entryIds)
      .toSet

  private def entryIds(list: Xml.Element): Chunk[String] =
    list.getChildren.flatMap(_.asElement)
      .filter(el => BibliographyItem.isEntryName(el.getName))
      .flatMap(xmlId)

  private def listBiblIds(xml: Xml.Element, headerBiblIds: Set[String]): Set[String] =
    xml.gather(el => Option.when(el.getName == "listBibl")(el), stopAtCode = false)
      .flatMap(entryIds)
      .filterNot(headerBiblIds.contains)
      .toSet

  private def convertListBibl(element: Xml.Element, headerBiblIds: Set[String]): Xml.Element =
    if element.getName != "listBibl" || BibliographyItem.isList(element) then element
    else if entryIds(element).exists(headerBiblIds.contains) then element
    else
      val children: Xml.Nodes = element.getChildren.map: node =>
        node.asElement.filter(el => BibliographyItem.isEntryName(el.getName)) match
          case Some(entry) =>
            val withId: Xml.Element = xmlId(entry).fold(entry)(entry.setId)
            withId.add(BibliographyItem.ItemClass)
          case None =>
            node
      element.add(Citation.ListClass).setChildren(children)

  private def fillEmptyPointer(element: Xml.Element, biblIds: Set[String]): Xml.Element =
    if !element.isA || biblIds.isEmpty then element
    else
      val fragment: Option[String] = element.getHref.filter(_.startsWith("#")).map(_.substring(1))
      val empty: Boolean = element.getChildren.filterNot(_.isWhitespace).isEmpty
      fragment.filter(biblIds.contains).filter(_ => empty) match
        case Some(id) =>
          val label: String = element.get("n").map(_.trim).filter(_.nonEmpty).getOrElse(id)
          element.setText(label)
        case None =>
          element

  // Front-matter `.bib` keys: `@cRef`, or a bare `@target` that is not a native listBibl id.
  // `#id` to a listBibl entry stays an internal link (tips). Same key can be used both ways
  // (`#knuth79` vs citeproc `#bibl-knuth79`).
  private def convertCite(element: Xml.Element, biblIds: Set[String]): Xml.Element =
    if !element.isA || Citation.isCite(element) then element
    else
      val href: Option[String] = element.getHref.map(_.trim).filter(_.nonEmpty)
      val fragment: Option[String] =
        href.filter(_.startsWith("#")).map(_.substring(1)).filter(_.nonEmpty)
      if fragment.exists(biblIds.contains) then element
      else if href.exists(biblIds.contains) then
        element.setHref(s"#${href.get}")
      else
        val fromCref: Option[String] =
          element.get("cRef").orElse(element.get("cref")).map(_.trim).filter(Citation.isBibKey)
        val fromBare: Option[String] =
          href.filter(h => !h.startsWith("#") && !h.contains("/") && Citation.isBibKey(h))
        val key: Option[String] = fromCref.orElse(fromBare)
        val locator: Option[String] = element.get("n").map(_.trim).filter(_.nonEmpty)
        key.fold(element)(k =>
          Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item(k, locator)))
        )

  // Empty `div type="bibliography"` (or `tei-class="bibliography"`) is the citeproc placeholder.
  private def convertBibliographyPlaceholder(element: Xml.Element): Xml.Element =
    if Citation.isList(element) || !element.getName.equalsIgnoreCase("div") then element
    else if !isTeiBibliographyPlaceholder(element) then element
    else Citation.listPlaceholder.setId(xmlId(element))

  private def isTeiBibliographyPlaceholder(element: Xml.Element): Boolean =
    val empty: Boolean = element.getChildren.forall(_.isWhitespace)
    val typed: Boolean = element.get("type").contains("bibliography")
    val classed: Boolean =
      element.get("tei-class").exists(_.split(" ").exists(_ == "bibliography"))
    empty && (typed || classed)

  private def convertBareQuote(element: Xml.Element): Xml.Element =
    val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
    val (bibl, body): (Xml.Nodes, Xml.Nodes) = children.partition(isBibl)
    Quote.make(None, bibl.flatMap(asAttribution), body).setId(xmlId(element))

  private def isQuoted(node: Xml.Node): Boolean =
    node.asElement.exists(el => el.getName == "quote" || el.getName == "q")

  private def isBibl(node: Xml.Node): Boolean =
    node.asElement.exists(el =>
      el.getName == "bibl" || el.getName == "biblStruct" || el.getName == "listBibl"
    )

  private def isCitAttribution(node: Xml.Node): Boolean =
    isBibl(node) || node.asElement.exists(el =>
      el.getName == "ref" || el.getName == "ptr" || el.getName == "a"
    )

  private def unwrapQuoted(node: Xml.Node): Xml.Nodes =
    node.asElement.filter(el => el.getName == "quote" || el.getName == "q")
      .fold(Chunk(node))(_.getChildren.filterNot(_.isWhitespace))

  private def asAttribution(node: Xml.Node): Xml.Nodes =
    node.asElement.filter(el => el.getName == "bibl" || el.getName == "biblStruct") match
      case Some(el) =>
        Chunk(Xml.element("cite").setChildren(el.getChildren.filterNot(_.isWhitespace)))
      case None =>
        Chunk(node)

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
