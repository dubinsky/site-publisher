package org.podval.tools.publish.markup

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.page.FullMarkupPage
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

  override def rootElements: Set[String] =
    Set("TEI", "store", "collection", "entityLists") ++ EntityKind.values.map(_.element).toSet

  override def xmlContent(content: String, sourceFile: File): String = content

  // Sections in TEI:
  //<div type="section" n="2"> // chapter", "section", "part", "subsection", etc
  //  <head>Methodology</head>
  //  <p>...</p>
  //</div>
  override def pageHeader(page: FullMarkupPage): Html.Element =
    super.pageHeader(page) // TODO

  override def entityListsIndex(xml: Xml.Element): Option[EntityLists.Index] =
    EntityLists.harvest(xml)

  override def process(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    val tei2Html: Xml2Html = Xml2Html("tei")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    // Xml2Html prefixes reserved HTML attributes (`class` → `tei-class`, `lang` → `tei-lang`).
    // Convert footnotes, glossary, quotes, and code in a second pass so IR `class` values are kept.
    val converted: Xml.Element = xml.transform(
      element => convertSpecial(tei2Html.convert(element)),
      stopAtCode = false
    )
    // After Xml2Html so store `lang` is not prefixed again on already-converted names.
    val withStore: Xml.Element = converted.transform(convertStoreChrome, stopAtCode = false)
    val title: Option[Xml.Element] = documentTitle(withStore)
    val body: Xml.Element = title.fold(withStore)(stripTitle(withStore, _))
    val headerBiblIds: Set[String] = headerListBiblEntryIds(body)
    val biblIds: Set[String] = listBiblIds(body, headerBiblIds)
    // TODO does it really need to be a separate pass?
    val withIr: Xml.Element = body.transform(
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

    (markHeadedDivs(withIr), title)

  private def isTeiTitle(element: Xml.Element): Boolean =
    element.getName == "tei-title" || element.getName == "title"

  private def documentTitle(root: Xml.Element): Option[Xml.Element] =
    root.getName match
      case "TEI" =>
        pickTitle(
          root.getChildren.flatMap(_.asElement).filter(_.getName == "teiHeader")
            .flatMap(_.getChildren.flatMap(_.asElement).filter(_.getName == "fileDesc"))
            .flatMap(_.getChildren.flatMap(_.asElement).filter(_.getName == "titleStmt"))
            .flatMap(_.getChildren.flatMap(_.asElement).filter(isTeiTitle))
        )
      case "store" | "collection" | "entityLists" =>
        pickTitle(root.getChildren.flatMap(_.asElement).filter(isTeiTitle))
      case "div" if root.hasClass("store") || root.hasClass("collection") =>
        pickTitle(root.getChildren.flatMap(_.asElement).filter(isTeiTitle))
      case _ =>
        None

  private def pickTitle(candidates: Seq[Xml.Element]): Option[Xml.Element] =
    val nonempty: Seq[Xml.Element] = candidates.filter(_.getText.trim.nonEmpty)
    nonempty.find(_.get("type").contains("main")).orElse(nonempty.headOption)

  private def stripTitle(root: Xml.Element, title: Xml.Element): Xml.Element =
    root.setChildren(root.getChildren.flatMap: node =>
      if node eq title then Chunk.empty
      else node.asElement.match
        case Some(el) => Chunk(stripTitle(el, title))
        case None => Chunk(node)
    )

  // After Xml2Html, `head` is `tei-head`. Transform is parent-first, so this is a second pass.
  private[markup] def markHeadedDivs(xml: Xml.Element): Xml.Element =
    xml.transform(
      element => Section.markHeaded(element, _.getName == "tei-head"),
      stopAtCode = false
    )

  private def convertSpecial(element: Xml.Element): Xml.Element =
    val stripped: Xml.Element = dropIncludes(element)
    stripped.getName match
      case "row" =>
        renameElement("tr", stripped)

      case "cell" =>
        renameElement("td", copyAttribute("cols", "colspan", stripped))

      case "graphic" =>
        renameElement("img", copyAttribute("url", "src", stripped))

      case "ref" | "ptr" =>
        teiHref(stripped).fold(renameElement("a", stripped))(value =>
          renameElement("a", stripped.setHref(value))
        )

      case "term" =>
        teiHref(stripped).fold(stripped)(value => renameElement("a", stripped.setHref(value)))

      case name if isEntityName(name) =>
        val ref: Option[String] = stripped.get("ref").map(_.trim).filter(_.nonEmpty)
        ref.fold(stripped)(_ => renameElement("a", copyAttribute("ref", "href", stripped)))

      case name if isEntityList(name) =>
        convertEntityList(stripped)

      case "pb" =>
        // TODO convert 'n' attribute?
        renameElement("a", stripped.setText(facsimileSymbol))

      case _ =>
        stripped

  private def convertStoreChrome(element: Xml.Element): Xml.Element = element.localName match
    case "store" | "collection" => convertStore(element)
    case "by" => convertBy(element)
    case _ => element

  private def convertStore(element: Xml.Element): Xml.Element =
    val kids: Xml.Nodes = element.getChildren.map: node =>
      node.asElement match
        case Some(el) if el.localName == "name" => convertName(el)
        case Some(el) if el.localName == "by" => convertBy(el)
        case _ => node
    renameElement("div", element.setChildren(kids))

  private def convertBy(element: Xml.Element): Xml.Element =
    val selector: Option[String] = element.get("selector").map(_.trim).filter(_.nonEmpty)
    val heading: Xml.Nodes = selector.fold(Chunk.empty[Xml.Node]): s =>
      Chunk(Xml.element("em").setText(s"$s:"))
    var by: Xml.Element = element
      .rename("div")
      .addClass("store-by")
      .set("selector", "")
      .setChildren(heading)
    selector.foreach(s => by = by.set("data-selector", s))
    by

  private def convertName(element: Xml.Element): Xml.Element =
    val n: Option[String] = element.get("n").map(_.trim).filter(_.nonEmpty)
    val text: String = n.getOrElse(element.getText.trim)
    val lang: Option[String] =
      element.get("lang").orElse(element.get("tei-lang")).map(_.trim).filter(_.nonEmpty)
    var span: Xml.Element = element.rename("span").addClass("store-name").set("n", "").set("tei-lang", "")
    if text.nonEmpty then span = span.setText(text)
    lang.foreach: value =>
      span = span.set("lang", value)
    span

  private def dropIncludes(element: Xml.Element): Xml.Element =
    element.setChildren(element.getChildren.filterNot(node => node.asElement.exists(isInclude)))

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
      val caption: Seq[Xml.Node] = captionSource.flatMap: node =>
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
      .filter(isBibliographyEntry)
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
        node.asElement.filter(isBibliographyEntry) match
          case Some(entry) =>
            val withId: Xml.Element = xmlId(entry).fold(entry)(entry.setId)
            withId.add(BibliographyItem.ItemClass)
          case None =>
            node
      element.add(Citation.ListClass).setChildren(children)

  private def isBibliographyEntry(element: Xml.Element): Boolean =
    val name: String = element.getName
    name == "bibl" || name == "biblStruct" || name == "biblFull"

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

  private def isEntityName(name: String): Boolean =
    EntityKind.values.exists(_.nameElement == name)

  private def isEntityList(name: String): Boolean =
    EntityKind.values.exists(_.listElement == name)

  private def convertEntityList(element: Xml.Element): Xml.Element =
    val withId: Xml.Element =
      element.get("n").map(_.trim).filter(_.nonEmpty).fold(element)(element.setId)
    withId.setChildren(withId.getChildren.map: node =>
      node.asElement match
        case Some(el) if isTeiTitle(el) => el.rename("tei-head")
        case _ => node
    )

  private def isEntityNameLink(element: Xml.Element): Boolean =
    EntityKind.values.exists(kind => element.hasClass(kind.nameElement))

  // Front-matter `.bib` keys: `@cRef`, or a bare `@target` that is not a native listBibl id.
  // `#id` to a listBibl entry stays an internal link (tips). Same key can be used both ways
  // (`#knuth79` vs citeproc `#bibl-knuth79`).
  // Entity `@ref` on persName/placeName/orgName is a filename, not a bib key.
  private def convertCite(element: Xml.Element, biblIds: Set[String]): Xml.Element =
    if !element.isA || Citation.isCite(element) || isEntityNameLink(element) then element
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
