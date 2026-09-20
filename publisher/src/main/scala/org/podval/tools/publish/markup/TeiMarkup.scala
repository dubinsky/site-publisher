package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, Xml2Html, XmlAst, XmlAttribute, XmlElement}
import java.io.File

object TeiMarkup extends Markup(
  name = "TEI",
  xmlWriterConfig = TeiXmlWriterConfig,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  private[publish] val tei2Html: Xml2Html = Xml2Html("tei")

  override def rootElements: Set[String] =
    Set("TEI", "store", "collection", "entityLists") ++ EntityKind.values.map(_.element).toSet

  def isStoreRoot(element: Xml.Element): Boolean =
    element.isNamed("store") || element.isNamed("collection")

  /** First-parse TEI source checks. `namesWithoutRef` is for documents and store
    * title/abstract/body, not entity-file name elements (those are the definition). */
  private[publish] def reportSourceErrors(
    xml: Xml.Element,
    errorReporter: PageErrorReporter,
    namesWithoutRef: Boolean
  ): Unit =
    if namesWithoutRef then
      xml.gather(
        el =>
          Option.when(
            EntityKind.forNameElement(el.getName.localName).isDefined &&
            el.get("ref").map(_.trim).forall(_.isEmpty)
          )(el.getText),
        stopAtCode = false
      ).foreach(text => errorReporter.error(PageError.NoRef, text))
    xml.gather(
      el => Option.when(el.isNamed("unclear"))(el.getText),
      stopAtCode = false
    ).foreach(text => errorReporter.error(PageError.Unclear, text))

  private[publish] def expectedEntityFileName(displayName: String): String =
    displayName.replace(' ', '_')

  override def xmlContent(content: String, sourceFile: File): String = content

  // Sections in TEI:
  //<div type="section" n="2"> // chapter", "section", "part", "subsection", etc
  //  <head>Methodology</head>
  //  <p>...</p>
  //</div>
  override def process(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    // Convert footnotes, glossary, quotes, pb, and code in a second pass so IR `class` values are kept.
    val converted: Xml.Element = xml.transform(
      element => convertSpecial(tei2Html.convert(element), errorReporter),
      stopAtCode = false
    )
    // Title while the root is still `store` / `collection`. Header chrome is
    // `PageHeader.collectorPageHeader`; the store/collection tree is emptied.
    val title: Option[Xml.Element] = documentTitle(converted)
    val withoutTitle: Xml.Element = title.fold(converted)(stripTitle(converted, _))
    val body: Xml.Element = withoutTitle.transform(convertStoreChrome, stopAtCode = false)
    val headerBiblIds: Set[String] = headerListBiblEntryIds(body)
    val biblIds: Set[String] = listBiblIds(body, headerBiblIds)
    // TODO does it really need to be a separate pass?
    val withIr: Xml.Element = body.transform(
      element =>
        var result: Xml.Element = element.setChildren(
          element.getChildren.convertElements(convertFootnote(_, footnoteCorrelationIds))
        )
        result = convertGlossary(result).getOrElse(result)
        result = convertListBibl(result, headerBiblIds)
        result = convertCite(result, biblIds)
        result = fillEmptyPointer(result, biblIds)
        result = convertBibliographyPlaceholder(result)
        result = convertQuote(result)
        result = convertFigure(result)
        result = convertPb(result)
        result = TeiGap.convert(result)
        // Do not re-wrap `<code>` already inside `<pre>`.
        if !result.isNamed("pre") then
          result = result.setChildren(result.getChildren.convertElements(convertCode))
        result,
      stopAtCode = false
    )

    (markHeadedDivs(withIr), title)

  private def isTeiTitle(element: Xml.Element): Boolean =
    tei2Html.is(element, XmlElement.Title)

  private def documentTitle(root: Xml.Element): Option[Xml.Element] =
    root.getName.qName match
      case "TEI" =>
        pickTitle(
          root.getChildren.flatMap(_.asElement).filter(_.isNamed("teiHeader"))
            .flatMap(_.getChildren.flatMap(_.asElement).filter(_.isNamed("fileDesc")))
            .flatMap(_.getChildren.flatMap(_.asElement).filter(_.isNamed("titleStmt")))
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
    nonempty.find(_.get(XmlAttribute.Type).contains("main")).orElse(nonempty.headOption)

  private def stripTitle(root: Xml.Element, title: Xml.Element): Xml.Element =
    root.setChildren(root.getChildren.flatMapNodes(node =>
      if node eq title then Seq.empty
      else node.asElement.match
        case Some(el) => Seq(stripTitle(el, title))
        case None => Seq(node)
    ))

  // Transform is parent-first, so this is a second pass after convert.
  private[markup] def markHeadedDivs(xml: Xml.Element): Xml.Element =
    xml.transform(
      element => Section.markHeaded(element, tei2Html.is(_, XmlElement.Head)),
      stopAtCode = false
    )

  private def convertSpecial(element: Xml.Element, errorReporter: PageErrorReporter): Xml.Element =
    val stripped: Xml.Element = dropIncludes(element)
    stripped.getName.qName match
      case "row" =>
        stripped.renameKeepingClass("tr")

      case "cell" =>
        stripped.copyAttribute("cols", "colspan").renameKeepingClass("td")

      case "graphic" =>
        stripped.copyAttribute("url", "src").renameKeepingClass("img")

      case "ref" | "ptr" =>
        teiHref(stripped).fold(stripped.renameKeepingClass("a"))(value =>
          stripped.setHref(value).renameKeepingClass("a")
        )

      case "term" =>
        teiHref(stripped).fold(stripped)(value => stripped.setHref(value).renameKeepingClass("a"))

      case "date" =>
        TeiDate.convert(stripped, errorReporter)

      case name if isEntityName(name) =>
        val ref: Option[String] = stripped.get("ref").map(_.trim).filter(_.nonEmpty)
        ref.fold(stripped)(_ => stripped.copyAttribute("ref", "href").renameKeepingClass("a"))

      case name if isEntityList(name) =>
        convertEntityList(stripped)

      case _ =>
        stripped

  /** Xml2Html + TEI specials for a store header fragment (`title`, `abstract`).
    * `TeiGap` after Xml2Html so `gap-tip` `class` is not rewritten to `tei-class`. */
  private[publish] def convertFragment(xml: Xml.Element, errorReporter: PageErrorReporter): Xml.Element =
    xml.transform(
      element => convertSpecial(tei2Html.convert(element), errorReporter),
      stopAtCode = false
    ).transform(TeiGap.convert, stopAtCode = false)

  /** Convert `note place="end"` in an already-assembled fragment tree (one id sequence),
    * then harvest, number, and append the list. */
  private[publish] def finishFootnotes(xml: Xml.Element, report: PageErrorReporter): Xml.Element =
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")
    val converted: Xml.Element = xml.transform(
      element => element.setChildren(
        element.getChildren.convertElements(convertFootnote(_, footnoteCorrelationIds))
      ),
      stopAtCode = false
    )
    Footnote.finish(converted, report)

  private def convertStoreChrome(element: Xml.Element): Xml.Element = element.getName.localName match
    case "store" | "collection" => element.setChildren(Seq.empty)
    case _ => element

  private def dropIncludes(element: Xml.Element): Xml.Element =
    element.setChildren(element.getChildren.filterNot(node => node.asElement.exists(_.isInclude)))

  private def teiHref(element: Xml.Element): Option[String] =
    element.getHref
      .orElse(element.get("ref"))
      .orElse(tei2Html.get(element, XmlAttribute.Target))

  private def xmlId(element: Xml.Element): Option[String] =
    element.getId.filter(_.nonEmpty).orElse(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))

  // After Xml2Html so `a.pb` / icon `class` is not prefixed to `tei-class`.
  // `TeiGap.convert` is in this pass for the same reason (`span.gap-tip`).
  private def convertPb(element: Xml.Element): Xml.Element =
    if !element.isNamed("pb") then element
    else
      val n: Option[String] = element.get("n").map(_.trim).filter(_.nonEmpty)
      Pb.anchor(n)

  // Figure in TEI: <figure> with <graphic url> (already <img>) and optional <head>/<figDesc>.
  // Convert after Xml2Html so Figure IR classes are not prefixed to tei-class.
  private def convertFigure(element: Xml.Element): Xml.Element =
    if !element.isNamed("figure") || Figure.is(element) then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val heads: Xml.Nodes = children.filter(isTeiCaption)
      val descs: Xml.Nodes = children.filter(isFigDesc)
      val captionSource: Xml.Nodes = if heads.nonEmpty then heads else descs
      val body: Xml.Nodes =
        children.filterNot: node =>
          heads.exists(_ eq node) || (heads.isEmpty && descs.exists(_ eq node))
      val caption: Xml.Nodes = captionSource.flatMap: node =>
        node.asElement.fold(Seq(node))(_.getChildren.filterNot(_.isWhitespace).toSeq)
      Figure.make(caption, body).setId(xmlId(element))

  private def isTeiCaption(node: Xml.Node): Boolean =
    node.asElement.exists(tei2Html.is(_, XmlElement.Head))

  private def isFigDesc(node: Xml.Node): Boolean =
    node.asElement.exists(el => el.getName.qName.equalsIgnoreCase("figDesc"))

  // Quote in TEI: <quote>, or <cit> grouping quote/q with bibl/biblStruct/ref.
  // Convert after Xml2Html so Quote IR classes are not prefixed to tei-class.
  // Bare <q> stays HTML <q> (inline). Do not invent attribution from @who/@source.
  private def convertQuote(element: Xml.Element): Xml.Element =
    element.getName.qName match
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
    xml.gather(el => Option.when(el.isNamed("teiHeader"))(el), stopAtCode = false)
      .flatMap(header =>
        header.gather(el => Option.when(el.isNamed("listBibl"))(el), stopAtCode = false)
      )
      .flatMap(entryIds)
      .toSet

  private def entryIds(list: Xml.Element): Seq[String] =
    list.getChildren.flatMap(_.asElement)
      .filter(isBibliographyEntry)
      .flatMap(xmlId)

  private def listBiblIds(xml: Xml.Element, headerBiblIds: Set[String]): Set[String] =
    xml.gather(el => Option.when(el.isNamed("listBibl"))(el), stopAtCode = false)
      .flatMap(entryIds)
      .filterNot(headerBiblIds.contains)
      .toSet

  private def convertListBibl(element: Xml.Element, headerBiblIds: Set[String]): Xml.Element =
    if !element.isNamed("listBibl") || BibliographyItem.isList(element) then element
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
    val name: String = element.getName.qName
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
        case Some(el) if isTeiTitle(el) => el.rename(tei2Html.elementName(XmlElement.Head))
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

  // Empty `div type="bibliography"` (or reserved `class="bibliography"`) is the citeproc placeholder.
  private def convertBibliographyPlaceholder(element: Xml.Element): Xml.Element =
    if Citation.isList(element) || !element.getName.qName.equalsIgnoreCase("div") then element
    else if !isTeiBibliographyPlaceholder(element) then element
    else Citation.listPlaceholder.setId(xmlId(element))

  private def isTeiBibliographyPlaceholder(element: Xml.Element): Boolean =
    val empty: Boolean = element.getChildren.forall(_.isWhitespace)
    val typed: Boolean = element.get(XmlAttribute.Type).contains("bibliography")
    val classed: Boolean =
      tei2Html.get(element, XmlAttribute.CssClass).exists(_.split(" ").exists(_ == "bibliography"))
    empty && (typed || classed)

  private def convertBareQuote(element: Xml.Element): Xml.Element =
    val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
    val (bibl: Xml.Nodes, body: Xml.Nodes) = children.partition(isBibl)
    Quote.make(None, bibl.flatMap(asAttribution), body).setId(xmlId(element))

  private def isQuoted(node: Xml.Node): Boolean =
    node.asElement.exists(el => el.isNamed("quote") || el.isNamed("q"))

  private def isBibl(node: Xml.Node): Boolean =
    node.asElement.exists(el =>
      el.isNamed("bibl") || el.isNamed("biblStruct") || el.isNamed("listBibl")
    )

  private def isCitAttribution(node: Xml.Node): Boolean =
    isBibl(node) || node.asElement.exists(el =>
      el.isNamed("ref") || el.isNamed("ptr") || el.isA
    )

  private def unwrapQuoted(node: Xml.Node): Xml.Nodes =
    node.asElement.filter(el => el.isNamed("quote") || el.isNamed("q"))
      .fold(Seq(node))(_.getChildren.filterNot(_.isWhitespace))

  private def asAttribution(node: Xml.Node): Xml.Nodes =
    node.asElement.filter(el => el.isNamed("bibl") || el.isNamed("biblStruct")) match
      case Some(el) =>
        Seq(Xml.element("cite").setChildren(el.getChildren.filterNot(_.isWhitespace)))
      case None =>
        Seq(node)

  // Glossary in TEI: <list type="gloss"> (also type="glossary") of <label>/<item>.
  // Convert after Xml2Html so Glossary IR classes are not prefixed to tei-class.
  private def convertGlossary(element: Xml.Element): Option[Xml.Element] =
    val isGlossList: Boolean =
      element.isNamed("list") &&
      element.get(XmlAttribute.Type).exists(t => t == "gloss" || t == "glossary")
    if !isGlossList then None
    else Some:
      element.renameKeepingClass("dl")
        .setChildren(groupGlossEntries(element.getChildren))
        .add(Glossary.ListClass)

  private def groupGlossEntries(nodes: Xml.Nodes): Xml.Nodes =
    var result: Xml.Nodes = Nil
    var pendingLabel: Option[Xml.Element] = None

    def asDt(label: Xml.Element): Xml.Element =
      Xml.element(XmlElement.Dt).setChildren(label.getChildren.filterNot(_.isWhitespace))

    def asDd(item: Xml.Element): Xml.Element =
      Xml.element(XmlElement.Dd).setChildren(item.getChildren.filterNot(_.isWhitespace))

    def emit(label: Xml.Element, item: Option[Xml.Element]): Unit =
      val dt: Xml.Element = asDt(label)
      val dd: Option[Xml.Element] = item.map(asDd)
      val id: Option[String] =
        xmlId(label).orElse(item.flatMap(xmlId)).orElse:
          val text: String = dt.getText.trim
          Option.when(text.nonEmpty)(XmlAst.toId(text))
      result = result :+ Glossary.item(id, dt +: dd.toSeq)

    nodes.foreach: node =>
      node.asElement match
        case Some(label) if label.isNamed("label") =>
          pendingLabel.foreach(emit(_, None))
          pendingLabel = Some(label)
        case Some(item) if item.isNamed("item") =>
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
    result

  // Code in TEI: <code lang="scala"> (tagdocs). `code` is not reserved; @lang is.
  // Inline stays <code class="language-…">; a newline means a block, wrapped in <pre>.
  // Do not convert <eg> / <egXML>.
  private def convertCode(element: Xml.Element): Option[Xml.Nodes] =
    if !element.isNamed("code") then None
    else
      val lang: Option[String] =
        tei2Html.get(element, XmlAttribute.Lang).map(_.trim).filter(_.nonEmpty)
      var code: Xml.Element = element
      lang.foreach: name =>
        val cls: String = s"language-${name.toLowerCase}"
        if !code.hasClass(cls) then code = code.addClass(cls)
      val wrapped: Xml.Element =
        if code.getText.contains('\n') then Xml.element(XmlElement.Pre).setChildren(Seq(code))
        else code
      Some(Seq(wrapped))

  // Footnotes in TEI:
  // <note place="end" n="3">Footnote body</note>
  // TODO do not ignore n?
  private def convertFootnote(element: Xml.Element, correlationIds: IdGenerator): Option[Xml.Nodes] =
    val isFootnote: Boolean = element.isNamed("note") && element.get("place").contains("end")
    if !isFootnote then None else Some:
      val correlationId = correlationIds.generate()
      Seq(
        Footnote.link(correlationId),
        Footnote.body(correlationId, element.getChildren)
      )
