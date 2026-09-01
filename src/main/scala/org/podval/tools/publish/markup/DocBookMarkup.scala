package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, Xml2Html, XmlAttribute, XmlUtil}
import org.podval.xml.XmlUtil.*
import zio.blocks.chunk.Chunk

import java.io.File

object DocBookMarkup extends Markup(
  name = "DocBook",
  xmlDialect = DocBookXmlDialect,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  override def rootElements: Set[String] = Set(
    "article", "book", "chapter", "appendix", "part", "set", "preface", "refentry", "topic"
  )

  private val sectionElements: Set[String] = Set(
    "section", "sect1", "sect2", "sect3", "sect4", "sect5", "simplesect",
    "chapter", "appendix", "preface"
  )

  private val linkElements: Set[String] = Set(
    "link", "ulink", "xref", "olink", "biblioref", "footnoteref"
  )

  private val infoElements: Set[String] = Set(
    "info", "articleinfo", "bookinfo", "chapterinfo", "appendixinfo", "prefaceinfo",
    "partinfo", "setinfo", "refentryinfo"
  )

  private val wrapperElements: Set[String] = Set(
    "tgroup", "mediaobject", "inlinemediaobject", "imageobject", "videoobject", "audioobject"
  )

  private val admonitionTypes: Set[String] = Set("note", "tip", "warning", "caution", "important")

  override def xmlContent(content: String, sourceFile: File): String = content

  override def process(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    val db2Html: Xml2Html = Xml2Html("db")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")
    val coNumbers: IdGenerator = IdGenerator("")
    // Xml2Html prefixes reserved HTML attributes (`class` → `db-class`, `lang` → `db-lang`).
    // Convert footnotes, glossary, quotes, and code in a second pass so IR `class` values are kept.
    val converted: Xml.Element = xml.transform(
      element =>
        val renameSections: Boolean = !(element eq xml) || !rootElements.contains(xml.getName)
        convertSpecial(db2Html.convert(element), renameSections),
      stopAtCode = false
    )
    val title: Option[Xml.Element] = documentTitle(converted)
    val body: Xml.Element = title.fold(converted)(stripDocumentTitle(converted, _))
    val footnoteIds: Set[String] = footnoteDefinitionIds(body)
    val biblIds: Set[String] = bibliographyEntryIds(body)
    val withIr: Xml.Element = body.transform(
      element =>
        var result: Xml.Element = element.setChildren(
          convertElements(element.getChildren, convertFootnote(_, footnoteCorrelationIds))
        )
        result = result.setChildren(convertElements(result.getChildren, convertFootnoteRef(_, footnoteIds)))
        result = convertGlossary(result)
        result = convertVariableList(result)
        result = convertAdmonition(result)
        result = convertAside(result)
        result = convertQuote(result)
        result = convertFigure(result)
        result = convertVideo(result)
        result = convertCalloutList(result)
        result = result.setChildren(convertElements(result.getChildren, convertCo(_, coNumbers)))
        result = convertBibliography(result)
        result = convertCitation(result)
        result = convertCiteLink(result, biblIds)
        // Do not re-wrap `<code>` already inside `<pre>`.
        if result.getName != "pre" then
          result = result.setChildren(convertElements(result.getChildren, convertCode))
        result,
      stopAtCode = false
    )
    (markHeadedDivs(withIr), title)

  // After Xml2Html, `title` is `db-title`. Transform is parent-first, so this is a second pass.
  private def markHeadedDivs(xml: Xml.Element): Xml.Element =
    xml.transform(
      element => Section.markHeaded(element, _.getName == "db-title"),
      stopAtCode = false
    )

  private def isDbTitle(element: Xml.Element): Boolean = element.getName == "db-title"

  private def documentTitle(root: Xml.Element): Option[Xml.Element] =
    val children: Seq[Xml.Element] = root.getChildren.flatMap(_.asElement).toSeq
    children.find(isDbTitle).orElse:
      children.filter(el => infoElements.contains(el.getName))
        .flatMap(_.getChildren.flatMap(_.asElement).find(isDbTitle))
        .headOption

  private def stripDocumentTitle(root: Xml.Element, title: Xml.Element): Xml.Element =
    if root.getChildren.exists(_ eq title) then
      root.setChildren(root.getChildren.filterNot(_ eq title))
    else
      root.setChildren(root.getChildren.flatMap: node =>
        node.asElement.filter(el => infoElements.contains(el.getName)) match
          case Some(info) if info.getChildren.exists(_ eq title) =>
            val stripped: Xml.Element = info.setChildren(info.getChildren.filterNot(_ eq title))
            if stripped.getChildren.forall(_.isWhitespace) then Chunk.empty else Chunk(stripped)
          case _ =>
            Chunk(node)
      )

  private def xmlId(element: Xml.Element): Option[String] =
    element.getId.filter(_.nonEmpty).orElse(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))

  private def convertSpecial(element: Xml.Element, renameSections: Boolean): Xml.Element =
    val el: Xml.Element = element.setChildren(flattenWrappers(element.getChildren))
    el.getName match
      case "para" | "simpara" =>
        renameElement("p", el)

      case "emphasis" | "phrase" =>
        convertEmphasis(el)

      case "itemizedlist" =>
        renameElement("ul", el)

      case "orderedlist" =>
        renameElement("ol", el)

      case "listitem" =>
        renameElement("li", el)

      case name if linkElements.contains(name) =>
        // `renameElement` would add class `link`, which is the section permalink class.
        val linked: Xml.Element = copyLinkHref(el)
        val tagged: Xml.Element =
          if name == "link" then linked.rename("a") else renameElement("a", linked)
        fillEmptyLink(tagged)

      case "imagedata" =>
        renameElement("img", copyAttribute("fileref", "src", el))

      case "videodata" | "audiodata" =>
        copyAttribute("fileref", "src", el)

      case "row" =>
        renameElement("tr", el)

      case "entry" =>
        renameElement("td", copyMorerows(el))

      case "informaltable" =>
        renameElement("table", el)

      case "subscript" =>
        renameElement("sub", el)

      case "superscript" =>
        renameElement("sup", el)

      case "quote" =>
        renameElement("q", el)

      case name if renameSections && sectionElements.contains(name) =>
        renameElement("div", el)

      case _ =>
        el

  private def convertEmphasis(element: Xml.Element): Xml.Element =
    element.get("role").map(_.trim.toLowerCase) match
      case Some("bold") | Some("strong") =>
        renameElement("strong", element)
      case Some("strikethrough") | Some("line-through") =>
        renameElement("del", element)
      case _ if element.getName == "emphasis" =>
        renameElement("em", element)
      case _ =>
        element

  private def copyLinkHref(element: Xml.Element): Xml.Element =
    dbHref(element).fold(element)(element.setHref)

  // Xml2Html prefixes reserved HTML attributes (`target` → `db-target`).
  private def dbHref(element: Xml.Element): Option[String] =
    element.getHref.map(_.trim).filter(_.nonEmpty)
      .orElse(element.get("xlink:href").map(_.trim).filter(_.nonEmpty))
      .orElse(element.get("url").map(_.trim).filter(_.nonEmpty))
      .orElse(element.get("linkend").map(_.trim).filter(_.nonEmpty).map(asHrefFragment))

  private def asHrefFragment(value: String): String =
    if value.startsWith("#") then value else s"#$value"

  private def fillEmptyLink(element: Xml.Element): Xml.Element =
    if !element.isA || element.getChildren.filterNot(_.isWhitespace).nonEmpty then element
    else
      val label: Option[String] =
        element.get("xreflabel").map(_.trim).filter(_.nonEmpty)
          .orElse(element.getHref.filter(_.startsWith("#")).map(_.substring(1)).filter(_.nonEmpty))
      label.fold(element)(element.setText)

  private def copyMorerows(element: Xml.Element): Xml.Element =
    element.get("morerows").map(_.trim).flatMap(_.toIntOption) match
      case Some(n) if n > 0 => element.set("rowspan", (n + 1).toString)
      case _ => element

  // CALS `tgroup` is not HTML; mediaobject wrappers are not either.
  private def flattenWrappers(nodes: Xml.Nodes): Xml.Nodes =
    nodes.flatMap: node =>
      node.asElement match
        case Some(el) if wrapperElements.contains(el.getName) => flattenWrappers(el.getChildren)
        case Some(el) if el.getName == "colspec" || el.getName == "spanspec" => Chunk.empty
        case _ => Chunk(node)

  private def convertFootnote(element: Xml.Element, correlationIds: IdGenerator): Option[Xml.Nodes] =
    if element.getName != "footnote" then None
    else
      val correlationId: String = xmlId(element).getOrElse(correlationIds.generate())
      Some(Chunk(Footnote.link(correlationId), Footnote.body(correlationId, element.getChildren)))

  private def footnoteDefinitionIds(xml: Xml.Element): Set[String] =
    xml.gather(el => Option.when(el.getName == "footnote")(xmlId(el)).flatten, stopAtCode = false).toSet

  private def convertFootnoteRef(element: Xml.Element, footnoteIds: Set[String]): Option[Xml.Nodes] =
    if !element.isA || !element.hasClass("footnoteref") then None
    else
      val fragment: Option[String] =
        element.getHref.filter(_.startsWith("#")).map(_.substring(1)).filter(_.nonEmpty)
      fragment.filter(footnoteIds.contains).map(id => Chunk(Footnote.link(id)))

  private def convertGlossary(element: Xml.Element): Xml.Element =
    if !(element.getName == "glosslist" || element.getName == "glossary") || Glossary.isList(element) then
      element
    else
      val titles: Xml.Nodes = element.getChildren.filter(node => node.asElement.exists(isDbTitle))
      val entries: Chunk[Xml.Element] = element.gather(
        el => Option.when(el.getName == "glossentry")(convertGlossEntry(el)),
        stopAtCode = false
      )
      val dl: Xml.Element = Xml.element("dl").add(Glossary.ListClass).setChildren(entries)
      if titles.isEmpty then dl else Xml.element("div").setChildren(titles ++ Chunk(dl))

  private def convertGlossEntry(entry: Xml.Element): Xml.Element =
    val children: Seq[Xml.Element] = entry.getChildren.flatMap(_.asElement).toSeq
    val term: Option[Xml.Element] = children.find(_.getName == "glossterm")
    val definition: Option[Xml.Element] = children.find(_.getName == "glossdef")
    val dt: Xml.Element =
      Xml.element("dt").setChildren(term.fold(Chunk.empty)(_.getChildren.filterNot(_.isWhitespace)))
    val dd: Option[Xml.Element] = definition.map: defn =>
      Xml.element("dd").setChildren(defn.getChildren.filterNot(_.isWhitespace))
    val id: Option[String] =
      xmlId(entry).orElse(term.flatMap(xmlId)).orElse:
        val text: String = dt.getText.trim
        Option.when(text.nonEmpty)(XmlUtil.toId(text))
    Glossary.item(id, Chunk.from(dt +: dd.toSeq))

  private def convertVariableList(element: Xml.Element): Xml.Element =
    if element.getName != "variablelist" then element
    else
      val items: Xml.Nodes = element.getChildren.flatMap: node =>
        node.asElement.filter(_.getName == "varlistentry") match
          case Some(entry) => convertVarListEntry(entry)
          case None => Chunk(node)
      Xml.element("dl").setChildren(items)

  private def convertVarListEntry(entry: Xml.Element): Xml.Nodes =
    val children: Seq[Xml.Element] = entry.getChildren.flatMap(_.asElement).toSeq
    val dts: Seq[Xml.Element] = children.filter(_.getName == "term").map: term =>
      Xml.element("dt").setChildren(term.getChildren.filterNot(_.isWhitespace))
    val dds: Seq[Xml.Element] = children.filter(_.getName == "listitem").map: item =>
      Xml.element("dd").setChildren(item.getChildren.filterNot(_.isWhitespace))
    Chunk.from(dts ++ dds)

  private def convertAdmonition(element: Xml.Element): Xml.Element =
    if !admonitionTypes.contains(element.getName) || Admonition.is(element) then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val titleEl: Option[Xml.Element] = children.flatMap(_.asElement).find(isDbTitle)
      val body: Xml.Nodes = children.filterNot(node => titleEl.exists(title => node.asElement.contains(title)))
      val title: Option[String] = titleEl.map(_.getText.trim).filter(_.nonEmpty)
      Admonition.make(element.getName, title, body).setId(xmlId(element))

  private def convertAside(element: Xml.Element): Xml.Element =
    if element.getName != "sidebar" || Aside.is(element) then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val titleEl: Option[Xml.Element] = children.flatMap(_.asElement).find(isDbTitle)
      val body: Xml.Nodes = children.filterNot(node => titleEl.exists(title => node.asElement.contains(title)))
      val title: Option[String] = titleEl.map(_.getText.trim).filter(_.nonEmpty)
      Aside.make(title, body).setId(xmlId(element))

  private def convertQuote(element: Xml.Element): Xml.Element =
    if (element.getName != "blockquote" && element.getName != "epigraph") || Quote.is(element) then
      element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val titleEl: Option[Xml.Element] = children.flatMap(_.asElement).find(isDbTitle)
      val attribution: Xml.Nodes = children.filter(node => node.asElement.exists(_.getName == "attribution"))
      val body: Xml.Nodes = children.filterNot: node =>
        titleEl.exists(title => node.asElement.contains(title)) ||
        attribution.exists(_ eq node)
      val title: Option[String] = titleEl.map(_.getText.trim).filter(_.nonEmpty)
      val attribNodes: Xml.Nodes = attribution.flatMap: node =>
        node.asElement.fold(Chunk(node))(_.getChildren.filterNot(_.isWhitespace))
      Quote.make(title, attribNodes, body).setId(xmlId(element))

  private def convertFigure(element: Xml.Element): Xml.Element =
    if (element.getName != "figure" && element.getName != "informalfigure") || Figure.is(element) then
      element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val captions: Xml.Nodes = children.filter: node =>
        node.asElement.exists(el => isDbTitle(el) || el.getName == "caption")
      val body: Xml.Nodes = children.filterNot(node => captions.exists(_ eq node))
      val caption: Seq[Xml.Node] = captions.toSeq.flatMap: node =>
        node.asElement.fold(Seq(node))(_.getChildren.filterNot(_.isWhitespace).toSeq)
      Figure.make(caption, body).setId(xmlId(element))

  private def convertVideo(element: Xml.Element): Xml.Element =
    if element.getName != "videodata" then element
    else
      val src: Option[String] =
        element.get("src").orElse(element.get("fileref")).map(_.trim).filter(_.nonEmpty)
      src.fold(element): href =>
        if isRemoteVideo(href) then
          Xml.element("iframe").add(Video.EmbedClass).set("src", href)
        else
          val label: String = href.split('/').lastOption.getOrElse(href)
          Video.make(href, label).setId(xmlId(element))

  private def isRemoteVideo(src: String): Boolean =
    val lower: String = src.toLowerCase
    lower.contains("youtube.com") || lower.contains("youtu.be") || lower.contains("vimeo.com")

  private def convertCalloutList(element: Xml.Element): Xml.Element =
    if element.getName != "calloutlist" || Callout.isList(element) then element
    else
      val items: Xml.Nodes = element.getChildren.flatMap: node =>
        node.asElement.filter(_.getName == "callout") match
          case Some(callout) =>
            Chunk(Xml.element("li").setChildren(callout.getChildren.filterNot(_.isWhitespace)))
          case None =>
            Chunk(node)
      Xml.element("ol").add(Callout.ListClass).setChildren(items)

  private def convertCo(element: Xml.Element, coNumbers: IdGenerator): Option[Xml.Nodes] =
    if element.getName != "co" then None
    else
      val number: String =
        element.get("label").map(_.trim).filter(_.nonEmpty).getOrElse(coNumbers.generate())
      Some(Chunk(Callout.marker(number)))

  private def bibliographyEntryIds(xml: Xml.Element): Set[String] =
    xml.gather(
      el => Option.when(el.getName == "bibliography")(el),
      stopAtCode = false
    ).flatMap(entryIds).toSet

  private def entryIds(list: Xml.Element): Chunk[String] =
    list.getChildren.flatMap(_.asElement)
      .filter(isBibliographyEntry)
      .flatMap(xmlId)

  private def convertBibliography(element: Xml.Element): Xml.Element =
    if element.getName != "bibliography" || BibliographyItem.isList(element) || Citation.isList(element) then
      element
    else if entryIds(element).isEmpty then
      Citation.listPlaceholder.setId(xmlId(element))
    else
      val children: Xml.Nodes = element.getChildren.map: node =>
        node.asElement.filter(isBibliographyEntry) match
          case Some(entry) =>
            val withId: Xml.Element = entry.copyXmlId
            withId.getId.filter(_.nonEmpty).fold(withId)(_ => withId.add(BibliographyItem.ItemClass))
          case None => node
      element.add(Citation.ListClass).setChildren(children)

  private def isBibliographyEntry(element: Xml.Element): Boolean =
    val name: String = element.getName
    name == "biblioentry" || name == "bibliomixed"

  private def convertCitation(element: Xml.Element): Xml.Element =
    if element.getName != "citation" || Citation.isCite(element) then element
    else
      val raw: String = element.getText.trim
      val comma: Int = raw.indexOf(',')
      val (key: String, locator: Option[String]) =
        if comma < 0 then (raw, None)
        else (raw.substring(0, comma).trim, Some(raw.substring(comma + 1).trim).filter(_.nonEmpty))
      if !Citation.isBibKey(key) then element
      else Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item(key, locator)))

  private def convertCiteLink(element: Xml.Element, biblIds: Set[String]): Xml.Element =
    if !element.isA || Citation.isCite(element) || !element.hasClass("biblioref") then element
    else
      val fragment: Option[String] =
        element.getHref.filter(_.startsWith("#")).map(_.substring(1)).filter(_.nonEmpty)
      if fragment.exists(biblIds.contains) then element
      else
        val key: Option[String] = fragment.filter(Citation.isBibKey)
        val locator: Option[String] = element.get("xrefstyle").map(_.trim).filter(_.nonEmpty)
        key.fold(element)(k => Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item(k, locator))))

  private def convertCode(element: Xml.Element): Option[Xml.Nodes] =
    val language: Option[String] =
      element.get("language").map(_.trim).filter(_.nonEmpty)
    element.getName match
      case "literal" =>
        Some(Chunk(withLanguage(renameElement("code", element), language)))
      case "code" =>
        Some(Chunk(wrapIfMultiline(withLanguage(element, language))))
      case "programlisting" | "screen" | "literallayout" =>
        val code: Xml.Element = withLanguage(Xml.element("code").setChildren(element.getChildren), language)
        Some(Chunk(Xml.element("pre").setChildren(Chunk(code))))
      case _ =>
        None

  private def withLanguage(element: Xml.Element, language: Option[String]): Xml.Element =
    language.fold(element): name =>
      val cls: String = s"language-${name.toLowerCase}"
      if element.hasClass(cls) then element else element.addClass(cls)

  private def wrapIfMultiline(code: Xml.Element): Xml.Element =
    if code.getText.contains('\n') then Xml.element("pre").setChildren(Chunk(code)) else code
