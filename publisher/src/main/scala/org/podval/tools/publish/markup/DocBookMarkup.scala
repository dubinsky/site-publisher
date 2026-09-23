package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, Xml2Html, XmlAst, XmlAttribute, XmlElement}
import Xml.given
import java.io.File

object DocBookMarkup extends Markup(
  name = "DocBook",
  xmlWriterConfig = DocBookXmlWriterConfig,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  private[publish] val db2Html: Xml2Html = Xml2Html("db")

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
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")
    val coNumbers: IdGenerator = IdGenerator("")
    // Convert footnotes, glossary, quotes, and code in a second pass so IR `class` values are kept.
    val converted: Xml.Element = xml.transform(
      element =>
        val renameSections: Boolean = !(element eq xml) || !xml.getName.localNameIn(rootElements)
        convertSpecial(db2Html.convert(element), renameSections),
      stopAtCode = false
    )
    val title: Option[Xml.Element] = documentTitle(converted)
    val body: Xml.Element = title.fold(converted)(stripDocumentTitle(converted, _))
    val footnoteIds: Set[String] = footnoteDefinitionIds(body)
    val biblIds: Set[String] = bibliographyEntryIds(body)
    val withIr: Xml.Element = body.rewrite(
      (element, parent) =>
        convertFootnote(element, footnoteCorrelationIds)
          .orElse(convertFootnoteRef(element, footnoteIds))
          .orElse(convertCo(element, coNumbers)) match
          case Some(nodes) =>
            Xml.Rewrite.Replace(nodes)
          case None =>
            var result: Xml.Element = element
            result = convertGlossary(result)
            result = convertVariableList(result)
            result = convertAdmonition(result)
            result = convertAside(result)
            result = convertQuote(result)
            result = convertFigure(result)
            result = convertVideo(result)
            result = convertCalloutList(result)
            result = convertBibliography(result)
            result = convertCitation(result)
            result = convertCiteLink(result, biblIds)
            // Do not re-wrap `<code>` already inside `<pre>`.
            if parent.exists(_.isNamed("pre")) then Xml.Rewrite.Keep(result)
            else convertCode(element) match
              case Some(nodes) => rewriteCode(nodes)
              case None => Xml.Rewrite.Keep(result)
      ,
      stopAtCode = false
    )
    (markHeadedDivs(withIr), title)

  // Transform is parent-first, so this is a second pass after convert.
  private def markHeadedDivs(xml: Xml.Element): Xml.Element =
    xml.transform(
      // TODO use isDbTitle
      element => Section.markHeaded(element, db2Html.is(_, XmlElement.Title)),
      stopAtCode = false
    )

  private def isDbTitle(element: Xml.Element): Boolean =
    db2Html.is(element, XmlElement.Title)

  private def documentTitle(root: Xml.Element): Option[Xml.Element] =
    val children: Seq[Xml.Element] = root.getChildren.flatMap(_.asElement)
    children.find(isDbTitle).orElse:
      children.filter(el => el.getName.localNameIn(infoElements))
        .flatMap(_.getChildren.flatMap(_.asElement).find(isDbTitle))
        .headOption

  private def stripDocumentTitle(root: Xml.Element, title: Xml.Element): Xml.Element =
    if root.getChildren.exists(_ eq title) then
      root.setChildren(root.getChildren.filterNot(_ eq title))
    else
      root.setChildren(root.getChildren.flatMapNodes(node =>
        node.asElement.filter(el => el.getName.localNameIn(infoElements)) match
          case Some(info) if info.getChildren.exists(_ eq title) =>
            val stripped: Xml.Element = info.setChildren(info.getChildren.filterNot(_ eq title))
            if stripped.getChildren.forall(_.isWhitespace) then Seq.empty else Seq(stripped)
          case _ =>
            Seq(node)
      ))

  private def xmlId(element: Xml.Element): Option[String] =
    element.getId.filter(_.nonEmpty).orElse(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))

  private def convertSpecial(element: Xml.Element, renameSections: Boolean): Xml.Element =
    val el: Xml.Element = element.setChildren(flattenWrappers(element.getChildren))
    el.getName.qName match
      case "para" | "simpara" =>
        el.renameKeepingClass("p")

      case "emphasis" | "phrase" =>
        convertEmphasis(el)

      case "itemizedlist" =>
        el.renameKeepingClass("ul")

      case "orderedlist" =>
        el.renameKeepingClass("ol")

      case "listitem" =>
        el.renameKeepingClass("li")

      case name if linkElements.contains(name) =>
        // `renameKeepingClass` would add class `link`, which is the section permalink class.
        val linked: Xml.Element = copyLinkHref(el)
        val tagged: Xml.Element =
          if name == "link" then linked.rename("a") else linked.renameKeepingClass("a")
        fillEmptyLink(tagged)

      case "imagedata" =>
        el.copyAttribute("fileref", "src").renameKeepingClass("img")

      case "videodata" | "audiodata" =>
        el.copyAttribute("fileref", "src")

      case "row" =>
        el.renameKeepingClass("tr")

      case "entry" =>
        copyMorerows(el).renameKeepingClass("td")

      case "informaltable" =>
        el.renameKeepingClass("table")

      case "subscript" =>
        el.renameKeepingClass("sub")

      case "superscript" =>
        el.renameKeepingClass("sup")

      case "quote" =>
        el.renameKeepingClass("q")

      case name if renameSections && sectionElements.contains(name) =>
        el.renameKeepingClass("div")

      case _ =>
        el

  private def convertEmphasis(element: Xml.Element): Xml.Element =
    element.get(XmlAttribute.Role).map(_.trim.toLowerCase) match
      case Some("bold") | Some("strong") =>
        element.renameKeepingClass("strong")
      case Some("strikethrough") | Some("line-through") =>
        element.renameKeepingClass("del")
      case _ if element.isNamed("emphasis") =>
        element.renameKeepingClass("em")
      case _ =>
        element

  private def copyLinkHref(element: Xml.Element): Xml.Element =
    dbHref(element).fold(element)(element.setHref)

  private def dbHref(element: Xml.Element): Option[String] =
    element.getHref.map(_.trim).filter(_.nonEmpty)
      .orElse(element.get("xlink:href").map(_.trim).filter(_.nonEmpty))
      .orElse(element.get("url").map(_.trim).filter(_.nonEmpty))
      .orElse(element.get("linkend").map(_.trim).filter(_.nonEmpty).map(asHrefFragment))

  private def asHrefFragment(value: String): String =
    if value.startsWith("#") then value else s"#$value"

  private def fillEmptyLink(element: Xml.Element): Xml.Element =
    if !element.isA || !element.getChildren.forall(_.isWhitespace) then element
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
        case Some(el) if el.getName.localNameIn(wrapperElements) => flattenWrappers(el.getChildren)
        case Some(el) if el.isNamed("colspec") || el.isNamed("spanspec") => Seq.empty
        case _ => Seq(node)

  private def convertFootnote(element: Xml.Element, correlationIds: IdGenerator): Option[Xml.Nodes] =
    if !element.isNamed("footnote") then None
    else
      val correlationId: String = xmlId(element).getOrElse(correlationIds.generate())
      Some(Seq(Footnote.link(correlationId), Footnote.body(correlationId, element.getChildren)))

  private def footnoteDefinitionIds(xml: Xml.Element): Set[String] =
    xml.gather(el => Option.when(el.isNamed("footnote"))(xmlId(el)).flatten, stopAtCode = false).toSet

  private def convertFootnoteRef(element: Xml.Element, footnoteIds: Set[String]): Option[Xml.Nodes] =
    if !element.isA || !element.hasClass("footnoteref") then None
    else
      val fragment: Option[String] =
        element.getHref.filter(_.startsWith("#")).map(_.substring(1)).filter(_.nonEmpty)
      fragment.filter(footnoteIds.contains).map(id => Seq(Footnote.link(id)))

  private def convertGlossary(element: Xml.Element): Xml.Element =
    if !(element.isNamed("glosslist") || element.isNamed("glossary")) || Glossary.isList(element) then
      element
    else
      val titles: Xml.Nodes = element.getChildren.filter(node => node.asElement.exists(isDbTitle))
      val entries: Seq[Xml.Element] = element.gather(
        el => Option.when(el.isNamed("glossentry"))(convertGlossEntry(el)),
        stopAtCode = false
      )
      val dl: Xml.Element = Xml.element(XmlElement.Dl).add(Glossary.ListClass).setChildren(entries)
      if titles.isEmpty then dl else Xml.element(XmlElement.Div).setChildren(titles ++ Seq(dl))

  private def convertGlossEntry(entry: Xml.Element): Xml.Element =
    val children: Seq[Xml.Element] = entry.getChildren.flatMap(_.asElement)
    val term: Option[Xml.Element] = children.find(_.isNamed("glossterm"))
    val definition: Option[Xml.Element] = children.find(_.isNamed("glossdef"))
    val dt: Xml.Element =
      Xml.element(XmlElement.Dt).setChildren(term.fold(Seq.empty)(_.getChildren.filterNot(_.isWhitespace)))
    val dd: Option[Xml.Element] = definition.map: defn =>
      Xml.element(XmlElement.Dd).setChildren(defn.getChildren.filterNot(_.isWhitespace))
    val id: Option[String] =
      xmlId(entry).orElse(term.flatMap(xmlId)).orElse:
        val text: String = dt.getText.trim
        Option.when(text.nonEmpty)(XmlAst.toId(text))
    Glossary.item(id, dt +: dd.toSeq)

  private def convertVariableList(element: Xml.Element): Xml.Element =
    if !element.isNamed("variablelist") then element
    else
      val items: Xml.Nodes = element.getChildren.flatMap: node =>
        node.asElement.filter(_.isNamed("varlistentry")) match
          case Some(entry) => convertVarListEntry(entry)
          case None => Seq(node)
      Xml.element(XmlElement.Dl).setChildren(items)

  private def convertVarListEntry(entry: Xml.Element): Xml.Nodes =
    val children: Seq[Xml.Element] = entry.getChildren.flatMap(_.asElement)
    val dts: Seq[Xml.Element] = children.filter(_.isNamed("term")).map: term =>
      Xml.element(XmlElement.Dt).setChildren(term.getChildren.filterNot(_.isWhitespace))
    val dds: Seq[Xml.Element] = children.filter(_.isNamed("listitem")).map: item =>
      Xml.element(XmlElement.Dd).setChildren(item.getChildren.filterNot(_.isWhitespace))
    dts ++ dds

  private def convertAdmonition(element: Xml.Element): Xml.Element =
    if !element.getName.localNameIn(admonitionTypes) || Admonition.is(element) then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val titleEl: Option[Xml.Element] = children.flatMap(_.asElement).find(isDbTitle)
      val body: Xml.Nodes = children.filterNot(node => titleEl.exists(title => node.asElement.contains(title)))
      val title: Option[String] = titleEl.map(_.getText.trim).filter(_.nonEmpty)
      Admonition.make(element.getName.qName, title, body).setId(xmlId(element))

  private def convertAside(element: Xml.Element): Xml.Element =
    if !element.isNamed("sidebar") || Aside.is(element) then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val titleEl: Option[Xml.Element] = children.flatMap(_.asElement).find(isDbTitle)
      val body: Xml.Nodes = children.filterNot(node => titleEl.exists(title => node.asElement.contains(title)))
      val title: Option[String] = titleEl.map(_.getText.trim).filter(_.nonEmpty)
      Aside.make(title, body).setId(xmlId(element))

  private def convertQuote(element: Xml.Element): Xml.Element =
    if (!element.isElement(XmlElement.Blockquote) && !element.isNamed("epigraph")) || Quote.is(element) then
      element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val titleEl: Option[Xml.Element] = children.flatMap(_.asElement).find(isDbTitle)
      val attribution: Xml.Nodes = children.filter(node => node.asElement.exists(_.isNamed("attribution")))
      val body: Xml.Nodes = children.filterNot: node =>
        titleEl.exists(title => node.asElement.contains(title)) ||
        attribution.exists(_ eq node)
      val title: Option[String] = titleEl.map(_.getText.trim).filter(_.nonEmpty)
      val attribNodes: Xml.Nodes = attribution.flatMap: node =>
        node.asElement.fold(Seq(node))(_.getChildren.filterNot(_.isWhitespace))
      Quote.make(title, attribNodes, body).setId(xmlId(element))

  private def convertFigure(element: Xml.Element): Xml.Element =
    if (!element.isNamed("figure") && !element.isNamed("informalfigure")) || Figure.is(element) then
      element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val captions: Xml.Nodes = children.filter: node =>
        node.asElement.exists(el => isDbTitle(el) || el.isNamed("caption"))
      val body: Xml.Nodes = children.filterNot(node => captions.exists(_ eq node))
      val caption: Xml.Nodes = captions.flatMap: node =>
        node.asElement.fold(Seq(node))(_.getChildren.filterNot(_.isWhitespace).toSeq)
      Figure.make(caption, body).setId(xmlId(element))

  private def convertVideo(element: Xml.Element): Xml.Element =
    if !element.isNamed("videodata") then element
    else
      val src: Option[String] =
        element.get(XmlAttribute.Src).orElse(element.get("fileref")).map(_.trim).filter(_.nonEmpty)
      src.fold(element): href =>
        if isRemoteVideo(href) then
          Xml.element("iframe").add(Video.EmbedClass).set(XmlAttribute.Src, href)
        else
          val label: String = href.split('/').lastOption.getOrElse(href)
          Video.make(href, label).setId(xmlId(element))

  private def isRemoteVideo(src: String): Boolean =
    val lower: String = src.toLowerCase
    lower.contains("youtube.com") || lower.contains("youtu.be") || lower.contains("vimeo.com")

  private def convertCalloutList(element: Xml.Element): Xml.Element =
    if !element.isNamed("calloutlist") || Callout.isList(element) then element
    else
      val items: Xml.Nodes = element.getChildren.flatMap: node =>
        node.asElement.filter(_.isNamed("callout")) match
          case Some(callout) =>
            Seq(Xml.element(XmlElement.Li).setChildren(callout.getChildren.filterNot(_.isWhitespace)))
          case None =>
            Seq(node)
      Xml.element(XmlElement.Ol).add(Callout.ListClass).setChildren(items)

  private def convertCo(element: Xml.Element, coNumbers: IdGenerator): Option[Xml.Nodes] =
    if !element.isNamed("co") then None
    else
      val number: String =
        element.get("label").map(_.trim).filter(_.nonEmpty).getOrElse(coNumbers.generate())
      Some(Seq(Callout.marker(number)))

  private def bibliographyEntryIds(xml: Xml.Element): Set[String] =
    xml.gather(
      el => Option.when(el.isNamed("bibliography"))(el),
      stopAtCode = false
    ).flatMap(entryIds).toSet

  private def entryIds(list: Xml.Element): Seq[String] =
    list.getChildren.flatMap(_.asElement)
      .filter(isBibliographyEntry)
      .flatMap(xmlId)

  private def convertBibliography(element: Xml.Element): Xml.Element =
    if !element.isNamed("bibliography") || BibliographyItem.isList(element) || Citation.isList(element) then
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
    val name: String = element.getName.qName
    name == "biblioentry" || name == "bibliomixed"

  private def convertCitation(element: Xml.Element): Xml.Element =
    if !element.isNamed("citation") || Citation.isCite(element) then element
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

  // `rewrite` visits Replace nodes again with the same parent.
  // Inline `code` (and `literal` renamed to `code`) stays `code`, so Keep it.
  // A `pre` wrapper is Replace so the inner `code` sees `pre`.
  private def rewriteCode(nodes: Xml.Nodes): Xml.Rewrite =
    nodes match
      case Seq(only) if only.asElement.exists(_.isNamed("code")) =>
        Xml.Rewrite.Keep(only.asElement.get)
      case _ =>
        Xml.Rewrite.Replace(nodes)

  private def convertCode(element: Xml.Element): Option[Xml.Nodes] =
    val language: Option[String] =
      element.get("language").map(_.trim).filter(_.nonEmpty)
    element.getName.qName match
      case "literal" =>
        Some(Seq(withLanguage(element.renameKeepingClass("code"), language)))
      case "code" =>
        Some(Seq(wrapIfMultiline(withLanguage(element, language))))
      case "programlisting" | "screen" | "literallayout" =>
        val code: Xml.Element = withLanguage(Xml.element(XmlElement.Code).setChildren(element.getChildren), language)
        Some(Seq(Xml.element(XmlElement.Pre).setChildren(Seq(code))))
      case _ =>
        None

  private def withLanguage(element: Xml.Element, language: Option[String]): Xml.Element =
    language.fold(element): name =>
      val cls: String = s"language-${name.toLowerCase}"
      if element.hasClass(cls) then element else element.addClass(cls)

  private def wrapIfMultiline(code: Xml.Element): Xml.Element =
    if code.getText.contains('\n') then Xml.element(XmlElement.Pre).setChildren(Seq(code)) else code
