package org.podval.tools.publish.markup

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlUtil}
import zio.blocks.chunk.Chunk
import java.io.File

// TODO deal with
// class="bare" means “this anchor’s label is the bare URI”.
// Default AsciiDoc print CSS treats non-bare http(s) links specially—e.g. appends the URL after the text.
// For bare links that would duplicate the URL, so rules like:
//   a.bare, a[href^="#"], a[href^="mailto:"] { text-decoration: none !important }
//   a[href^="http:"]:not(.bare)::after, a[href^="https:"]:not(.bare)::after { content: "(" attr(href) ")"; ... }
//skip the “print URL after text” decoration when class="bare" is present.
object AsciiDocMarkup extends Markup(
  name = "AsciiDoc",
  extension = "adoc",
  // Note: by supplying `htmlsyntax=xml` we ensure that Asciidoctor produces well-formed XML;
  // the only markup with `rendersToXml=true` is HTML ;)
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect
):
  private var asciidoctorVar: Option[Asciidoctor] = None
  def asciidoctor: Asciidoctor = synchronized:
    asciidoctorVar.getOrElse:
      val result: Asciidoctor = Asciidoctor.Factory.create()
      AsciiDocCiteExtension.register(result)
      asciidoctorVar = Some(result)
      result

  def closeAsciidoctor(): Unit = synchronized:
    asciidoctorVar.foreach(_.close())
    asciidoctorVar = None

  override def isSectionHeader(element: Xml.Element): Boolean = HtmlMarkup.isSectionHeader(element)

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.getId.contains("footnotes")

  override def xmlContent(content: String, sourceFile: File): String =
    convert(content, sourceFile, asciidoctor)

  private[markup] def convert(
    content: String,
    sourceFile: File,
    asciidoctor: Asciidoctor
  ): String =
    val attributes: Attributes = Attributes
      .builder()
      // Suppress the TOC.
      .attribute("toc", null)
      // Suppress section anchors, automatic ids, links and numbers.
      .attribute("sectanchors", null)
      .attribute("sectids", null)
      .attribute("sectlinks", null)
      .attribute("sectnums", null)
      // Preserve document title.
      .showTitle(true)
      // Render into XML and not HTML.
      .attribute("htmlsyntax", "xml")
      // Set some attributes.
      .attribute("docfile", sourceFile.getAbsolutePath)
      .attribute("docdir", sourceFile.getParentFile.getAbsolutePath)

      // Note: if in the future we need to set default values of attributes from Site:
//      .attribute("author@", site.config.author)
//      .attribute("email@", site.config.email)

      .build()

    val options: Options = Options
      .builder()
      .backend("html5")
      .toFile(false)
      .standalone(false)
      .safe(SafeMode.UNSAFE)
      .attributes(attributes)
      .build()

    val result: String = asciidoctor.convert(content, options)

    // Wrap AsciiDoc rendered as HTML in a 'div'.
    s"<div>$result</div>"

  override def process(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    HtmlMarkup.process(cleanup(xml), errorReporter)

  private[markup] def cleanup(xml: Xml.Element): Xml.Element =
    val cleaned: Xml.Element = xml.transform((element: Xml.Element) =>
      var result: Xml.Element = element

      val classes: Chunk[String] = result.getClasses
      if classes.nonEmpty then result = result.setClasses(classes.filterNot(spuriousClasses.contains))

      var children: Xml.Nodes = result.getChildren
      children = XmlUtil.convertElements(children, HtmlMarkup.unwrapSpuriousParagraph)
      children = removeSpuriousDivs(children)
      if asciidoctorGlossaryClasses.forall(result.hasClass) then
        children = convertGlossaryLists(children)
        result = result
          .setClasses(result.getClasses.filterNot(asciidoctorGlossaryClasses.contains))
          .add(Glossary.ListClass)
      children = convertCalloutMarks(children)
      result = result.setChildren(children)
      result = convertTaskList(result)
      result = convertFootnoteLink(result).getOrElse(result)
      result = convertFootnoteBody(result).getOrElse(result)
      result = convertCalloutList(result)
      result = convertAdmonition(result)
      result = convertSidebar(result)
      result = convertQuote(result)
      result = convertImageBlock(result)
      result
    )
    // Default transform does not recurse into `<code>`, where listing callouts live.
    rewriteCalloutMarks(cleaned)

  private def rewriteCalloutMarks(element: Xml.Element): Xml.Element =
    val children: Xml.Nodes = convertCalloutMarks(element.getChildren).map: node =>
      node.asElement.fold(node)(rewriteCalloutMarks)
    element.setChildren(children)

  // Distill the soup of meaningless `div`s that Asciidoctor emits;
  // see, for example, https://tiffnix.com/soupault#html-de-uglifier-plugin.

  private val spuriousClasses: Set[String] = Set(
    "tableblock", "halign-left", "valign-top", "frame-all", "grid-all", "fit-content", "stretch"
  )

  private val spuriousDivClasses: Set[String] = Set(
    "paragraph", "sectionbody", "ulist", "olist", "openblock", "content"
  )

  private def isSpuriousDiv(element: Xml.Element): Boolean =
    element.getClasses.exists(spuriousDivClasses.contains) ||
    element
      .getChildren
      .flatMap(_.asElement)
      .headOption
      .flatMap(HtmlMarkup.headerLevel)
      .exists(headerLevel => element.hasClass(s"sect${headerLevel - 1}"))

  private def removeSpuriousDivs(children: Xml.Nodes): Xml.Nodes = XmlUtil.convertElements(children, element =>
    Option.when(element.getName == "div" && isSpuriousDiv(element))(
      removeSpuriousDivs(element.getChildren)
    )
  )

  // Asciidoctor html5 without `icons`: `<b class="conum">(1)</b>` in the listing;
  // `div.colist > ol > li`. With `icons=font`: `<i class="conum" data-value="1"></i><b>(1)</b>`
  // and a table in the colist. Convert both to Callout IR.
  private def convertCalloutMarks(nodes: Xml.Nodes): Xml.Nodes =
    var skipGuardBold: Boolean = false
    val result: List[Xml.Node] = nodes.toList.flatMap: node =>
      if skipGuardBold then
        skipGuardBold = false
        node.asElement.filter(isGuardBold).fold(List(node))(_ => Nil)
      else
        node.asElement match
          case Some(element) =>
            convertCalloutMark(element) match
              case Some(mark) =>
                skipGuardBold = isConumIcon(element)
                List(mark)
              case None =>
                List(node)
          case None =>
            List(node)
    Chunk.from(result)

  private def convertCalloutMark(element: Xml.Element): Option[Xml.Element] =
    val fromIcon: Option[String] =
      Option.when(isConumIcon(element) || element.hasClass("conum"))(
        element.get("data-value").flatMap(calloutNumber).orElse(calloutNumber(element.getText))
      ).flatten
    val fromImg: Option[String] =
      Option.when(element.getName == "img" && element.get("src").exists(_.contains("callout")))(
        element.get("alt").flatMap(calloutNumber)
      ).flatten
    fromIcon.orElse(fromImg).map(Callout.marker)

  private def calloutNumber(text: String): Option[String] =
    val trimmed: String = text.trim
    val digits: String =
      if trimmed.startsWith("(") && trimmed.endsWith(")") && trimmed.length >= 3
      then trimmed.substring(1, trimmed.length - 1)
      else trimmed
    Option.when(digits.nonEmpty && digits.forall(_.isDigit))(digits)

  private def isConumIcon(element: Xml.Element): Boolean =
    element.getName == "i" && element.hasClass("conum")

  private def isGuardBold(element: Xml.Element): Boolean =
    element.getName == "b" && calloutNumber(element.getText).isDefined

  private val asciidocAdmonitionTypes: Set[String] =
    Set("note", "tip", "important", "caution", "warning")

  // Asciidoctor: <div class="sidebarblock"><div class="content">…; `content` is unwrapped first.
  private def convertSidebar(element: Xml.Element): Xml.Element =
    if element.getName != "div" || !element.hasClass("sidebarblock") then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val (title, body): (Option[String], Xml.Nodes) =
        children.headOption.flatMap(_.asElement)
          .filter(child => child.getName == "div" && child.hasClass("title")) match
            case Some(heading) =>
              (Some(heading.getText.trim).filter(_.nonEmpty), children.drop(1))
            case None =>
              (None, children)
      Aside.make(title, body)

  // Asciidoctor: <div class="quoteblock"> optional div.title, blockquote, optional div.attribution.
  // Inner paragraph/content wrappers are still on the blockquote; unwrap them into the IR body.
  private def convertQuote(element: Xml.Element): Xml.Element =
    if element.getName != "div" || !element.hasClass("quoteblock") then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val (title, rest): (Option[String], Xml.Nodes) =
        children.headOption.flatMap(_.asElement)
          .filter(child => child.getName == "div" && child.hasClass("title")) match
            case Some(heading) =>
              (Some(heading.getText.trim).filter(_.nonEmpty), children.drop(1))
            case None =>
              (None, children)
      val inner: Option[Xml.Element] =
        rest.flatMap(_.asElement).find(_.getName == "blockquote")
      val attribution: Xml.Nodes =
        rest.flatMap(_.asElement)
          .find(el => el.getName == "div" && el.hasClass("attribution"))
          .fold(Chunk.empty[Xml.Node])(_.getChildren.filterNot(_.isWhitespace))
      val body: Xml.Nodes =
        inner.fold(rest.filterNot(isQuoteAttribution)): quote =>
          removeSpuriousDivs(quote.getChildren)
      Quote.make(title, attribution, body).setId(element.getId)

  // Asciidoctor: <div class="imageblock"><div class="content">img</div> optional div.title.
  // `content` is unwrapped first; title is a sibling after the image.
  private def convertImageBlock(element: Xml.Element): Xml.Element =
    if element.getName != "div" || !element.hasClass("imageblock") then element
    else
      val children: Xml.Nodes = element.getChildren.filterNot(_.isWhitespace)
      val (body, caption): (Xml.Nodes, Option[String]) =
        children.lastOption.flatMap(_.asElement)
          .filter(child => child.getName == "div" && child.hasClass("title")) match
            case Some(heading) =>
              (children.dropRight(1), Some(heading.getText.trim).filter(_.nonEmpty))
            case None =>
              (children, None)
      val extra: Chunk[String] = element.getClasses.filterNot(_ == "imageblock")
      extra.foldLeft(Figure.make(caption, body).setId(element.getId))(_.addClass(_))

  private def isQuoteAttribution(node: Xml.Node): Boolean =
    node.asElement.exists(el => el.getName == "div" && el.hasClass("attribution"))

  // Asciidoctor: <div class="admonitionblock note"><table> icon + content cells.
  private def convertAdmonition(element: Xml.Element): Xml.Element =
    if element.getName != "div" || !element.hasClass("admonitionblock") then element
    else
      val typeName: String =
        element.getClasses.find(asciidocAdmonitionTypes.contains).getOrElse("note")
      val cells: Seq[Xml.Element] = element.gather(el =>
        Option.when(el.getName == "td")(el)
      ).toSeq
      val icon: Option[Xml.Element] = cells.find(_.hasClass("icon"))
      val content: Option[Xml.Element] = cells.find(_.hasClass("content"))
      val contentChildren: Xml.Nodes =
        content.fold(Chunk.empty[Xml.Node])(_.getChildren.filterNot(_.isWhitespace))
      val (titleFromContent, body): (Option[String], Xml.Nodes) =
        contentChildren.headOption.flatMap(_.asElement)
          .filter(child => child.getName == "div" && child.hasClass("title")) match
            case Some(heading) =>
              (Some(heading.getText.trim).filter(_.nonEmpty), contentChildren.drop(1))
            case None =>
              (None, contentChildren)
      val titleFromIcon: Option[String] =
        icon.flatMap: cell =>
          val labelled: Option[String] = cell
            .gather(el => el.get("title"))
            .headOption
            .orElse(Some(cell.getText.trim).filter(_.nonEmpty))
          labelled.map(_.trim).filter(_.nonEmpty)
      Admonition.make(typeName, titleFromContent.orElse(titleFromIcon), body)

  private def convertCalloutList(element: Xml.Element): Xml.Element =
    if element.getName != "div" || !element.hasClass("colist") then element
    else
      val innerOl: Option[Xml.Element] =
        element.getChildren.flatMap(_.asElement).find(_.getName == "ol")
      val fromTable: Option[Xml.Element] =
        element.getChildren.flatMap(_.asElement).find(_.getName == "table").map: table =>
          val rows: Seq[Xml.Element] = table.getChildren.flatMap(_.asElement).toSeq.flatMap: child =>
            if child.getName == "tr" then Seq(child)
            else child.getChildren.flatMap(_.asElement).filter(_.getName == "tr").toSeq
          val items: Seq[Xml.Element] = rows.map: tr =>
            val cells: Seq[Xml.Element] =
              tr.getChildren.flatMap(_.asElement).filter(_.getName == "td").toSeq
            Xml.element("li").setChildren(cells.lift(1).fold(Chunk.empty[Xml.Node])(_.getChildren))
          Xml.element("ol").setChildren(Chunk.from(items))
      innerOl.orElse(fromTable).fold(element)(_.add(Callout.ListClass))

  // Asciidoctor: <div class="dlist glossary"><dl> sibling dt/dd.
  // Convert to Glossary IR (class="glossary" wrapper, glossary-item children).
  private val asciidoctorGlossaryClasses: Set[String] = Set("dlist", "glossary")

  // Asciidoctor html5: ul.checklist; default glyphs &#10003;/&#10063;, %interactive inputs,
  // icons=font Font Awesome.
  private def convertTaskList(element: Xml.Element): Xml.Element =
    if element.getName != "ul" && element.getName != "ol" then element
    else
      val acceptMarkers: Boolean = element.hasClass("checklist")
      val children: Xml.Nodes = element.getChildren.map: node =>
        node.asElement.filter(_.getName == "li").fold(node)(convertChecklistItem(_, acceptMarkers))
      var list: Xml.Element = element.setChildren(children)
      if acceptMarkers then
        list = list.setClasses(list.getClasses.filterNot(_ == "checklist"))
      TaskList.asList(list)

  private def convertChecklistItem(li: Xml.Element, acceptMarkers: Boolean): Xml.Element =
    val rest: Xml.Nodes = li.getChildren.dropWhile(node => node.asText.exists(_.forall(_.isWhitespace)))
    rest.headOption.flatMap(_.asElement).filter(isInteractiveCheckbox) match
      case Some(box) =>
        val done: Boolean = box.get("checked").isDefined || box.get("data-item-complete").contains("1")
        TaskList.asItem(li, done, rest.tail)
      case None if acceptMarkers =>
        rest.headOption.flatMap(_.asElement).filter(isFaCheckbox) match
          case Some(icon) =>
            TaskList.asItem(li, icon.hasClass("fa-check-square-o"), rest.tail)
          case None =>
            rest.headOption.flatMap(_.asText).flatMap(markerState) match
              case Some((done, remainder)) =>
                val after: Xml.Nodes =
                  if remainder.isEmpty then rest.tail
                  else Xml.text(remainder) +: rest.tail
                TaskList.asItem(li, done, after)
              case None => li
      case None => li

  private def isInteractiveCheckbox(element: Xml.Element): Boolean =
    element.getName == "input" && (
      element.get("type").contains("checkbox") || element.get("data-item-complete").isDefined
    )

  private def isFaCheckbox(element: Xml.Element): Boolean =
    element.getName == "i" && (
      element.hasClass("fa-check-square-o") || element.hasClass("fa-square-o")
    )

  private val checkedMarkers: Set[Char] = Set('\u2713', '\u2714', '\u2611')
  private val uncheckedMarkers: Set[Char] = Set('\u274f', '\u2610', '\u25a1')

  private def markerState(text: String): Option[(Boolean, String)] =
    val trimmed: String = text.dropWhile(_.isWhitespace)
    if trimmed.isEmpty then None
    else
      val mark: Char = trimmed.charAt(0)
      val rest: String = trimmed.substring(1).dropWhile(_.isWhitespace)
      if checkedMarkers.contains(mark) then Some(true, rest)
      else if uncheckedMarkers.contains(mark) then Some(false, rest)
      else None

  private def convertGlossaryLists(nodes: Xml.Nodes): Xml.Nodes =
    nodes.map: node =>
      node.asElement match
        case Some(dl) if dl.getName == "dl" =>
          dl.setChildren(DescriptionList.groupItems(dl.getChildren, Glossary.ItemClass, DescriptionList.stripExplicitTermId))
        case _ => node

  // From:
  //   <sup class="footnote">[<a id="_footnoteref_N" class="footnote" href="#_footnotedef_N">N</a>]</sup>
  // To:
  //   <a class="footnote-link" footnoteCorrelationId="N"/>
  private def convertFootnoteLink(element: Xml.Element): Option[Xml.Element] =
    val isFootnoteLink: Boolean = element.getName == "sup" /* && element.hasClass("footnote") */
    if !isFootnoteLink then None else
      for correlationId: String <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote"))
        .flatMap(_.getTextOpt)
      yield
        Footnote.link(correlationId)

  // From:
  //   <div class="footnote" id="_footnotedef_N"><a href="#_footnoteref_N">N</a>. Footnote Body</div>
  // To:
  //   <span class="footnote" footnoteCorrelationId="N">Footnote Body</span>
  private def convertFootnoteBody(element: Xml.Element): Option[Xml.Element] =
    val isFootnoteBody: Boolean = element.getName == "div" && element.hasClass("footnote")
    if !isFootnoteBody then None else
      // 'a' child
      for correlationId: String <- element
        .getChildren
        .flatMap(_.asElement)
        .headOption
        .flatMap(_.getTextOpt)
      yield
        // after the 'a' child
        var body: Xml.Nodes = element
          .getChildren
          .dropUntil(_.asElement.isDefined)

        body.head.asText.foreach: text => 
          if text.startsWith(".") then
            body = Xml.text(text.drop(1)) +: body.tail

        Footnote.body(
          correlationId,
          body
        )
