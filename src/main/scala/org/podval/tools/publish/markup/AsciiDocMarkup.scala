package org.podval.tools.publish.markup

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.Site
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
  def asciidoctor(site: Site): Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    //    // Note: only extensions packaged as jars will work - if they are on the classpath.
    //    site.asciidoctorExtensions.foreach: gemName =>
    //      site.log.info(s"Loading AsciiDoc extension gem '$gemName'")
    //      result.requireLibrary(gemName)
    result

  override def isSectionHeader(element: Xml.Element): Boolean = HtmlMarkup.isSectionHeader(element)

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.getId.contains("footnotes")

  override def xmlContent(content: String, sourceFile: File, site: Site): String =
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

    val result: String = site.asciidoctor.convert(content, options)

    // Wrap AsciiDoc rendered as HTML in a 'div'.
    s"<div>$result</div>"

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    HtmlMarkup.process(source, cleanup(xml))

  private[markup] def cleanup(xml: Xml.Element): Xml.Element =
    xmlDialect.transform(xml, (element: Xml.Element) =>
      var result: Xml.Element = element

      val classes: Chunk[String] = result.getClasses
      if classes.nonEmpty then result = result.setClasses(classes.filterNot(spuriousClasses.contains))

      var children: Xml.Nodes = result.getChildren
      children = XmlUtil.convertElements(children, removeSpuriousParagraphs)
      children = removeSpuriousDivs(children)
      if result.getName == "dl" then children = groupDescriptionListItems(children)
      result = result.setChildren(children)

      result = convertFootnoteLink(result).getOrElse(result)
      result = convertFootnoteBody(result).getOrElse(result)
      result
    )

  // Distill the soup of meaningless `div`s that Asciidoctor emits;
  // see, for example, https://tiffnix.com/soupault#html-de-uglifier-plugin.

  private val spuriousClasses: Set[String] = Set(
    "tableblock", "halign-left", "valign-top", "frame-all", "grid-all", "fit-content", "stretch"
  )

  private val spuriousDivClasses: Set[String] = Set(
    "paragraph", "sectionbody", "ulist", "olist", "quoteblock", "openblock", "content"
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

  // Note: written by Grok ;)
  // Asciidoctor emits one <dl> with sibling <dt>/<dd> and [[id]] as an empty <a id>
  // inside the term. HTML5 groups each name-value pair in a <div>; put the id on that div.
  private def groupDescriptionListItems(nodes: Xml.Nodes): Xml.Nodes =
    var result: List[Xml.Node] = Nil
    var group: List[Xml.Node] = Nil
    var groupId: Option[String] = None

    def flush(): Unit =
      if group.nonEmpty then
        result = result :+ Xml
          .element("div")
          .add(Glossary.ItemClass)
          .setId(groupId)
          .setChildren(Chunk.from(group))
        group = Nil
        groupId = None

    nodes.foreach: node =>
      node.asElement match
        case Some(element) if element.getName == "dt" =>
          flush()
          val (id, dt) = takeTermId(element)
          groupId = id
          group = List(dt)
        case Some(element) if element.getName == "dd" =>
          if group.isEmpty then result = result :+ element
          else group = group :+ element
        case Some(element) =>
          flush()
          result = result :+ element
        case None =>
          if !node.isWhitespace then
            flush()
            result = result :+ node

    flush()
    Chunk.from(result)

  private def takeTermId(dt: Xml.Element): (Option[String], Xml.Element) =
    val (leading, rest) = dt.getChildren.span(node =>
      node.isWhitespace || node.asElement.exists(isEmptyIdAnchor)
    )
    val fromAnchor: Option[String] = leading.flatMap(_.asElement).flatMap(_.getId).headOption
    val stripped: Xml.Element = if fromAnchor.isEmpty then dt else dt.setChildren(rest)
    val id: Option[String] = fromAnchor.orElse(stripped.getId)
    val term: Xml.Element = if stripped.getId.isEmpty then stripped else stripped.setId("")
    (id, term)

  private def isEmptyIdAnchor(element: Xml.Element): Boolean =
    element.isA &&
    element.getId.nonEmpty &&
    element.getHref.isEmpty &&
    element.getChildren.forall(_.isWhitespace)

  // Remove 'p's in 'td's, 'li's, and 'dd's.
  private def removeSpuriousParagraphs(element: Xml.Element): Option[Xml.Nodes] =
    val isElementToConvert: Boolean = element.getName == "td" || element.getName == "li" || element.getName == "dd"
    if !isElementToConvert then None else
      val (init, tail) = element.getChildren.span(_.asElement.isEmpty)
      for
        head <- tail.headOption.map(_.asElement.get)
        if head.getName == "p"
      yield
        Chunk(element.setChildren(init ++ head.getChildren ++ tail.tail))

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
