package org.podval.tools.publish.markup

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.page.PageSource
import org.podval.xml.{HtmlXmlDialect, Xml}
import zio.blocks.chunk.Chunk
import java.io.File

object AsciiDocMarkup extends Markup(
  name = "AsciiDoc",
  allowsInternalFrontMatter = true,
  extension = "adoc",
  // Note: by supplying `htmlsyntax=xml` we ensure that Asciidoctor produces well-formed XML;
  // the only markup with `rendersToXml=true` is HTML ;)
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect
):
  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.getId.contains("footnotes")

  private var asciidoctorVar: Option[Asciidoctor] = None
  private def asciidoctor: Asciidoctor = asciidoctorVar.getOrElse:
    val result: Asciidoctor = Asciidoctor.Factory.create()
//    // Note: only extensions packaged as jars will work - if they are on the classpath.
//    site.asciidoctorExtensions.foreach: gemName =>
//      site.log.info(s"Loading AsciiDoc extension gem '$gemName'")
//      result.requireLibrary(gemName)
    asciidoctorVar = Some(result)
    result

  override def xmlContent(content: String, sourceFile: File): String =
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
      .standalone(false) // No HTNL wrapper: site publisher does it for all formats.
      .safe(SafeMode.UNSAFE /* TODO more safe? */)
      .attributes(attributes)
      .build()

    val result: String = asciidoctor.convert(content, options)

    // Wrap AsciiDoc rendered as HTML in a 'div'.
    s"<div>$result</div>"

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    val result: Xml.Element = xmlDialect.transform(xml, (element: Xml.Element) =>
      var result: Xml.Element = element
      result = cleanUp(result) // TODO run in a separate transform()?
      result = convertFootnoteLink(result).getOrElse(result)
      result = convertFootnoteBody(result).getOrElse(result)
      result
    )
    HtmlMarkup.process(
      source,
      result
    )

  // Distill the soup of meaningless `div`s that Asciidoctor emits;
  // see, for example, https://tiffnix.com/soupault#html-de-uglifier-plugin.
  private def cleanUp(element: Xml.Element): Xml.Element =
    removeSpuriousClasses(element).setChildren(removeSpuriousElements(element.getChildren))

  private def removeSpuriousClasses(element: Xml.Element): Xml.Element =
    val classes = element.getClasses
    if classes.isEmpty then element else
      element.setClasses(classes.filterNot(spuriousClasses.contains))

  private def removeSpuriousElements(children: Xml.Nodes): Xml.Nodes = children.flatMap: child =>
    val replacement: Option[Xml.Nodes] = child.asElement.flatMap: element =>
      if
        // Remove spurious 'div's.
        element.getName == "div" && (
          element
            .getClasses
            .exists(cls => spuriousDivClasses.contains(cls))
          ||
          element
            .getChildren
            .flatMap(_.asElement)
            .headOption
            .flatMap(HtmlMarkup.headerLevel)
            .exists(headerLevel => element.hasClass(s"sect${headerLevel - 1}"))
          )
      then
        Some(removeSpuriousElements(element.getChildren))
      else if element.getName == "td" || element.getName == "li" then
        // Remove 'p's in 'td's and 'li's.
        val (init, tail) = element.getChildren.span(_.asElement.isEmpty)
        for
          head <- tail.headOption.map(_.asElement.get)
          if head.getName == "p"
        yield
          Chunk(element.setChildren(init ++ head.getChildren ++ tail.tail))
      else
        None

    replacement match
      case None =>
        Chunk(child)
      case Some(result) =>
        result

  private val spuriousDivClasses: Set[String] = Set(
    "paragraph", "sectionbody", "ulist", "olist", "quoteblock", "openblock", "content"
  )

  private val spuriousClasses: Set[String] = Set(
    "tableblock", "halign-left", "valign-top", "frame-all", "grid-all", "fit-content", "stretch"
  )

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
        .map(_.getText)
      yield
        Footnotes.linkStub(correlationId)

  // From:
  //   <div class="footnote" id="_footnotedef_N"><a href="#_footnoteref_N">N</a>. Footnote Body</div>
  // To:
  //   <span class="footnote" footnoteCorrelationId="N">Footnote Body</span>
  private def convertFootnoteBody(element: Xml.Element): Option[Xml.Element] =
    val isFootnoteBody: Boolean = element.getName == "div" && element.hasClass("footnote")
    if !isFootnoteBody then None else
      for correlationId: String <- element
        .getChildren
        .flatMap(_.asElement)
        .headOption
        .map(_.getText)
      yield
        val body: Xml.Nodes = element.getChildren.dropUntil(_.asElement.isDefined) // TODO why?
        Footnotes.bodyStub(
          correlationId,
          body.head.asText match
            case Some(text) if text.startsWith(".") => Xml.text(text.drop(1)) +: body.tail
            case _ => body
        )
