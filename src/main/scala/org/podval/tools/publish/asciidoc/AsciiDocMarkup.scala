package org.podval.tools.publish.asciidoc

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.markup.{HtmlSections, MarkupKind, Processor}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

object AsciiDocMarkup extends MarkupKind(
  name = "AsciiDoc",
  allowsInternalFrontMatter = true,
  extension = "adoc",
  // Note: by supplying `htmlsyntax=xml` we ensure that Asciidoctor produces well-formed XML;
  // the only markup with `rendersToXml=true` is HTML ;)
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect
):
  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = HtmlSections.retrieveTitle(xml)

  override def pageHeader(page: MarkupPage): Html.Element = MarkupKind.pageHeader(page)

  // TODO
  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] = HtmlSections.sections(source, xml)

  private var asciidoctorVar: Option[Asciidoctor] = None
  private def asciidoctor(site: Site): Asciidoctor = asciidoctorVar.getOrElse:
    val result: Asciidoctor = Asciidoctor.Factory.create()
//    // Note: only extensions packaged as jars will work - if they are on the classpath.
//    site.asciidoctorExtensions.foreach: gemName =>
//      site.log.info(s"Loading AsciiDoc extension gem '$gemName'")
//      result.requireLibrary(gemName)
    asciidoctorVar = Some(result)
    result

  override def xmlContent(
    site: Site,
    sourcePath: Path,
    content: String
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
      .attribute("docfile", site.sourceFile(sourcePath).getAbsolutePath)
      .attribute("docdir", site.sourceFile(sourcePath).getParentFile.getAbsolutePath)

      // TODO author and email attributes should be set from the Site

      .build()

    val options: Options = Options
      .builder()
      .backend("html5")
      .toFile(false)
      .standalone(false) // No HTNL wrapper: site publisher does it for all formats.
      .safe(SafeMode.UNSAFE /* TODO more safe? */)
      .attributes(attributes)
      .build()

    val result: String = asciidoctor(site).convert(content, options)

    // Wrap AsciiDoc rendered as HTML in a 'div'.
    s"<div>$result</div>"

  def processors: Seq[Processor] = Seq(
    new AsciiDocDivSoupConverter,
    new AsciiDocFootnoteLinksConverter,
    new AsciiDocFootnoteBodiesConverter
  )

  def isFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.getId.contains("footnotes")
  