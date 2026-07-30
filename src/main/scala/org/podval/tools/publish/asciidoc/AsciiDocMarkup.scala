package org.podval.tools.publish.asciidoc

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.html.{HtmlMarkup, HtmlSectionIdsConverter}
import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.link.Toc
import org.podval.tools.publish.markup.{Converter, Markup}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

object AsciiDocMarkup extends Markup(
  name = "AsciiDoc",
  allowsInternalFrontMatter = true,
  extension = "adoc",
  // Note: by supplying `htmlsyntax=xml` we ensure that Asciidoctor produces well-formed XML;
  // the only markup with `rendersToXml=true` is HTML ;)
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect
):
  override def converters(
    ids: IdGenerator = IdGenerator("_generated_id"),
    source: PageSource
  ): Seq[Converter] = Seq(
    AsciiDocDivSoupConverter(),
    AsciiDocFootnoteBodiesConverter(),
    AsciiDocFootnoteLinksConverter(),
    HtmlSectionIdsConverter(ids)
  )

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.getId.contains("footnotes")

  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = HtmlMarkup.retrieveTitle(xml)

  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] = HtmlMarkup.sections(source, xml)

  override def section(xml: Xml.Element, sectionId: String, toc: Toc): Xml.Element = HtmlMarkup.section(xml, sectionId, toc)

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
