package org.podval.tools.publish.asciidoc

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.html.{HtmlMarkup, HtmlSectionsTransformer}
import org.podval.tools.publish.markup.{Markup, Processor}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlXmlDialect, Xml}
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
  override def processors(
    ids: IdGenerator,
    source: PageSource
  ): Seq[Processor] = Seq(
    AsciiDocDivSoupConverter(),
    AsciiDocFootnoteBodiesConverter(),
    AsciiDocFootnoteLinksConverter(),
    HtmlSectionsTransformer(ids)
  )

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.getId.contains("footnotes")

  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = HtmlMarkup.retrieveTitle(xml)

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
//      .showTitle(true)
      // Render into XML and not HTML.
      .attribute("htmlsyntax", "xml")
      // Set some attributes.
      .attribute("docfile", sourceFile.getAbsolutePath)
      .attribute("docdir", sourceFile.getParentFile.getAbsolutePath)

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

    val result: String = asciidoctor.convert(content, options)

    // Wrap AsciiDoc rendered as HTML in a 'div'.
    s"<div>$result</div>"
