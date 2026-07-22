package org.podval.tools.publish.asciidoc

import org.asciidoctor.{Asciidoctor, Attributes, Options, SafeMode}
import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.markup.HtmlLikeMarkup

object AsciiDocMarkup extends HtmlLikeMarkup(
  name = "AsciiDoc",
  allowsInternalFrontMatter = false,
  extension = "adoc",
  // Note: by supplying `htmlsyntax=xml` we ensure that Asciidoctor produces well-formed XML;
  // the only markup with `rendersToXml=true` is HTML ;)
  rendersToXml = true
):
  private var asciidoctorVar: Option[Asciidoctor] = None
  private def asciidoctor(site: Site): Asciidoctor = asciidoctorVar.getOrElse:
    val result: Asciidoctor = Asciidoctor.Factory.create()
    // Note: only extensions packaged as jars will work - if they are on the classpath.
    site.asciidoctorExtensions.foreach: gemName =>
      site.log.info(s"Loading AsciiDoc extension gem '$gemName'")
      result.requireLibrary(gemName)
    asciidoctorVar = Some(result)
    result

  override def xmlContent(
    site: Site,
    sourcePath: Path,
    content: String
  ): String =
    val attributes: Attributes = Attributes
      .builder()
      // Preserve document title.
      .showTitle(true)
      // Render into XML and not HTML.
      .attribute("htmlsyntax", "xml")
      // Set some attributes.
      .attribute("docfile", site.sourceFile(sourcePath).getAbsolutePath)
      .attribute("docdir", site.sourceFile(sourcePath).getParentFile.getAbsolutePath)

      // TODO author and email attributes should be set from the Site

      // TODO presence/absence and placement of the TOC can be controlled/overridden from here:
      //      .tableOfContents() Boolean/Placement

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
