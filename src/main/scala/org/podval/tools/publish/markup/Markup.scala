package org.podval.tools.publish.markup

import org.podval.tools.publish.page.FrontMatter
import org.podval.tools.publish.site.{PageError, PageErrorReporter, Path, Site}
import org.podval.tools.publish.util.Files
import org.podval.xml.{Html, Xml, XmlDialect, XmlEncode, XmlParser}
import java.io.File

// TODO because of the cross-markup transclusion,
// all markup-specific stylesheets need to be always included;
// in fact, MathJax and friends too...
// unless we actually calculate the set of markup languages used in a page ;)
abstract class Markup(
  final val name: String,
  // Write policy (`render`, `plus`).
  // TODO use xmlDialect.plus(HtmlXmlDialect) for printing mixed HTML
  // and xmlDialect for pretty-printing native.
  final val xmlDialect: XmlDialect,
  final val extension: String,
  additionalExtensions: Set[String] = Set.empty,
  rendersToXml: Boolean
) derives CanEqual:
  final override def toString: String = name

  final val extensions: Set[String] = additionalExtensions + extension

  def rootElements: Set[String] = Set.empty

  def xmlContent(content: String, sourceFile: File): String

  // Process raw parsed XML:
  // - clean it up (AsciiDoc div soup etc.)
  // - nest HTML sections
  // - convert footnotes into common format
  // - extract title
  def process(xml: Xml.Element, errorReporter: PageErrorReporter): (Xml.Element, Option[Xml.Element])

  final def readAndParse(
    site: Site,
    sourcePath: Path,
    frontMatterStandAlone: Option[Path],
    message: String,
    firstReading: Boolean,
  ): (FrontMatter, Xml.Element) =
    site.log.debug(s"$message: $sourcePath")

    val sourceFile: File = site.sourceFile(sourcePath)
    val sourceContent: String = Files.read(sourceFile)
    val (frontMatterInternalContent: Option[String], content: String) = FrontMatter.split(sourceContent)
    val frontMatterStandAloneContent: Option[String] = frontMatterStandAlone.map(site.sourceFile).map(Files.read)

    if frontMatterInternalContent.isDefined && frontMatterStandAloneContent.isDefined then
      site.error(
        sourcePath = sourcePath,
        kind = PageError.AmbiguousFrontMatter,
        message = s"Ambiguous FrontMatter: both internal [$frontMatterInternalContent] and standalone [$frontMatterStandAloneContent] found"
      )

    val frontMatterContent: Option[String] = frontMatterInternalContent.orElse(frontMatterStandAloneContent)

    val frontMatter: FrontMatter = FrontMatter.parse(frontMatterContent) match
      case Right(frontMatter) =>
        // TODO mark as stand-alone for round-trip
        frontMatter
      case Left(error) =>
        if firstReading then
          site.error(
            sourcePath = sourcePath,
            kind = PageError.MalformedFrontMatter,
            message = s"Malformed FrontMatter: [$frontMatterContent]",
            cause = Some(error)
          )

        FrontMatter.empty

    val xmlString: String = xmlContent(content, sourceFile)

    val xml: Xml.Element = XmlParser.parse(xmlString, isXml = rendersToXml) match
      case Right(xml) =>
        xml
      case Left(error) =>
        if firstReading then
          site.error(
            sourcePath = sourcePath,
            kind = PageError.MalformedXml,
            message = s"malformed XML ($name)",
            cause = Some(error)
          )

        Xml
          .element(name)
          .addClass(s"malformed-$name")
          .setText(s"malformed $name: $error\n${XmlEncode.escape(xmlString)}")

    (frontMatter, xml)

object Markup:
  // Known markup languages.
  // Note: some XmlLike markups can have extensions other than `.xml`.
  lazy val all: Seq[Markup] = Seq(
    HtmlMarkup,
    MarkdownMarkup,
    AsciiDocMarkup,
    TeiMarkup,
    DocBookMarkup
  )

  // TODO verify that extensions do not overlap
  private lazy val forExtension: Map[String, Markup] = all
    .flatMap(markup => markup.extensions.map(_ -> markup))
    .toMap

  def forExtension(extension: String): Option[Markup] = forExtension.get(extension)

  private lazy val forElement: Map[String, Markup] = all
    .flatMap(markup => markup.rootElements.map(_ -> markup))
    .toMap

  def forElement(element: String): Option[Markup] = forElement.get(element)
