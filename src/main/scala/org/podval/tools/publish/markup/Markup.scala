package org.podval.tools.publish.markup

import org.podval.tei.EntityKind
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.tools.publish.processor.{ConverterWithIds, HtmlConverter, PostConverter, Processors, Transformer}
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.{FrontMatter, PageContent}
import org.podval.tools.publish.util.Files
import org.podval.xml.{Xml, XmlDialect, XmlParser}
import java.io.File

abstract class Markup derives CanEqual:
  def name: String

  final override def toString: String = name

  def extension: String

  def additionalExtensions: Set[String]

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)
  
  def xmlContent(content: String): String

  // TODO use xmlDialect.plus(HtmlXmlDialect) for processing/printing
  // and xmlDialect for pretty-printing.
  def xmlDialect: XmlDialect

  def entityKind(xml: Xml.Element): Option[EntityKind] = None
  
  def sections(content: PageContent): Seq[Fragment.Section]

  def processors: Processors

  lazy val converters: Seq[ConverterWithIds] = processors
    .processors
    .collect { case converter: ConverterWithIds => converter }
    .sortBy(_.convertLinks)

  lazy val transformers: Seq[Transformer] = processors
    .processors
    .collect { case transformer: Transformer => transformer }
    .sortBy(_.transformsFootnotes)

  lazy val postConverters: Seq[PostConverter] = processors
    .processors
    .collect { case postConverter: PostConverter => postConverter }

  lazy val htmlConverters: Seq[HtmlConverter] = processors
    .processors
    .collect { case htmlConverter: HtmlConverter => htmlConverter }

  final def parse(content: String): Either[Throwable, Xml.Element] =
    XmlParser.parse(xmlContent(content))

  final def readAndParse(
    site: Site,
    sourcePath: Path,
    standAloneFrontMatter: Option[Path],
    message: String,
    firstReading: Boolean,
  ): (FrontMatter, Xml.Element) =
    site.log.debug(s"$message: $sourcePath")

    val frontMatterContentStandAlone: Option[String] = standAloneFrontMatter
      .map(_.file(site.sourceDirectory))
      .map(Files.read)

    val (frontMatterContentInternal: Option[String], content: String) =
      FrontMatter.split(Files.read(sourcePath.file(site.sourceDirectory)))

    // TODO error if both are present
    val frontMatterContent: Option[String] = frontMatterContentInternal.orElse(frontMatterContentStandAlone)

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

    val xml: Xml.Element = parse(content) match
      case Right(xml) =>
        xml
      case Left(error) =>
        if firstReading then
          site.error(
            sourcePath = sourcePath,
            kind = PageError.MalformedXml,
            message = s"malformed XML ($extension)",
            cause = Some(error)
          )

        Xml
          .element(xmlDialect.root.head)
          .addClass(s"malformed-$extension")
          .setText(s"Malformed $name: $error")

    (frontMatter, xml)

object Markup:
  val xmlLike: List[XmlLikeMarkup] = List(
    TeiMarkup
  )

  // Some XmlLike markups can have extensions other than `.xml`, so we add `xmlLike` to `all`:
  private val all: List[Markup] = xmlLike ++ List(
    MarkdownMarkup,
    HtmlMarkup
  )

  def forExtension(extension: Option[String]): Option[Markup] = extension.flatMap(forExtension)
  private def forExtension(extension: String): Option[Markup] = all.find(_.isExtension(extension))
