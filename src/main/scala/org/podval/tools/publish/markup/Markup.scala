package org.podval.tools.publish.markup

import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.tools.publish.processor.{Converter, HtmlConverter, PostConverter, Processors, Transformer}
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.{FrontMatter, PageContent}
import org.podval.tools.publish.util.Files
import org.podval.xml.{HtmlClass, Xml, XmlDialect, XmlElement, XmlParser}

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
  
  def sections(content: PageContent): Seq[Fragment.Section]

  def processors: Processors

  lazy val converters: Seq[Converter] = processors
    .processors
    .collect { case converter: Converter => converter }
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
    message: String,
    firstReading: Boolean,
  ): (FrontMatter, Xml.Element) =
    site.log.debug(s"$message: $sourcePath")

    val (frontMatterContent: Option[String], content: String) =
      FrontMatter.split(Files.read(sourcePath.file(site.sourceDirectory)))

    val frontMatter: FrontMatter = FrontMatter.parse(frontMatterContent) match
      case Right(frontMatter) =>
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
          .element(XmlElement(xmlDialect.root.head))
          .add(HtmlClass(s"malformed-$extension"))
          .setText(s"Malformed $name: $error")

    (frontMatter, xml)

object Markup:
  val xmlLike: List[XmlLikeMarkup] = List(
    TeiMarkup
  )

  // Some XmlLike markups can have extensions other than `.xml`, so we add `xmlLike` to `all`:
  val all: List[Markup] = xmlLike ++ List(
    MarkdownMarkup,
    HtmlMarkup
  )
