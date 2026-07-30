package org.podval.tools.publish.markup

import org.podval.tei.EntityKind
import org.podval.tools.publish.asciidoc.AsciiDocMarkup
import org.podval.tools.publish.html.HtmlMarkup
import org.podval.tools.publish.link.{Fragment, Toc}
import org.podval.tools.publish.markdown.MarkdownMarkup
import org.podval.tools.publish.page.{FrontMatter, MarkupPage, Page, PageSource}
import org.podval.tools.publish.site.{PageError, Path, Site}
import org.podval.tools.publish.tei.TeiMarkup
import org.podval.tools.publish.util.{Date, Files, IdGenerator}
import org.podval.xml.{Html, Xml, XmlDialect, XmlEncode, XmlParser}
import zio.blocks.html.*

// TODO make this a JS library too, to install markup-specific stylesheet
abstract class Markup(
  final val name: String,
  // TODO use xmlDialect.plus(HtmlXmlDialect) for processing/printing
  // and xmlDialect for pretty-printing.
  final val xmlDialect: XmlDialect,
  allowsInternalFrontMatter: Boolean,
  final val extension: String,
  additionalExtensions: Set[String] = Set.empty,
  rendersToXml: Boolean
) derives CanEqual:
  final override def toString: String = name

  final val extensions: Set[String] = additionalExtensions + extension

  def rootElements: Set[String] = Set.empty

  def xmlContent(
    site: Site,
    sourcePath: Path,
    content: String
  ): String

  def converters(
    ids: IdGenerator,
    source: PageSource
  ): Seq[Converter]

  def postConverters(
    source: PageSource
  ): Seq[Converter] =
    Seq.empty

  def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element])

  def sections(source: PageSource, xml: Xml.Element): Seq[Fragment.Section]

  def isSpuriousFootnotesDiv(element: Xml.Element): Boolean = false

  def entityKind(xml: Xml.Element): Option[EntityKind] = None

  def isTocPlaceholder(element: Html.Element): Boolean = false

  def pageHeader(page: MarkupPage): Html.Element = Markup.pageHeader(page)

  final def readAndParse(
    site: Site,
    sourcePath: Path,
    standAloneFrontMatter: Option[Path],
    message: String,
    firstReading: Boolean,
  ): (FrontMatter, Xml.Element) =
    site.log.debug(s"$message: $sourcePath")

    val frontMatterContentStandAlone: Option[String] = standAloneFrontMatter
      .map(site.sourceFile)
      .map(Files.read)

    val (frontMatterContentInternal: Option[String], content: String) =
      val content: String = Files.read(site.sourceFile(sourcePath))
      if allowsInternalFrontMatter
      then FrontMatter.split(content)
      else (None, content)

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

    val xmlString: String = xmlContent(
      site,
      sourcePath,
      content
    )

    val xml: Xml.Element = (if rendersToXml then XmlParser.parseXml(xmlString) else XmlParser.parseHtml(xmlString)) match
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

  final def process(source: PageSource, xml: Xml.Element): Xml.Element =
    // Run converters
    val converter: Converter =
      // TODO shove it into PageContent?
      val ids: IdGenerator = IdGenerator("_generated_id")
      Converter.concat(
        converters(
          ids = ids,
          source
        ) ++
        // Converters that convert links need to run after everything that was to become a link had.
        Seq(
          AnchorIdsConverter(ids),
          InternalLinksConverter(source)
        )
      )

    val converted: Xml.Element = xmlDialect.transform(xml, converter.doConvert)

    // Run transformers
    FootnotesTransformer(this).transform(converted, source)

  def postProcess(source: PageSource, xml: Xml.Element): Xml.Element =
    // Run post-converters
    val postConverter: Converter = Converter.concat(
      postConverters(source) ++ Seq(
        InternalLinksPostConverter(source)
      )
    )

    xmlDialect.transform(xml, postConverter.doConvert)

  def section(xml: Xml.Element, sectionId: String, toc: Toc): Xml.Element
  
object Markup:
  // Known markup languages.
  // Note: some XmlLike markups can have extensions other than `.xml`.
  lazy val all: Seq[Markup] = Seq(
    HtmlMarkup,
    MarkdownMarkup,
    AsciiDocMarkup,
    TeiMarkup
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

  def pageHeader(page: MarkupPage): Html.Element =
    header(className := "post-header",
      postPath(page),
      h1(className := "post-title p-name", itemProp := "name headline", page.title),
      Option.when(!page.hasSyntheticContent)(articleMeta(page))
    )

  private def postPath(page: Page): Html.Element =
    def parents(page: Page): Seq[Page] = page.parent match
      case None => Seq.empty
      case Some(parent) => parents(parent) :+ parent

    val pathFull: Seq[Page] = parents(page)
    val path: Seq[Page] = if pathFull.isEmpty then pathFull else pathFull.tail
    span(className := "post-path", path.map(page => span("/", page.ref(withIcon = false))))

  private def articleMeta(page: Page): Html.Element =
    div(className := "post-meta",
      join(
        join(
          join(
            timeHtml(Option.when(page.dateModified.nonEmpty)("Published:"), page.date, "dt-published", "datePublished"),
            "•",
            timeHtml(Some("Updated:"), page.dateModified, "dt-modified", "dateModified")
          ),
          "•",
          page.author.fold(Seq.empty): author =>
            Seq(
              span(className := "post-authors",
                span(className := "post-author", itemProp := "author", itemScope := true, itemType := "http://schema.org/Person",
                  span(className := "p-author h-card", itemProp := "name", author)
                )
              )
            )
        ),
        "|",
        page.tags.map(page.site.tags.tagRef)
      )
    )

  private def join(left: Seq[Html.Element], text: String, right: Seq[Html.Element]): Seq[Html.Element] =
    if left.nonEmpty && right.nonEmpty
    then left ++ Seq(span(className := "bullet-divider", text)) ++ right
    else left ++ right

  private def timeHtml(label: Option[String], date: Option[Date], cls: String, itemprop: String): Seq[Html.Element] =
    date.fold(Seq.empty): date =>
      label.fold(Seq.empty)(label => Seq(span(className := "meta-label", label))) ++
        Seq(time(className := cls, datetime := date.toString, itemProp := itemprop, date.toShortString))

  