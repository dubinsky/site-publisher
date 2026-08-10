package org.podval.tools.publish.markup

import org.podval.tei.EntityKind
import org.podval.tools.publish.page.{FrontMatter, MarkupPage, Page, PageSource}
import org.podval.tools.publish.site.{PageError, Path, Site}
import org.podval.tools.publish.util.{Date, Files}
import org.podval.xml.{Html, Xml, XmlDialect, XmlEncode, XmlParser}
import zio.blocks.html.*
import java.io.File

// TODO because of the cross-markup transclusion,
// all markup-specific stylesheets need to be always included;
// in fact, MathJax and friends too...
// unless we actually calculate the set of markup languages used in a page ;)
abstract class Markup(
  final val name: String,
  // TODO use xmlDialect.plus(HtmlXmlDialect) for processing/printing
  // and xmlDialect for pretty-printing.
  final val xmlDialect: XmlDialect,
  final val extension: String,
  additionalExtensions: Set[String] = Set.empty,
  rendersToXml: Boolean
) derives CanEqual:
  final override def toString: String = name

  final val extensions: Set[String] = additionalExtensions + extension

  def rootElements: Set[String] = Set.empty

  def xmlContent(content: String, sourceFile: File, site: Site): String

  // Process raw parsed XML:
  // - clean it up (AsciiDoc div soup etc.)
  // - nest HTML sections
  // - convert footnotes into common format
  // - extract title
  def process(source: PageSource, xml: Xml.Element): (Xml.Element, Option[Xml.Element])

  def isSpuriousFootnotesDiv(element: Xml.Element): Boolean = false

  def entityKind(xml: Xml.Element): Option[EntityKind] = None

  def isTocPlaceholder(element: Html.Element): Boolean = false

  def pageHeader(page: MarkupPage): Html.Element = Markup.pageHeader(page)

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
    // TODO error if both are present
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

    val xmlString: String = xmlContent(content, sourceFile, site)

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

  