package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, Icon}
import org.podval.xml.{Html, HtmlXmlDialect, Xml}
import scala.ref.SoftReference

abstract class MarkupPage(site: Site, path: Path) extends Page.Real(site, path) with Page.WithContent:
  override def titleDefault: String = path.fileName

  private var sourceVar: Option[MarkupPage.Source] = None
  final override def source: Option[MarkupPage.Source] = sourceVar
  
  def setSource(
    markup: Markup,
    sourcePath: Path
  ): Unit = this.sourceVar = Some(MarkupPage.Source(
    site = site,
    markup = markup,
    sourcePath = sourcePath
  ))

  final override def sourcePath: Option[Path] = source.map(_.sourcePath)

  final override def backLinks: Seq[BackLinks.BackLink] = source.fold(Seq.empty): source =>
    val cached = source.cached
    source.markup.backLinks(
      cached.xml,
      this
    )

  final override def content: String =
    val markupContent: Option[Html.Element] = source.map: source =>
      val cached = source.cached
      source.markup.htmlContent(
        cached.xml,
        cached.toc,
        source.errorReporter,
        this
      )

    val html: Html.Element = Minima.render(
      page = this,
      markupContent = markupContent,
      syntheticContent = syntheticContentOpt
    )
    HtmlXmlDialect.render(html)

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]

object MarkupPage:
  final class Simple(site: Site, path: Path) extends MarkupPage(site, path) with Page.NonDirectory:
    override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note
    override def hasSyntheticContent: Boolean = false
    override protected def syntheticContentOpt: Option[Html.Element] = None

  abstract class WithSyntheticContent(site: Site, path: Path) extends MarkupPage(site, path):
    final override def hasSyntheticContent: Boolean = true
    final override protected def syntheticContentOpt: Option[Html.Element] = Some(syntheticContent)
    protected def syntheticContent: Html.Element

  final class Cached(
    val frontMatter: FrontMatter,
    val xml: Xml.Element,
    val toc: Toc
  )

  final class Source(
    val site: Site,
    val markup: Markup,
    val sourcePath: Path
  ):
    val errorReporter: PageError.Reporter = PageError.SiteReporter(sourcePath, site)

    private var cachedVar: Option[SoftReference[Cached]] = None

    def cached: Cached = cachedVar match
      case None => parse("Reading")
      case Some(reference) => reference.get match
        case None => parse("Re-reading evicted")
        case Some(cached) => cached

    private def parse(message: String): Cached =
      site.log.debug(s"$message MarkupSource: $sourcePath")

      val (frontMatterContent: Option[String], markupContent: String) =
        FrontMatter.split(Files.read(sourcePath.file(site.sourceDirectory)))

      val frontMatter: FrontMatter =  FrontMatter.parse(frontMatterContent, errorReporter)

      val xml: Xml.Element = markup.parseAndPreProcess(
        markupContent,
        errorReporter,
        site.config.url
      )

      val toc: Toc = markup.toc(xml, errorReporter)
      
      val result: Cached = Cached(
        frontMatter = frontMatter,
        xml = xml,
        toc = toc
      )

      cachedVar = Some(SoftReference(result))

      result

