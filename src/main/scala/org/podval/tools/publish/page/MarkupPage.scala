package org.podval.tools.publish.page

import org.podval.tools.publish.js
import org.podval.tools.publish.markup.Facsimile
import org.podval.tools.publish.site.{Feed, Path, Seo, Site, Sitemap}
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, HtmlXmlDialect, XmlElement}
import zio.blocks.html.{content as contentAttribute, lang as langAttribute, *}

abstract class MarkupPage(site: Site, path: Path) extends Page(site, path) with PageWithContent:
  override def titleDefault: String = path.fileName

  final def math: Boolean = site.config.math || frontMatter.math

  final def lang: String = content(_.frontMatter.lang).orElse(langDefault).orElse(site.config.lang).getOrElse("en")
  // TODO set to "en" and clean up overrides
  protected def langDefault: Option[String] = None

  private var sourceVar: Option[PageSource] = None
  final def setSource(source: PageSource): Unit = this.sourceVar = Some(source)
  final override def source: Option[PageSource] = sourceVar

  def prev: Option[Page]
  def next: Option[Page]

  def pagerPrev: Option[Page] = None
  def pagerNext: Option[Page] = None

  def hasSyntheticContent: Boolean = false

  protected def syntheticContentOpt: Option[Html.Element] = None

  // TODO use markup.xmlDialect?
  final override def textContent: String = htmlString(markupContent, syntheticContentOpt)

  protected def htmlString(
    markup: Option[Html.Element],
    synthetic: Option[Html.Element]
  ): String =
    HtmlXmlDialect.render(toHtml(
      pageHeader = pageHeader,
      markupContent = markup,
      syntheticContent = synthetic
    ))

  def markupContent: Option[Html.Element]

  final def markupContent(
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Html.Element] = content.flatMap(_.markupContent(sectionId, isTerminal))

  // TODO maybe remove this in favour of PageHeader?
  def pageHeader: Option[Html.Element]

  // Other HTML/PDF views of the same document (site-header icons). Empty unless this
  // is a FullMarkupPage or one of its chunks.
  protected def formatSourcePage: Option[FullMarkupPage] = None
  protected def formatIsChunked: Boolean = false
  protected def formatIsFacsimile: Boolean = false
  protected def isFacsimileViewer: Boolean = false

  final def formatLinks: Seq[Html.Element] = formatSourcePage.toSeq.flatMap: page =>
    val onePage: Option[Html.Element] =
      if formatIsFacsimile then
        Some(formatLink(
          page.publishedPath.toString,
          Icon.fileLines,
          "Transcription",
          Some(Facsimile.textTarget)
        ))
      else Option.when(formatIsChunked)(
        formatLink(page.publishedPath.toString, Icon.fileLines, "One-page HTML")
      )
    val chunked: Option[Html.Element] =
      Option.when(!formatIsChunked && !formatIsFacsimile && page.chunk)(
        formatLink(page.publishedPath.add(DirectoryPage.fileName).html.toString, Icon.tableList, "Chunked HTML")
      )
    val pdf: Option[Html.Element] = Option.when(page.pdf)(
      formatLink(page.path.withExtension(PdfPage.extension).toString, Icon.pdf, "PDF")
    )
    val facsimile: Option[Html.Element] = Option.when(!formatIsFacsimile)(
      site.pages.facsimilePage(page).map: viewer =>
        formatLink(
          viewer.publishedPath.toString,
          Icon.images,
          "Facsimile",
          Some(Facsimile.facsimileTarget)
        )
    ).flatten
    Seq(onePage, chunked, pdf, facsimile).flatten

  final def translationLinks: Seq[Html.Element] =
    CollectionIndex.translationsToLink(formatSourcePage.getOrElse(this)).flatMap: translation =>
      CollectionIndex.langOf(translation).map: lang =>
        a(
          className := "nav-item translation",
          href := translation.publishedPath.toString,
          s"[$lang]"
        )

  private def formatLink(
    url: String,
    icon: Icon,
    label: String,
    target: Option[String] = None
  ): Html.Element =
    a(
      className := "nav-item page-format",
      href := url,
      titleAttr := label,
      aria("label") := label,
      target.map(name => attr("target") := name),
      icon.html
    )

  // Based on https://github.com/jekyll/minima
  private def toHtml(
    pageHeader: Option[Html.Element],
    markupContent: Option[Html.Element],
    syntheticContent: Option[Html.Element]
  ): Html.Element =
    def getLanguages(element: Html.Element): Seq[String] =
      if element.isElement(XmlElement.Code)
      then element.getPrefixedClasses("language")
      else element.flatMapElements(getLanguages)

    val languages: Set[String] = markupContent.fold(Set.empty)(getLanguages(_).toSet)
    val languagesToHighlight: Set[String] = languages - "mermaid"

    val articleBody: Seq[Html.Element] = Seq(markupContent, syntheticContent).flatten

    val libraries: List[js.JSLibrary] = List(
      Option.when(languagesToHighlight.nonEmpty)(js.Highlights(languagesToHighlight)),
      Option.when(math)(js.MathJax),
      Some(js.FontAwesome),
      Option.when(languages.contains("mermaid"))(js.Mermaid),
      site.googleAnalytics.map(js.GoogleAnalytics(_)),
      Some(site)
    ).flatten

    html(langAttribute := lang,
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", contentAttribute := "IE=edge"),
        meta(name := "viewport", contentAttribute := "width=device-width, initial-scale=1"),
        Seo.head(this),
        site.favicon,
        site.license,
        Sitemap.sitemapLink,
        Feed.feedMeta(site),
        pagerPrev.map(p => link(rel := "prev", href := p.path.toString)),
        pagerNext.map(p => link(rel := "next", href := p.path.toString)),
        libraries.flatMap(library => library.stylesheet.map(ref =>
          link(rel := "stylesheet", href := s"${library.cdn}$ref")
        )),
        libraries.flatMap(_.headInlineJs.map(code => script().inlineJs(code)))
      ),
      body(
        site.siteHeader(this),
        main(className := "page-content", aria("label") := "Content",
          div(className := "wrapper",
            article(className := "post h-entry", itemScope := true, itemType := s"http://schema.org/${Seo.schemaType(this)}",
              pageHeader,
              div(className := "post-content e-content", itemProp := "articleBody", articleBody),
              a(className := "u-url", href := path.toString, hidden := true)
            ),
            site.backLinks.html(this)
          ),
        ),
        site.siteFooter,
        libraries.flatMap(_.scripts)
      )
    ).when(isCollectionIndex)(className := "wide")
     .when(isFacsimileViewer)(className := "facsimile")

  private def isCollectionIndex: Boolean =
    doc.exists(_.wide)
