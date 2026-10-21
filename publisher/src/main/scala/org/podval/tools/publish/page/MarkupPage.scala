package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.js
import org.podval.tools.publish.markup.Facsimile
import org.podval.tools.publish.site.{Feed, Messages, Path, Seo, Site, Sitemap}
import org.podval.tools.publish.util.Icon
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlAttribute, XmlElement}
import org.podval.xml.dsl.{*, given}

abstract class MarkupPage(site: Site, path: Path) extends Page(site, path) with PageWithContent:
  override def titleDefault: String = path.fileName

  /** The code stored before conversion. Bibliography calls this during conversion; reading content again would recurse. */
  final def lang: String =
    val fromSite: String = MarkupPage.languageCode(site.config.lang).getOrElse("en")
    source.fold(fromSite) { pageSource =>
      pageSource.documentCode.getOrElse {
        val _ = pageSource.content
        pageSource.documentCode.getOrElse(fromSite)
      }
    }

  final def interfaceCode: String = site.languageSpec.language.get.name

  final def interfaceDir: String =
    if interfaceCode == "he" || interfaceCode == "yi" then "rtl" else "ltr"

  final def languagesDiffer: Boolean = lang != interfaceCode

  /** Interface `lang` and `dir` when the document is a different language. */
  final def withInterfaceLang(element: Xml.Element): Xml.Element =
    if !languagesDiffer then element
    else element.set(XmlAttribute.Lang, interfaceCode).set("dir", interfaceDir)

  /** Document `lang` when it differs from the interface. */
  final def withDocumentLang(element: Xml.Element): Xml.Element =
    if !languagesDiffer then element
    else element.set(XmlAttribute.Lang, lang)

  private var sourceVar: Option[PageSource] = None
  final def setSource(source: PageSource): Unit = this.sourceVar = Some(source)
  final override def source: Option[PageSource] = sourceVar

  // One-list prose. `EntityListsContent` is rebuilt when its soft cache misses, so the file cannot live there.
  private var inlineListProseVar: Option[PageSource] = None
  final def inlineListProse: Option[PageSource] = inlineListProseVar
  final def setInlineListProse(source: PageSource): Unit = inlineListProseVar = Some(source)

  def prev: Option[Page]
  def next: Option[Page]

  def pagerPrev: Option[Page] = None
  def pagerNext: Option[Page] = None

  def hasSyntheticContent: Boolean = false

  protected def extraLibraries: List[js.JSLibrary] = Nil

  protected def syntheticContentOpt: Option[Xml.Element] = None

  final override def textContent: String = htmlString(markupContent, syntheticContentOpt)

  protected def htmlString(
    markup: Option[Xml.Element],
    synthetic: Option[Xml.Element]
  ): String =
    HtmlXmlWriterConfig.render(pageRoot(
      pageHeader = pageHeader,
      markupContent = markup,
      syntheticContent = synthetic
    ))

  def markupContent: Option[Xml.Element]

  final def markupContent(
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Xml.Element] = content.flatMap(_.markupContent(sectionId, isTerminal))

  def pageHeader: Option[Xml.Element]

  // Other HTML/PDF views of the same document (site-header icons). Empty unless this
  // is a FullMarkupPage or one of its chunks.
  protected def formatSourcePage: Option[FullMarkupPage] = None
  protected def formatIsChunked: Boolean = false
  protected def formatIsFacsimile: Boolean = false
  protected def isFacsimileViewer: Boolean = false

  /** Owning full transcription only. Chunks and the viewer stay without a pane. */
  protected def facsimileAside: Option[Xml.Element] = None

  final def formatLinks: Seq[Xml.Element] = formatSourcePage.toSeq.flatMap: page =>
    given Language.Spec = site.languageSpec
    val onePage: Option[Xml.Element] =
      if formatIsFacsimile then
        Some(formatLink(
          page.publishedPath.toString,
          Icon.fileLines,
          Messages.text("format.transcription"),
          NamedWindows.targetAttr(page).orElse(Some(Facsimile.textTarget))
        ))
      else Option.when(formatIsChunked)(
        formatLink(page.publishedPath.toString, Icon.fileLines, Messages.text("format.onePage"))
      )
    val chunked: Option[Xml.Element] =
      Option.when(!formatIsChunked && !formatIsFacsimile && page.chunk)(
        formatLink(page.publishedPath.add(DirectoryPage.fileName).html.toString, Icon.tableList, Messages.text("format.chunked"))
      )
    val pdf: Option[Xml.Element] = Option.when(page.pdf)(
      formatLink(page.path.withExtension(PdfPage.extension).toString, Icon.pdf, "PDF")
    )
    val facsimile: Option[Xml.Element] = Option.when(!formatIsFacsimile)(
      site.pages.facsimilePage(page).map: viewer =>
        formatLink(
          viewer.publishedPath.toString,
          Icon.images,
          site.terms.heading("facsimile"),
          NamedWindows.targetAttr(viewer).orElse(Some(Facsimile.facsimileTarget))
        )
    ).flatten
    Seq(onePage, chunked, pdf, facsimile).flatten

  final def translationLinks: Seq[Xml.Element] =
    val page: MarkupPage = formatSourcePage.getOrElse(this)
    if CollectionIndex.isReading(page) then Seq.empty
    else site.pages.alternates.others(page).map: member =>
      a(
        className := "nav-item translation",
        href := member.page.publishedPath.toString,
        langAttr := member.code,
        attr("hreflang") := member.code,
        NamedWindows.targetAttr(member.page).map(name => target := name),
        Alternates.autonym(member.code)
      )

  /** On the original only, after the language links. Not an autonym and not `hreflang`. */
  final def readingLinks: Seq[Xml.Element] =
    val page: MarkupPage = formatSourcePage.getOrElse(this)
    CollectionIndex.readingOf(page).toSeq.map: reading =>
      a(
        className := "nav-item reading",
        href := reading.publishedPath.toString,
        NamedWindows.targetAttr(reading).map(name => target := name),
        "[reg]"
      )

  private def formatLink(
    url: String,
    icon: Icon,
    label: String,
    target: Option[String] = None
  ): Xml.Element =
    a(
      className := "nav-item page-format",
      href := url,
      titleAttr := label,
      aria("label") := label,
      target.map(name => attr("target") := name),
      icon.html
    )

  // Based on https://github.com/jekyll/minima
  private def pageRoot(
    pageHeader: Option[Xml.Element],
    markupContent: Option[Xml.Element],
    syntheticContent: Option[Xml.Element]
  ): Xml.Element =
    def getLanguages(element: Xml.Element): Seq[String] =
      if element.isElement(XmlElement.Code)
      then element.getPrefixedClasses("language")
      else element.flatMapElements(getLanguages)

    given Language.Spec = site.languageSpec
    val languages: Set[String] = markupContent.fold(Set.empty)(getLanguages(_).toSet)
    val languagesToHighlight: Set[String] = languages - "mermaid"

    val articleBody: Seq[Xml.Element] = Seq(markupContent, syntheticContent).flatten
    val companion: Option[Xml.Element] = facsimileAside
    // `className :=` replaces, so wide and facsimile share one string with the aside.
    val htmlClass: String = Seq(
      Option.when(isCollectionIndex)("wide"),
      Option.when(isFacsimileViewer)("facsimile"),
      Option.when(companion.isDefined)("facsimile-aside")
    ).flatten.mkString(" ")

    val libraries: List[js.JSLibrary] =
      List(
        Option.when(languagesToHighlight.nonEmpty)(js.Highlights(languagesToHighlight)),
        Some(js.MathJax),
        Some(js.FontAwesome),
        Option.when(languages.contains("mermaid"))(js.Mermaid),
        site.googleAnalytics.map(js.GoogleAnalytics(_))
      ).flatten ++ extraLibraries :+ site

    html(
      langAttr := lang,
      attr("dir") := interfaceDir,
      attr("data-interface-lang") := interfaceCode,
      head(
        meta(charset := "utf-8"),
        meta(name := "viewport", contentAttr := "width=device-width, initial-scale=1"),
        Seo.head(this),
        site.favicon,
        site.license,
        Sitemap.sitemapLink,
        Feed.feedMeta(site),
        pagerPrev.map(p => link(rel := "prev", href := p.path.toString)),
        pagerNext.map(p => link(rel := "next", href := p.path.toString)),
        libraries.flatMap(_.styleLinks),
        libraries.flatMap(_.headScripts)
      ),
      body(
        site.siteHeader(this),
        main(className := "page-content", aria("label") := Messages.text("nav.content"),
          div(className := "wrapper",
            article(className := "page",
              pageHeader,
              withDocumentLang(div(className := "page-body",
                articleBody ++ site.categories.html(this).toSeq
              ))
            ),
            site.backLinks.html(this),
            site.transclusions.html(this)
          ),
          companion
        ),
        site.siteFooter(this),
        libraries.flatMap(_.scripts)
      )
    ).when(htmlClass.nonEmpty)(className := htmlClass)
     .when(site.config.namedWindows)(attr("data-window-name") := NamedWindows.of(this))
     .when(site.config.apparatus)(attr("data-apparatus-enabled") := "true")

  private def isCollectionIndex: Boolean =
    doc.exists(_.wide)

object MarkupPage:
  /** Known names become the enum code. An unknown token is kept whole. */
  private[page] def languageCode(raw: Option[String]): Option[String] =
    raw.map(_.trim).filter(_.nonEmpty).map: token =>
      val primary: String = token.takeWhile(c => c != '-' && c != '_')
      Language.forName(token).orElse(Language.forName(primary)).map(_.name).getOrElse(token)
