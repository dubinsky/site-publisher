package org.podval.tools.publish.page

import org.podval.tools.publish.js
import org.podval.tools.publish.site.{Path, Site, Sitemap}
import org.podval.xml.{Html, HtmlElement, HtmlXmlDialect}
import zio.blocks.chunk.Chunk
import zio.blocks.html.{content as contentAttribute, lang as langAttribute, title as titleElement, *}
import zio.blocks.html.Dom.Element.Script

abstract class MarkupPage(site: Site, path: Path) extends RealPage(site, path) with PageWithContent:
  override def titleDefault: String = path.fileName

  def prev: Option[Page]
  def next: Option[Page]

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]

  // TODO use markup.xmlDialect?
  final override def textContent: String = HtmlXmlDialect.render(toHtml(
    pageHeader = pageHeader,
    markupContent = markupContent,
    syntheticContent = syntheticContentOpt
  ))

  def markupContent: Option[Html.Element]
  
  def pageHeader: Option[Html.Element]

  // Based on https://github.com/jekyll/minima
  private def toHtml(
    pageHeader: Option[Html.Element],
    markupContent: Option[Html.Element],
    syntheticContent: Option[Html.Element]
  ): Html.Element =
    def getLanguages(element: Html.Element): Chunk[String] =
      if element.isElement(HtmlElement.Code)
      then element.getPrefixedClasses("language")
      else element.flatMapElements(getLanguages)

    val languages: Set[String] = markupContent.fold(Set.empty)(getLanguages(_).toSet)
    val languagesToHighlight: Set[String] = languages - "mermaid"

    val articleBody: Seq[Html.Element] = Seq(markupContent, syntheticContent).flatten

    val libraries: List[js.JSLibrary] = List(
      Option.when(languagesToHighlight.nonEmpty)(js.Highlights(languages)),
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
        titleElement(title),
        Sitemap.sitemapLink,
        // TODO {%- seo -%}: https://github.com/jekyll/jekyll-seo-tag
        // TODO {%- feed_meta -%}: https://github.com/jekyll/jekyll-feed
        libraries.flatMap(library => library.stylesheet.map(ref =>
          link(rel := "stylesheet", href := s"${library.cdn}$ref")
        ))
      ),
      body(
        site.siteHeader(this),
        main(className := "page-content", aria("label") := "Content",
          div(className := "wrapper",
            article(className := "post h-entry", itemScope := true, itemType := "http://schema.org/BlogPosting",
              pageHeader,
              div(className := "post-content e-content", itemProp := "articleBody", articleBody),
              a(className := "u-url", href := path.toString, hidden := true)
            ),
            site.backLinks.html(this)
          ),
        ),
        site.siteFooter,
        libraries.flatMap(library => library.imports.map(externalJs =>
          // Note: `defer` here is crucial for MathJax: without it, some math renders incorrectly, with `$`s visible...
          script(defer := true).externalJs(s"${library.cdn}/$externalJs")
        )),
        libraries.flatMap(library => library.inlineJs.map(js =>
          // Note: `script` does *not* accept optional attributes; TODO bug?
          val scriptTag: Script = if library.isModule then script(`type` := "module") else script()
          scriptTag.inlineJs(js)
        ))
      )
    )
