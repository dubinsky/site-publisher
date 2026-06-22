package org.podval.tools.publish.page

import org.podval.tools.publish.{Path, Site, Sitemap}
import org.podval.tools.publish.js
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, HtmlElement, HtmlXmlDialect}
import zio.blocks.chunk.Chunk
import zio.blocks.html.{content as contentAttribute, lang as langAttribute, title as titleElement, *}
import zio.blocks.html.Dom.Element.Script

abstract class MarkupPage(site: Site, path: Path) extends RealPage(site, path) with PageWithContent:
  override def titleDefault: String = path.fileName

  private var sourceVar: Option[PageSource] = None
  final override def source: Option[PageSource] = sourceVar
  def setSource(source: PageSource): Unit = this.sourceVar = Some(source)

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]

  // TODO use markup.xmlDialect?
  final override def textContent: String = HtmlXmlDialect.render(toHtml)

  // Based on https://github.com/jekyll/minima
  private def toHtml: Html.Element =
    val pageHeader: Option[Html.Element] = content.map(content => content.markup.pageHeader(content))
    val markupContent: Option[Html.Element] = content.map(_.toHtml)
    val syntheticContent: Option[Html.Element] = syntheticContentOpt

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
      if !site.production then None else site.config.googleAnalytics.map(js.GoogleAnalytics(_)),
      Some(site)
    ).flatten

    html(langAttribute := lang,
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", contentAttribute := "IE=edge"),
        meta(name := "viewport", contentAttribute := "width=device-width, initial-scale=1"),
        link(rel := "sitemap", `type` := "application/xml", titleAttr := "Sitemap", href := Sitemap.path.toString),
        // TODO {%- seo -%}: https://github.com/jekyll/jekyll-seo-tag
        titleElement(title),
        libraries.flatMap(library => library.stylesheet.map(ref =>
          link(rel := "stylesheet", href := s"${library.cdn}$ref")
        )),
        // TODO {%- feed_meta -%}: https://github.com/jekyll/jekyll-feed
      ),
      body(
        // TODO move to Site
        header(className := "site-header",
          div(className := "wrapper",
            a(className := "site-title", href := "/", rel := "author", site.config.title),
            nav(className := "site-nav",
              input(`type` := "checkbox", id := "nav-trigger"),
              label(`for` := "nav-trigger",
                span(className := "menu-icon",
                  //                el("svg", "viewBox" -> "0 0 18 15", "width" -> "18px", "height" -> "15px")(
                  //                  el("path", "d" -> "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")()
                  //                )
                )
              ),
              div(className := "nav-items",
                site.pages.headerPages.map(_.page.ref()),
                parent.flatMap(parent => Option.when(parent.parent.isDefined)(parent.navRef(Icon.arrowUp))),
                parent.flatMap(_.prev(this)).map(_.navRef(Icon.arrowLeft)),
                parent.flatMap(_.next(this)).map(_.navRef(Icon.arrowRight))
              )
            )
          )
        ),
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
