package org.podval.tools.publish

import org.podval.tools.publish.js
import org.podval.tools.publish.util.{Date, Icon}
import org.podval.xml.Html
import zio.blocks.html.*
import zio.blocks.html.Dom.Element

// Based on https://github.com/jekyll/minima
object Minima:
  def render(
    page: MarkupPage,
    markupContent: Option[Html.Element],
    syntheticContent: Option[Html.Element]
  ): Html.Element =
    def site: Site = page.site
    
    def getLanguages(element: Html.Element): Seq[String] =
      if Html.Code.is(element)
      then Html.ClassName.getStartsWith(element, "language")
      else Html.flatMapChildren(element, getLanguages)

    val languages: Set[String] = markupContent.fold(Set.empty)(getLanguages(_).toSet)
    val languagesToHighlight: Set[String] = languages - "mermaid"

    val htmlContent: Seq[Html.Element] = Seq(markupContent, syntheticContent).flatten

    val libraries: List[js.JSLibrary] = List(
      Option.when(languagesToHighlight.nonEmpty)(js.Highlights(languages)),
      Option.when(page.math)(js.MathJax),
      Some(js.FontAwesome),
      Option.when(languages.contains("mermaid"))(js.Mermaid),
      if !site.production then None else site.config.googleAnalytics.map(js.GoogleAnalytics(_)),
      Some(site)
    ).flatten

    html(lang := page.lang,
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        link(rel := "sitemap", `type` := "application/xml", titleAttr := "Sitemap", href := Sitemap.path.toString),
        // TODO {%- seo -%}: https://github.com/jekyll/jekyll-seo-tag
        title(page.title),
        libraries.flatMap(library => library.stylesheet.map(ref =>
          link(rel := "stylesheet", href := s"${library.cdn}$ref")
        )),
        // TODO {%- feed_meta -%}: https://github.com/jekyll/jekyll-feed
      ),
      body(
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
                page.site.headerPages.map(_.page.ref()),
                page.parent.flatMap(parent => Option.when(parent.parent.isDefined)(navRef(parent, Icon.arrowUp))),
                page.parent.flatMap(_.prev(page)).collect { case page: MarkupPage => page }.map(navRef(_, Icon.arrowLeft)),
                page.parent.flatMap(_.next(page)).collect { case page: MarkupPage => page }.map(navRef(_, Icon.arrowRight))
              )
            )
          )
        ),
        main(className := "page-content", aria("label") := "Content",
          div(className := "wrapper",
            article(className := "post h-entry", itemScope := true, itemType := "http://schema.org/BlogPosting",
              header(className := "post-header",
                postPath(page),
                h1(className := "post-title p-name", itemProp := "name headline", page.title),
                Option.when(!page.hasSyntheticContent)(articleMeta(page))
              ),
              div(className := "post-content e-content", itemProp := "articleBody", htmlContent),
              a(className := "u-url", href := page.path.toString, hidden := true)
            ),
            site.backLinks.html(page)
          ),
        ),
        footer(className  := "site-footer h-card",
          data(className := "u-url", href := "/"),
          div(className := "wrapper",
            h2(className := "footer-heading", site.config.title),
            div(className := "footer-col-wrapper",
              div(className := "footer-col footer-col-1",
                ul(className := "contact-list",
                  li(className := "p-name", site.config.author),
                  li(a(className := "u-email", href := s"mailto:${site.config.email}", site.config.email))
                )
              ),
              div(className := "footer-col footer-col-2",
                div(className := "social-links",
                  ul(className := "social-media-list", site.socialLinks.map(social =>
                    li(
                      a(
                        rel := "me", 
                        href := social.href,
                        target := "_blank",
                        titleAttr := social.title,
                        Icon.brand(social.icon).html,
                        span(className := "username", social.userName)
                      )
                    )
                  ))
                )
              ),
              div(className := "footer-col footer-col-3",
                p(site.config.description),
                p(
                  a(
                    href := Feed.path.toString,
                    Icon.rss.html,
                    span(className := "rss-feed", "RSS feed")
                  )
                )
              )
            )
          )
        ),
        libraries.flatMap(library => library.imports.map(externalJs =>
          // Note: `defer` here is crucial for MathJax: without it, some math renders incorrectly, with `$`s visible...
          script(defer := true).externalJs(s"${library.cdn}/$externalJs")
        )),
        libraries.flatMap(library => library.inlineJs.map(js =>
          // Note: `script` does *not* accept optional attributes; TODO bug?
          val scriptTag: Element.Script =
            if library.isModule
            then script(`type` := "module")
            else script()
          scriptTag.inlineJs(js)
        ))
      )
    )

  private def postPath(page: MarkupPage): Html.Element =
    def parents(page: MarkupPage): Seq[MarkupPage] = page.parent match
      case None => Seq.empty
      case Some(parent) => parents(parent) :+ parent

    val pathFull: Seq[MarkupPage] = parents(page)
    val path: Seq[MarkupPage] = if pathFull.isEmpty then pathFull else pathFull.tail
    span(className := "post-path", path.map(page => span("/", page.ref(withIcon = false))))

  private def articleMeta(page: MarkupPage): Html.Element =
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

  private def navRef(page: Page, icon: Icon) = page.ref(
    cls = Some("nav-item"),
    icon = Some(icon),
    withTitle = false
  )

      
