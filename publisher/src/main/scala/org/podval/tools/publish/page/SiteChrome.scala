package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.site.{Config, Feed, Messages, ReaderFeatures, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl
import org.podval.xml.dsl.{*, given}

object SiteChrome:
  def header(site: Site, page: MarkupPage): Xml.Element =
    given Language.Spec = site.languageSpec
    val features: ReaderFeatures = site.readerFeatures
    val browse: Seq[Xml.Element] = Seq(
      page.up.flatMap(up => Option.when(up.up.isDefined)(up.navRef(Icon.arrowUp))),
      page.prev.map(_.navRef(Icon.arrowLeft)),
      page.next.map(_.navRef(Icon.arrowRight))
    ).flatten
    val views: Seq[Xml.Element] = page.formatLinks ++ page.translationLinks ++ page.readingLinks
    page.withInterfaceLang(dsl.header(className := "site-header",
      div(className := "wrapper",
        a(
          className := "site-title",
          href := "/",
          rel := "author",
          NamedWindows.hierarchyTarget(site.config).map(name => target := name),
          site.config.title
        ),
        Option.when(site.config.search.enabled)(headerSearch(site)),
        button(
          className := "nav-toggle",
          `type` := "button",
          aria("expanded") := "false",
          aria("controls") := "site-nav",
          aria("label") := Messages.text("nav.menu"),
          span(className := "nav-toggle-icon")
        ),
        nav(id := "site-nav", className := "site-nav",
          navGroup("nav-pages", site.pages.headerPages.map(_.ref())),
          navGroup("nav-browse", browse),
          navGroup("nav-views", views)
        ),
        details(className := "site-settings",
          summary(
            className := "site-settings-toggle",
            titleAttr := Messages.text("nav.settings"),
            aria("label") := Messages.text("nav.settings"),
            Icon.gear.html
          ),
          div(className := "site-settings-menu", role := "group", aria("label") := Messages.text("nav.settings"),
            div(className := "site-settings-section", colorSchemeSetting(site)),
            Option.when(features.inText)(
              div(
                className := "site-settings-section",
                role := "group",
                aria("labelledby") := "setting-in-text-label",
                div(
                  className := "site-settings-heading",
                  id := "setting-in-text-label",
                  Messages.text("settings.in-text")
                ),
                Option.when(features.terms)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-glossary-expand",
                      attr("data-setting") := "glossary-expand"
                    ),
                    Messages.text("settings.glossary")
                  )
                ),
                Option.when(features.choice)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-abbr-expand",
                      attr("data-setting") := "abbr-expand",
                      attr("checked") := "checked"
                    ),
                    Messages.text("settings.editorial")
                  )
                )
              )
            ),
            Option.when(features.layout)(
              div(className := "site-settings-section",
                Option.when(features.transclusion)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-transclusion-clean",
                      attr("data-setting") := "transclusion-clean"
                    ),
                    Messages.text("settings.transclusion")
                  )
                ),
                Option.when(features.facsimile)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-facsimile-aside",
                      attr("data-setting") := "facsimile-aside",
                      attr("checked") := "checked"
                    ),
                    Messages.text("settings.facsimile")
                  )
                )
              )
            ),
            Option.when(features.apparatus.nonEmpty)(
              div(
                className := "site-settings-section",
                features.apparatus.map(kind => apparatusSetting(site, kind, Messages.text(s"settings.$kind")))
              )
            )
          )
        )
      )
    ))

  def footer(site: Site): Xml.Element =
    given Language.Spec = site.languageSpec
    dsl.footer(className := "site-footer",
      div(className := "wrapper",
        div(className := "footer-col-wrapper",
          div(className := "footer-col",
            ul(className := "contact-list",
              li(site.config.author),
              li(a(href := s"mailto:${site.config.email}", site.config.email))
            )
          ),
          div(className := "footer-col",
            div(className := "social-links",
              ul(className := "social-media-list", site.socialLinks.map(social =>
                li(a(
                  rel := "me",
                  href := social.href,
                  target := "_blank",
                  titleAttr := social.title,
                  Icon.brand(social.icon).html,
                  span(className := "username", social.userName)
                ))
              ))
            )
          ),
          div(className := "footer-col",
            div(className := "footer-about",
              div(className := "footer-description", site.config.description),
              Feed.feedFooter,
              site.config.license.map(footerLicense)
            )
          )
        )
      )
    )

  def footer(site: Site, page: MarkupPage): Xml.Element =
    page.withInterfaceLang(footer(site))

  private def headerSearch(site: Site): Xml.Element =
    given Language.Spec = site.languageSpec
    val accessible: String = Messages.text("nav.search")
    element(
      "form",
      className := "header-search",
      attr("action") := "/search",
      attr("method") := "get",
      role := "search",
      button(
        `type` := "submit",
        aria("label") := accessible,
        Icon("magnifying-glass", Icon.Solid).html
      ),
      input(
        `type` := "search",
        attr("name") := "q",
        aria("label") := accessible
      )
    )

  private def colorSchemeSetting(site: Site): Xml.Element =
    given Language.Spec = site.languageSpec
    div(
      className := "site-settings-choice",
      role := "radiogroup",
      aria("labelledby") := "setting-color-scheme-label",
      span(id := "setting-color-scheme-label", className := "site-settings-choice-label", Messages.text("settings.color")),
      div(
        className := "site-settings-radios",
        colorChoice("system", Messages.text("settings.system"), selected = true),
        colorChoice("light", Messages.text("settings.light"), selected = false),
        colorChoice("dark", Messages.text("settings.dark"), selected = false)
      )
    )

  private def colorChoice(value: String, text: String, selected: Boolean): Xml.Element =
    label(
      input(
        `type` := "radio",
        attr("name") := "color-scheme",
        attr("value") := value
      ).when(selected)(attr("checked") := "checked"),
      text
    )

  private def apparatusSetting(site: Site, kind: String, groupLabel: String): Xml.Element =
    val setting: String = s"apparatus-$kind"
    div(
      className := "site-settings-choice",
      role := "radiogroup",
      // Not `data-apparatus`: that attribute is the prose mark, and those rules would wrap this group.
      attr("data-apparatus-kind") := kind,
      aria("labelledby") := s"setting-$setting-label",
      span(id := s"setting-$setting-label", className := "site-settings-choice-label", groupLabel),
      div(
        className := "site-settings-radios",
        Seq("distinct", "plain", "hidden").map: value =>
          label(
            input(
              `type` := "radio",
              attr("name") := setting,
              attr("value") := value
            ).when(value == "distinct")(attr("checked") := "checked"),
            apparatusLabel(site, kind, value)
          )
      )
    )

  /** Distinct shows the marks the text uses. Additions `[ ]`, deletions struck through, sources `< >`. */
  private def apparatusLabel(site: Site, kind: String, value: String): String =
    given Language.Spec = site.languageSpec
    value match
      case "plain" => Messages.text("settings.plain")
      case "hidden" => Messages.text("settings.hidden")
      case "distinct" => kind match
        case "add" => Messages.text("settings.distinct.add")
        case "del" => Messages.text("settings.distinct.del")
        case "see" => Messages.text("settings.distinct.see")
        case _ => "Distinct"
      case other => other.capitalize

  private def navGroup(cls: String, items: Seq[Xml.Element]): Option[Xml.Element] =
    Option.when(items.nonEmpty)(div(className := s"nav-group $cls", items))

  private def footerLicense(license: Config.License): Xml.Element =
    div(
      className := "footer-license",
      license.holder.map(holder =>
        license.holderLink match
          case Some(url) => a(href := url.trim, holder.trim)
          case None => span(holder.trim)
      ),
      license.link match
        case Some(url) => a(rel := "license", href := url.trim, license.name.trim)
        case None => span(license.name.trim)
    )
