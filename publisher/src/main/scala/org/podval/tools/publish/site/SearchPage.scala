package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.podval.tools.publish.js
import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class SearchPage(site: Site) extends SyntheticMarkupPage(site, Path("search").html):
  override def titleDefault: String =
    given Language.Spec = site.languageSpec
    Messages.text("nav.search")

  override protected def iconDefault: Icon = Icon("magnifying-glass", Icon.Solid)

  override protected def extraLibraries: List[js.JSLibrary] = List(js.Search)

  override protected def syntheticContent: Xml.Element =
    given Language.Spec = site.languageSpec
    div(
      id := "site-search",
      attr("data-type-query") := Messages.text("search.typeQuery"),
      attr("data-none") := Messages.text("search.none"),
      attr("data-load-failed") := Messages.text("search.loadFailed"),
      // search.js replaces {n}. The value is that token, not a count.
      attr("data-overflow") := Messages.text("search.overflow", "n" -> "{n}"),
      attr("data-kind-document") := Messages.text("search.kind.document"),
      attr("data-kind-name") := Messages.text("search.kind.name"),
      attr("data-kind-note") := Messages.text("search.kind.note"),
      attr("data-kind-collection") := Messages.text("search.kind.collection"),
      p(id := "site-search-status", className := "site-search-status"),
      element(
        "form",
        className := "site-search-filters",
        attr("action") := "/search",
        attr("method") := "get",
        role := "search",
        filter(Messages.text("search.year"), "year"),
        filter(Messages.text("search.archive"), "archive"),
        filter(Messages.text("search.language"), "lang")
      ),
      element("ol", id := "site-search-hits", className := "site-search-hits")
    )

  private def filter(caption: String, name: String)(using Language.Spec): Xml.Element =
    label(
      s"$caption ",
      element(
        "select",
        attr("name") := name,
        element("option", Messages.text("search.all"))
      )
    )
