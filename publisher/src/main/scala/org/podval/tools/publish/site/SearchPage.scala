package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.podval.tools.publish.js
import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class SearchPage(site: Site) extends SyntheticMarkupPage(site, Path("search").html):
  override def titleDefault: String =
    if russian then "Поиск" else "Search"

  override protected def iconDefault: Icon = Icon("magnifying-glass", Icon.Solid)

  override protected def extraLibraries: List[js.JSLibrary] = List(js.Search)

  override protected def syntheticContent: Xml.Element =
    div(
      id := "site-search",
      attr("data-lang") := dataLang,
      p(id := "site-search-status", className := "site-search-status"),
      element(
        "form",
        className := "site-search-filters",
        attr("action") := "/search",
        attr("method") := "get",
        role := "search",
        filter(if russian then "Год" else "Year", "year"),
        filter(if russian then "Архив" else "Archive", "archive"),
        filter(if russian then "Язык" else "Language", "lang")
      ),
      element("ol", id := "site-search-hits", className := "site-search-hits")
    )

  private def russian: Boolean =
    site.languageSpec == Language.Russian.toSpec

  private def dataLang: String =
    if russian then "ru" else "en"

  private def allLabel: String =
    if russian then "Все" else "All"

  private def filter(caption: String, name: String): Xml.Element =
    label(
      s"$caption ",
      element(
        "select",
        attr("name") := name,
        element("option", allLabel)
      )
    )
