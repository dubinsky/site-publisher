package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.podval.tools.publish.js
import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class GraphPage(site: Site) extends SyntheticMarkupPage(site, Path("graph").html):
  override def titleDefault: String =
    given Language.Spec = site.languageSpec
    Messages.text("graph.title")

  override protected def descriptionDefault: Option[String] =
    given Language.Spec = site.languageSpec
    Some(Messages.text("graph.description"))

  override protected def iconDefault: Icon = Icon("share-nodes", Icon.Solid)
  override protected def extraLibraries: List[js.JSLibrary] = List(js.GraphCss, js.Cytoscape)

  override protected def syntheticContent: Xml.Element =
    given Language.Spec = site.languageSpec
    div(
      className := "site-graph-wrap",
      div(
        className := "site-graph-toolbar",
        input(
          `type` := "search",
          id := "site-graph-search",
          attr("placeholder") := Messages.text("graph.filter"),
          aria("label") := Messages.text("graph.filterLabel")
        ),
        label(
          className := "site-graph-orphans",
          input(
            `type` := "checkbox",
            id := "site-graph-orphans",
            attr("checked") := "checked"
          ),
          Messages.text("graph.orphans")
        ),
        span(
          id := "site-graph-status",
          className := "site-graph-status",
          attr("data-load-failed") := Messages.text("graph.loadFailed"),
          // graph.js replaces {n} and {e}. The values are those tokens, not counts.
          attr("data-status") := Messages.text("graph.status", "n" -> "{n}", "e" -> "{e}"),
          attr("data-large") := Messages.text("graph.large")
        )
      ),
      div(id := "site-graph", className := "site-graph")
    )
