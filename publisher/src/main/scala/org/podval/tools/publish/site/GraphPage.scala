package org.podval.tools.publish.site

import org.podval.tools.publish.js
import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class GraphPage(site: Site) extends SyntheticMarkupPage(site, Path("graph").html):
  override def titleDefault: String = "Graph"
  override protected def descriptionDefault: Option[String] = Some("Site page graph")
  override protected def iconDefault: Icon = Icon("share-nodes", Icon.Solid)
  override protected def langDefault: Option[String] = Some("en")
  override protected def extraLibraries: List[js.JSLibrary] = List(js.GraphCss, js.Cytoscape)

  override protected def syntheticContent: Xml.Element =
    div(
      className := "site-graph-wrap",
      div(
        className := "site-graph-toolbar",
        input(
          `type` := "search",
          id := "site-graph-search",
          attr("placeholder") := "Filter",
          aria("label") := "Filter graph"
        ),
        label(
          className := "site-graph-orphans",
          input(
            `type` := "checkbox",
            id := "site-graph-orphans",
            attr("checked") := "checked"
          ),
          "Show orphans"
        ),
        span(id := "site-graph-status", className := "site-graph-status")
      ),
      div(id := "site-graph", className := "site-graph")
    )
