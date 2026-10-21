package org.podval.tools.publish.site

import org.podval.metadata.{Language, Name, PluralCategory}
import org.podval.tools.publish.js
import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

import java.util.Locale

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
        graphStatus
      ),
      div(id := "site-graph", className := "site-graph")
    )

  private def graphStatus(using spec: Language.Spec): Xml.Element =
    val base: Xml.Element = span(
      id := "site-graph-status",
      className := "site-graph-status",
      attr("data-load-failed") := Messages.text("graph.loadFailed"),
      // graph.js replaces {n}, {e}, {nodes}, and {edges}. The values are those tokens, not counts.
      attr("data-status") := Messages.text(
        "graph.status",
        "n" -> "{n}",
        "e" -> "{e}",
        "nodes" -> "{nodes}",
        "edges" -> "{edges}"
      ),
      attr("data-plural") := spec.language.get.name,
      attr("data-large") := Messages.text("graph.large")
    )
    addCardinal(addCardinal(base, "node", "graph.node"), "edge", "graph.edge")

  private def addCardinal(
    element: Xml.Element,
    kind: String,
    termId: String
  )(using spec: Language.Spec): Xml.Element =
    val found: Name = site.terms.getById(termId).names.doFind(spec)
    // Probes cover every category Language.pluralCategory returns for some n >= 0.
    val returned: Set[PluralCategory] = Set(0, 1, 2, 5, 10, 11, 21, 22).map(spec.language.get.pluralCategory)
    PluralCategory.values.toSeq.filter(returned.contains).foldLeft(element): (acc, category) =>
      found.form(category).fold(acc): word =>
        acc.set(s"data-$kind-${category.toString.toLowerCase(Locale.ROOT)}", word)
