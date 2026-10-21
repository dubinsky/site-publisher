package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.site.{Messages, Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

/** Site-wide place map. A file at `map` is rendered above the figure.
  * Points are every `place` with a parseable `geo`, not the members of a `listPlace`. */
final class MapPage(
  site: Site,
  places: Seq[Page]
) extends SyntheticMarkupPage(site, Path("map").html):
  override def titleDefault: String =
    given Language.Spec = site.languageSpec
    Messages.text("map.page")

  override protected def iconDefault: Icon = Icon("map", Icon.Regular)

  override def parent: Option[DirectoryPage] = None

  override def pageHeader: Option[Xml.Element] = Some(
    header(className := "page-header",
      h1(className := "page-title", title)
    )
  )

  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override def markupContent: Option[Xml.Element] =
    markupContent(sectionId = None, isTerminal = true)

  override protected def syntheticContent: Xml.Element =
    PlaceMap.render(places).get
