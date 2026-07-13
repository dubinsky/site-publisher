package org.podval.tools.publish

import org.podval.tools.publish.page.{NonDirectoryPage, SyntheticMarkupPage}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

final class Errors(
  site: Site,
  treatErrorsAsWarnings: Boolean
) extends SyntheticMarkupPage(site, Path("errors").html) with NonDirectoryPage:
  override def titleDefault: String = "Errors"
  override protected def descriptionDefault: Option[String] = Some("Site errors by kind")
  override protected def iconDefault: Icon = Icon.errors
  override protected def headerPagePriorityDefault: Int = 9
  override protected def langDefault: Option[String] = Some("en")

  private var errorsVar: List[PageError] = List.empty

  def error(pageError: PageError): Unit =
    errorsVar = errorsVar.appended(pageError)
    if treatErrorsAsWarnings
    then site.log.warn(pageError.getMessage)
    else site.log.error(pageError.getMessage)

  override protected def syntheticContent: Html.Element =
    if !treatErrorsAsWarnings && errorsVar.nonEmpty then throw new IllegalStateException("There were page errors") else
      val byKind: Map[PageError.Kind, List[PageError]] = errorsVar.groupBy(_.kind)
      val kinds: List[PageError.Kind] = byKind.keys.toList.intersect(PageError.all)
      div(
        className := "site-errors",
        id := "site-errors",
        kinds.map(kind =>
          div(
            className := "kind",
            h2(kind.toString),
            ul(byKind(kind).map(error => li(error.getMessage)))
          )
        )
      )
      
