package org.podval.tools.publish.site

import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

final class Errors(
  site: Site,
  treatErrorsAsWarnings: Boolean
) extends SyntheticMarkupPage(site, Path("errors").html):
  override def titleDefault: String = "Errors"
  override protected def descriptionDefault: Option[String] = Some("Site errors by kind")
  override protected def iconDefault: Icon = Icon.errors
  override protected def langDefault: Option[String] = Some("en")

  private var errorsVar: List[PageError] = List.empty

  def error(pageError: PageError): Unit =
    errorsVar = errorsVar.appended(pageError)
    if treatErrorsAsWarnings
    then site.log.warn(pageError.getMessage)
    else site.log.error(pageError.getMessage)

  def throwIfErrors(): Unit =
    if !treatErrorsAsWarnings && errorsVar.nonEmpty then
      throw new IllegalStateException("There were errors")
    
  override protected def syntheticContent: Html.Element =
    val byKind: Map[PageError.Kind, List[PageError]] = errorsVar.groupBy(_.kind)
    val kinds: List[PageError.Kind] = PageError.all.intersect(byKind.keys.toList)
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
    
