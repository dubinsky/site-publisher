package org.podval.tools.publish.site

import org.podval.tools.publish.page.SyntheticMarkupPage
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class Errors(
  site: Site,
  treatErrorsAsWarnings: Boolean
) extends SyntheticMarkupPage(site, Path("errors").html):
  override def titleDefault: String = "Errors"
  override protected def descriptionDefault: Option[String] = Some("Site errors by kind")
  override protected def iconDefault: Icon = Icon.errors

  private var errorsVar: List[PageError] = List.empty

  def error(pageError: PageError): Unit =
    errorsVar = errorsVar.appended(pageError)
    if treatErrorsAsWarnings
    then site.log.warn(pageError.getMessage)
    else site.log.error(pageError.getMessage)

  def throwIfErrors(): Unit =
    if !treatErrorsAsWarnings && errorsVar.nonEmpty then
      throw new IllegalStateException("There were errors")
    
  override protected def syntheticContent: Xml.Element =
    val byKind: Map[PageError.Kind, List[PageError]] = errorsVar.groupBy(_.kind)
    val kinds: List[PageError.Kind] = PageError.all.intersect(byKind.keys.toList)
    val toc: Xml.Element = ul(
      className := "site-errors-toc",
      kinds.map(kind => li(a(href := s"#${kind.id}", kind.toString)))
    )
    div(
      className := "site-errors",
      id := "site-errors",
      Option.when(kinds.length > 1)(toc),
      kinds.map(kind =>
        div(
          className := "kind",
          id := kind.id,
          h2(kind.toString),
          ul(byKind(kind).map(error => li(error.getMessage)))
        )
      )
    )
    
