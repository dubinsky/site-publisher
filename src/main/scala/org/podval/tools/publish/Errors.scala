package org.podval.tools.publish

import org.podval.tools.publish.page.{NonDirectoryPage, SyntheticMarkupPage}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

final class Errors(site: Site) extends SyntheticMarkupPage(site, Path("errors").html) with NonDirectoryPage:
  override def titleDefault: String = "Errors"
  override protected def descriptionDefault: Option[String] = Some("Site errors by kind")
  override protected def iconDefault: Icon = Icon.errors
  override protected def headerPagePriorityDefault: Int = 9
  override protected def langDefault: Option[String] = Some("en")

  override protected def syntheticContent: Html.Element =
    div(className := "site-errors", id := "site-errors")

  private var errorsVar: List[PageError] = List.empty
  //  def errors: List[PageError] = errorsVar

  def warning(pageError: PageError): Unit =
    errorsVar = errorsVar.appended(pageError)
    site.log.warn(pageError.getMessage)

  def error(pageError: PageError): Unit =
    if site.treatErrorsAsWarnings
    then warning(pageError)
    else throw pageError
