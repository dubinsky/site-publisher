package org.podval.tools.publish.page

import org.podval.tools.publish.{Minima, Path, Site}
import org.podval.xml.{Html, HtmlXmlDialect}

abstract class MarkupPage(site: Site, path: Path) extends RealPage(site, path) with PageWithContent:
  override def titleDefault: String = path.fileName

  private var sourceVar: Option[PageSource] = None
  final override def source: Option[PageSource] = sourceVar
  def setSource(source: PageSource): Unit = this.sourceVar = Some(source)

  final override def textContent: String =
    val markupContent: Option[Html.Element] = content.map(_.htmlContent)

    val html: Html.Element = Minima.render(
      page = this,
      markupContent = markupContent,
      syntheticContent = syntheticContentOpt
    )
    HtmlXmlDialect.render(html) // TODO use markup.xmlDialect?

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]
