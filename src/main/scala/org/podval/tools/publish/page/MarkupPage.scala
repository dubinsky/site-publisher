package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.{Minima, Path, Site}
import org.podval.xml.{Html, HtmlXmlDialect}

abstract class MarkupPage(site: Site, path: Path) extends RealPage(site, path) with PageWithContent:
  override def titleDefault: String = path.fileName

  private var sourceVar: Option[PageSource] = None
  final override def source: Option[PageSource] = sourceVar
  
  def setSource(
    markup: Markup,
    sourcePath: Path
  ): Unit = this.sourceVar = Some(PageSource(
    page = this,
    markup = markup,
    sourcePath = sourcePath
  ))

  final override def sourcePath: Option[Path] = source.map(_.sourcePath)

  final override def content: String =
    val markupContent: Option[Html.Element] = source.map(_.htmlContent)

    val html: Html.Element = Minima.render(
      page = this,
      markupContent = markupContent,
      syntheticContent = syntheticContentOpt
    )
    HtmlXmlDialect.render(html) // TODO use markup.xmlDialect?

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]
