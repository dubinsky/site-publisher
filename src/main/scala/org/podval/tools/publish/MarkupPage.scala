package org.podval.tools.publish

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, HtmlXmlDialect}

abstract class MarkupPage(site: Site, path: Path) extends Page.Real(site, path) with Page.WithContent:
  override def titleDefault: String = path.fileName

  private var sourceVar: Option[MarkupSource] = None
  final override def source: Option[MarkupSource] = sourceVar
  
  def setSource(
    markup: Markup,
    sourcePath: Path
  ): Unit = this.sourceVar = Some(MarkupSource(
    site = site,
    markup = markup,
    sourcePath = sourcePath
  ))

  final override def sourcePath: Option[Path] = source.map(_.sourcePath)

  final override def content: String =
    val markupContent: Option[Html.Element] = source.map(_.htmlContent(this))

    val html: Html.Element = Minima.render(
      page = this,
      markupContent = markupContent,
      syntheticContent = syntheticContentOpt
    )
    HtmlXmlDialect.render(html) // TODO use markup.xmlDialect?

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]

object MarkupPage:
  final class Simple(site: Site, path: Path) extends MarkupPage(site, path) with Page.NonDirectory:
    override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note
    override def hasSyntheticContent: Boolean = false
    override protected def syntheticContentOpt: Option[Html.Element] = None

  abstract class WithSyntheticContent(site: Site, path: Path) extends MarkupPage(site, path):
    final override def hasSyntheticContent: Boolean = true
    final override protected def syntheticContentOpt: Option[Html.Element] = Some(syntheticContent)
    protected def syntheticContent: Html.Element
