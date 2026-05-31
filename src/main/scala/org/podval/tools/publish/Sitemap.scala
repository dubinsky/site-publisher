package org.podval.tools.publish

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{Asset, Path, Site}
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

object Sitemap:
  val path: Path = Path("sitemap").withExtension("xml")

// TODO <?xml version='1.0' encoding='UTF-8'?>
final class Sitemap(site: Site) extends Asset.SyntheticXmlAsset(site, Sitemap.path):
  override protected def iconDefault: Icon = Icon("map", Icon.Regular)

  override def xmlContent: Xml.Element =
    var result: Xml.Element = Xml.element("urlset")
    result = Xml.setAttribute(result, "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
    result = Xml.setAttribute(result, "xsi:schemaLocation", "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd")
    result = Xml.setAttribute(result, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
    result = Xml.setChildren(result, Chunk.from(urls))
    result

  private def urls: List[Xml.Element] = site
    .pages
    .filter(_.path.extension.contains(HtmlLike.Html.extension))
    .map: page =>
      val loc = Xml.setText(Xml.element("loc"), s"${site.config.url}${page.path}")
      // TODO date format: 2009-08-07T14:30:00-04:00
      val lastmod: Option[Xml.Element] = page.dateModified.map(date => Xml.setText(Xml.element("lastmod"), date.toString))
      Xml.setChildren(Xml.element("url"), Chunk.from(Seq(loc) ++ lastmod.toSeq))
