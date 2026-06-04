package org.podval.tools.publish

import org.podval.tools.publish.markup.HtmlMarkup
import org.podval.tools.publish.page.SyntheticXmlAsset
import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{Path, Site}
import org.podval.xml.{Xml, XmlAttribute, XmlElement}
import zio.blocks.chunk.Chunk

object Sitemap:
  val path: Path = Path("sitemap").withExtension("xml")

// TODO <?xml version='1.0' encoding='UTF-8'?>
final class Sitemap(site: Site) extends SyntheticXmlAsset(site, Sitemap.path):
  override protected def iconDefault: Icon = Icon("map", Icon.Regular)

  override def xmlContent: Xml.Element =Xml
    .element(XmlElement("urlset"))
    .set(XmlAttribute.Xmlns("xsi"), "http://www.w3.org/2001/XMLSchema-instance")
    .set(XmlAttribute("xsi:schemaLocation"), "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd")
    .set(XmlAttribute.Xmlns, "http://www.sitemaps.org/schemas/sitemap/0.9")
    .setChildren(Chunk.from(urls))

  private def urls: List[Xml.Element] = site
    .pages
    .filter(_.path.extension.contains(HtmlMarkup.extension))
    .map: page =>
      val loc = Xml.element(XmlElement("loc")).setText(s"${site.config.url}${page.path}")
      // Date format: 2009-08-07T14:30:00-04:00
      val lastmod: Option[Xml.Element] = page.dateModifiedGit.map: date =>
        Xml.element(XmlElement("lastmod")).setText(date.toString)
      Xml
        .element(XmlElement("url"))
        .setChildren(Chunk.from(Seq(loc) ++ lastmod.toSeq))
