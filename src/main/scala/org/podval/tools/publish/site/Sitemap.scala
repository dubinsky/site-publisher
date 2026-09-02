package org.podval.tools.publish.site

import org.podval.tools.publish.markup.HtmlMarkup
import org.podval.tools.publish.page.SyntheticXmlAsset
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, Xml, XmlAttribute}
import zio.blocks.html.*

object Sitemap:
  val path: Path = Path("sitemap").withExtension("xml")
  
  def sitemapLink: Html.Element = link(
    rel := "sitemap", 
    `type` := "application/xml", 
    titleAttr := "Sitemap",
    href := path.toString
  )

// TODO <?xml version='1.0' encoding='UTF-8'?>
final class Sitemap(site: Site) extends SyntheticXmlAsset(site, Sitemap.path):
  override protected def iconDefault: Icon = Icon("map", Icon.Regular)

  override def xmlContent: Xml.Element =Xml
    .element("urlset")
    .set(XmlAttribute.Xmlns("xsi"), "http://www.w3.org/2001/XMLSchema-instance")
    .set("xsi:schemaLocation", "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd")
    .set(XmlAttribute.Xmlns, "http://www.sitemaps.org/schemas/sitemap/0.9")
    .setChildren(urls)

  private def urls: List[Xml.Element] = site
    .pages
    .pages
    .filter(_.path.extension.contains(HtmlMarkup.extension))
    .map: page =>
      val loc = Xml.element("loc").setText(s"${site.uri}${page.publishedPath}")
      // Date format: 2009-08-07T14:30:00-04:00
      val lastmod: Option[Xml.Element] = page.dateModifiedGit.map: date =>
        Xml.element("lastmod").setText(date.toString)
      Xml
        .element("url")
        .setChildren(Seq(loc) ++ lastmod.toSeq)
