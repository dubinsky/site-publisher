package org.podval.tools.publish.site

import org.podval.tools.publish.markup.HtmlMarkup
import org.podval.tools.publish.page.SyntheticXmlAsset
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

object Sitemap:
  val path: Path = Path("sitemap").withExtension("xml")
  
  def sitemapLink: Xml.Element = link(
    rel := "sitemap", 
    `type` := "application/xml", 
    titleAttr := "Sitemap",
    href := path.toString
  )

final class Sitemap(site: Site) extends SyntheticXmlAsset(site, Sitemap.path):
  override protected def iconDefault: Icon = Icon("map", Icon.Regular)

  override def xmlContent: Xml.Element =
    val xsi: String = "http://www.w3.org/2001/XMLSchema-instance"
    val location: String =
      "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"
    val sitemapNs: String = "http://www.sitemaps.org/schemas/sitemap/0.9"
    element(
      "urlset",
      xmlns("xsi") := xsi,
      attr("xsi:schemaLocation") := location,
      xmlns := sitemapNs,
      urls
    )

  private def urls: List[Xml.Element] = site
    .pages
    .pages
    .filter(_.path.extension.contains(HtmlMarkup.extension))
    .map: page =>
      val loc: Xml.Element = element("loc", s"${site.uri}${page.publishedPath}")
      // Date format: 2009-08-07T14:30:00-04:00
      val lastmod: Option[Xml.Element] = page.dateModifiedGit.map: date =>
        element("lastmod", date.toString)
      element("url", Seq(loc) ++ lastmod.toSeq)
