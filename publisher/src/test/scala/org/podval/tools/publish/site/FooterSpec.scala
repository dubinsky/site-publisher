package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.podval.xml.HtmlXmlWriterConfig
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class FooterSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Footer Fixture
      |description: aligned description
      |url: http://footer.test
      |author: Ada Lovelace
      |email: ada@footer.test
      |""".stripMargin

  private def withFooter(extra: String)(check: (Site, String) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-footer")
    try
      val dir: File = path.toFile
      Files.write(File(dir, "_site_config.yml"), siteConfig + extra)
      val site: Site = Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        logLevelOpt = Some("WARN")
      ))
      check(site, HtmlXmlWriterConfig.render(site.siteFooter))
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def licenseRow(html: String): String =
    val start: Int = html.indexOf("""<div class="footer-license">""")
    assert(start >= 0, html)
    val end: Int = html.indexOf("</div>", start)
    assert(end >= 0, html)
    html.substring(start, end + "</div>".length)

  private def anchors(html: String): Seq[String] =
    """(?s)<a\b[^>]*>.*?</a>""".r.findAllIn(html).toSeq

  test("no social and no license still keeps three columns and an icon feed") {
    withFooter("") { (site, html) =>
      val classes: Seq[String] = """class="([^"]*)"""".r.findAllMatchIn(html).map(_.group(1)).toSeq
      assert(classes.count(_.split("\\s+").contains("footer-col")) == 3, html)
      assert(!html.contains("footer-heading"), html)
      assert(!html.contains("<h2"), html)
      assert(!html.contains("<p"), html)
      assert(!html.contains("footer-license"), html)
      assert(html.contains("""<div class="footer-description">aligned description</div>"""), html)
      assert(html.contains("Ada Lovelace"), html)
      assert(site.license.isEmpty)
      val feed: String = """<a\b[^>]*>""".r.findAllIn(html).find(_.contains("footer-feed")).getOrElse("")
      assert(feed.contains("""href="/feed.xml""""), html)
      assert(feed.contains("""aria-label="RSS feed""""), html)
      assert(feed.contains("""title="RSS feed""""), html)
      assert(html.contains("fa-rss"), html)
      val text: String = html.replaceAll("""(?s)=("[^"]*"|'[^']*')""", "")
      assert(!text.contains("RSS feed"), html)
    }
  }

  test("license row links the holder and the license name") {
    withFooter(
      """license:
        |  name: CC BY 4.0
        |  link: http://creativecommons.org/licenses/by/4.0/
        |  holder: the Open Torah Project
        |  holder-link: http://www.opentorah.org/
        |""".stripMargin
    ) { (site, html) =>
      val row: String = licenseRow(html)
      val links: Seq[String] = anchors(row)
      assert(links.size == 2, row)
      assert(links.head.contains("""href="http://www.opentorah.org/""""), links.head)
      assert(links.head.contains("the Open Torah Project"), links.head)
      assert(!links.head.contains("rel="), links.head)
      assert(!links.head.contains("target"), links.head)
      assert(links(1).contains("""rel="license""""), links(1))
      assert(links(1).contains("""href="http://creativecommons.org/licenses/by/4.0/""""), links(1))
      assert(links(1).contains("CC BY 4.0"), links(1))
      val description: Int = html.indexOf("footer-description")
      val license: Int = html.indexOf("footer-license")
      val feed: Int = html.indexOf("footer-feed")
      assert(description < feed && feed < license, html)
      val head: String = HtmlXmlWriterConfig.render(site.license.get)
      assert(head.contains("""<link"""), head)
      assert(head.contains("""rel="license""""), head)
      assert(head.contains("""title="CC BY 4.0""""), head)
      assert(head.contains("""href="http://creativecommons.org/licenses/by/4.0/""""), head)
    }
  }

  test("holder without a link is a span") {
    withFooter(
      """license:
        |  name: CC BY 4.0
        |  link: http://creativecommons.org/licenses/by/4.0/
        |  holder: the Open Torah Project
        |""".stripMargin
    ) { (site, html) =>
      val row: String = licenseRow(html)
      assert(row.contains("<span>the Open Torah Project</span>"), row)
      val links: Seq[String] = anchors(row)
      assert(links.size == 1, row)
      assert(links.head.contains("""rel="license""""), links.head)
      assert(links.head.replaceAll("\\s+", " ").contains("CC BY 4.0"), links.head)
      assert(!links.head.contains("the Open Torah Project"), links.head)
      val description: Int = html.indexOf("footer-description")
      val license: Int = html.indexOf("footer-license")
      val feed: Int = html.indexOf("footer-feed")
      assert(description < feed && feed < license, html)
      assert(site.license.nonEmpty)
    }
  }

  test("license name without a link is a trimmed span and a head link without href") {
    withFooter(
      """license:
        |  name: "  CC by-nc-nd  "
        |""".stripMargin
    ) { (site, html) =>
      val row: String = licenseRow(html)
      assert(row.contains("<span>CC by-nc-nd</span>"), row)
      assert(anchors(row).isEmpty, row)
      assert(!row.contains("  CC"), row)
      val description: Int = html.indexOf("footer-description")
      val license: Int = html.indexOf("footer-license")
      val feed: Int = html.indexOf("footer-feed")
      assert(description < feed && feed < license, html)
      val head: String = HtmlXmlWriterConfig.render(site.license.get)
      assert(head.contains("""<link"""), head)
      assert(head.contains("""rel="license""""), head)
      assert(head.contains("""title="CC by-nc-nd""""), head)
      assert(!head.contains("href"), head)
    }
  }
