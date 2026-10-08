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

  test("no social and no license still keeps three columns and an icon feed") {
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-footer")
    try
      val dir: File = path.toFile
      Files.write(File(dir, "_site_config.yml"), siteConfig)
      val site: Site = Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        logLevelOpt = Some("WARN")
      ))
      val html: String = HtmlXmlWriterConfig.render(site.siteFooter)
      val classes: Seq[String] = """class="([^"]*)"""".r.findAllMatchIn(html).map(_.group(1)).toSeq
      assert(classes.count(_.split("\\s+").contains("footer-col")) == 3, html)
      assert(!html.contains("footer-heading"), html)
      assert(!html.contains("<h2"), html)
      assert(!html.contains("<p"), html)
      assert(html.contains("""<div class="footer-description">aligned description</div>"""), html)
      assert(html.contains("Ada Lovelace"), html)
      val feed: String = """<a\b[^>]*>""".r.findAllIn(html).find(_.contains("footer-feed")).getOrElse("")
      assert(feed.contains("""href="/feed.xml""""), html)
      assert(feed.contains("""aria-label="RSS feed""""), html)
      assert(feed.contains("""title="RSS feed""""), html)
      assert(html.contains("fa-rss"), html)
      val text: String = html.replaceAll("""(?s)=("[^"]*"|'[^']*')""", "")
      assert(!text.contains("RSS feed"), html)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))
  }
