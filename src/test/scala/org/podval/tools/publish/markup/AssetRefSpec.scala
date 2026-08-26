package org.podval.tools.publish.markup

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class AssetRefSpec extends AnyFunSuite:
  private def withSite(body: Site => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-assets")
    try
      val dir: File = path.toFile
      Files.write(
        File(dir, "_site_config.yml"),
        """title: Asset Fixture
          |description: AssetRef tests
          |url: http://asset.test
          |author: Test
          |email: test@asset.test
          |""".stripMargin
      )
      Files.write(
        File(dir, "pixel.svg"),
        """<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"/>"""
      )
      Files.write(
        File(dir, "assets/dot.svg"),
        """<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"/>"""
      )
      Files.write(
        File(dir, "index.md"),
        """---
          |title: Home
          |---
          |![root](pixel.svg)
          |""".stripMargin
      )
      Files.write(
        File(dir, "nested/page.md"),
        """---
          |title: Nested
          |---
          |Markdown path: ![local](pixel.svg)
          |Wiki name: ![[pixel.svg]]
          |Wiki path: ![[assets/dot.svg]]
          |""".stripMargin
      )
      Files.write(
        File(dir, "chunked.md"),
        """---
          |title: Chunked
          |chunk: true
          |chunk-depth: 2
          |---
          |Preamble ![p](pixel.svg)
          |
          |## Alpha
          |
          |![a](pixel.svg)
          |""".stripMargin
      )
      val target: File = File(dir, "_site")
      val site: Site = Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(target.getAbsolutePath),
        treatErrorsAsWarnings = true,
        logLevelOpt = Some("WARN")
      ))
      site.generate()
      body(site)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def html(site: Site, relative: String): String =
    Files.read(File(site.targetDirectory, relative)).replaceAll("\\s+", " ").replace("= ", "=")

  test("markdown image src at site root is rewritten to /pixel.svg") {
    withSite: site =>
      val page: String = html(site, "index.html")
      assert(page.contains("""src="/pixel.svg""""), page)
      assert(!page.contains("""unresolved-asset"""), page)
  }

  test("markdown path from a nested page does not basename-search") {
    withSite: site =>
      val page: String = html(site, "nested/page.html")
      assert(page.contains("""src="pixel.svg""""), page)
      assert(page.contains("""unresolved-asset"""), page)
      val errors: String = html(site, "errors.html")
      assert(errors.contains("missing asset"), errors)
      assert(errors.contains("pixel.svg"), errors)
  }

  test("wiki embed finds a bare filename anywhere and a vault path from root") {
    withSite: site =>
      val page: String = html(site, "nested/page.html")
      assert(page.contains("""src="/pixel.svg""""), page)
      assert(page.contains("""src="/assets/dot.svg""""), page)
      assert(!page.contains("data-wiki-embed"), page)
  }

  test("chunked pages keep the site-root asset path") {
    withSite: site =>
      val toc: String = html(site, "chunked/index.html")
      assert(toc.contains("""src="/pixel.svg""""), toc)
      val alpha: String = html(site, "chunked/Alpha.html")
      assert(alpha.contains("""src="/pixel.svg""""), alpha)
      val full: String = html(site, "chunked.html")
      assert(full.contains("""src="/pixel.svg""""), full)
  }

  test("resourceAttr is src/data on media IR only") {
    assert(AssetRef.resourceAttr(org.podval.xml.Xml.element("img")).contains("src"))
    assert(AssetRef.resourceAttr(org.podval.xml.Xml.element("video")).contains("src"))
    assert(AssetRef.resourceAttr(org.podval.xml.Xml.element("object")).contains("data"))
    assert(AssetRef.resourceAttr(org.podval.xml.Xml.element("iframe")).isEmpty)
    assert(AssetRef.resourceAttr(org.podval.xml.Xml.element("a")).isEmpty)
  }
