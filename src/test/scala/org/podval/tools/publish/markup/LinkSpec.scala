package org.podval.tools.publish.markup

import org.podval.tools.publish.page.Page
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class LinkSpec extends AnyFunSuite:
  private def withSite(body: Site => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-links")
    try
      val dir: File = path.toFile
      Files.write(
        File(dir, "_site_config.yml"),
        """title: Link Fixture
          |description: Link.resolve tests
          |url: http://link.test
          |author: Test
          |email: test@link.test
          |""".stripMargin
      )
      Files.write(
        File(dir, "index.md"),
        """---
          |title: Home
          |---
          |See [[notes]] and [[notes#Alpha#One]] and [[notes#^blk]].
          |""".stripMargin
      )
      Files.write(
        File(dir, "notes.md"),
        """---
          |title: Notes Title
          |---
          |Intro ^blk
          |
          |## Alpha
          |
          |### One
          |
          |Leaf.
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

  private def pageNamed(site: Site, name: String): Page =
    site.pages.pages.find(page => page.title == name || page.titleFromPath == name).get

  test("resolves a page by file name and by title") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val byName: Link = Link.resolve("notes", None, home).get
      assert(!byName.isIntrapage)
      assert(byName.url == "/notes.html")
      assert(byName.fragment.isEmpty)
      val byTitle: Link = Link.resolve("Notes Title", None, home).get
      assert(byTitle.url == "/notes.html")
  }

  test("absolute /notes matches the source path") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val link: Link = Link.resolve("/notes", None, home).get
      assert(link.url == "/notes.html")
  }

  test("missing page is unresolved") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      assert(Link.resolve("missing-page", None, home).isEmpty)
  }

  test("first # splits path from fragment; nested section is Alpha#One") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val link: Link = Link.resolve("notes#Alpha#One", None, home).get
      assert(link.url == "/notes.html#One")
      assert(link.fragment.map(_.id).contains("One"))
  }

  test("intrapage #Alpha and block #^blk") {
    withSite: site =>
      val notes: Page = pageNamed(site, "notes")
      val section: Link = Link.resolve("#Alpha", None, notes).get
      assert(section.isIntrapage)
      assert(section.url == "#Alpha")
      val block: Link = Link.resolve("#^blk", None, notes).get
      assert(block.isIntrapage)
      assert(block.fragment.map(_.id).contains("blk"))
      assert(block.url == "#blk")
  }

  test("interpage block notes#^blk") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val link: Link = Link.resolve("notes#^blk", None, home).get
      assert(!link.isIntrapage)
      assert(link.url == "/notes.html#blk")
  }
