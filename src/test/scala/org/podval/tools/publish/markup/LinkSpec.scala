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
          |aliases:
          |  - name: rgada
          |    to: /archive/rgada/category/VII/inventory/2/case/3140
          |""".stripMargin
      )
      Files.write(
        File(dir, "index.md"),
        """---
          |title: Home
          |---
          |See [[notes]] and [[notes#Alpha#One]] and [[notes#^blk]].
          |[Chunked HTML](/book/book/index.html) [One-Page HTML](/book/book.html)
          |""".stripMargin
      )
      Files.write(
        File(dir, "notes.md"),
        """---
          |title: Notes Title
          |aliases:
          |  - notes-alias
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
      Files.write(
        File(dir, "book/book.md"),
        """---
          |title: Nested Book
          |chunk: true
          |chunk-depth: 2
          |---
          |Preamble.
          |
          |## Alpha
          |
          |Leaf.
          |""".stripMargin
      )
      Files.write(
        File(dir, "aliased/index.md"),
        """---
          |permalink: /short
          |---
          |See [the child](/short/child) and [[short/child]].
          |""".stripMargin
      )
      Files.write(
        File(dir, "aliased/child.md"),
        "Child page under the aliased directory.\n"
      )
      Files.write(
        File(dir, "aliased/255.2.md"),
        "Dotted document id under the aliased directory.\n"
      )
      Files.write(
        File(dir, "archive/rgada/category/VII/inventory/2/case/3140.xml"),
        """<collection n="3140">
          |  <title>Case 3140</title>
          |</collection>
          |""".stripMargin
      )
      Files.write(
        File(dir, "archive/rgada/category/VII/inventory/2/case/3140/003.xml"),
        """<TEI>
          |  <teiHeader><fileDesc><titleStmt><title>Document 003</title></titleStmt></fileDesc></teiHeader>
          |  <text><body><p>Body of 003.</p></body></text>
          |</TEI>
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

  test("chunked TOC /book/book/index.html is not rewritten to /book/book.html") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val toc: Link = Link.resolve("/book/book/index.html", None, home).get
      assert(toc.url == "/book/book/index.html")
      val full: Link = Link.resolve("/book/book.html", None, home).get
      assert(full.url == "/book/book.html")
      val wiki: Link = Link.resolve("Nested Book", None, home).get
      assert(wiki.url == "/book/book.html")
  }

  test("relative index.html from a section chunk is the TOC") {
    withSite: site =>
      val alpha: Page = site.pages.pages.find(_.path == Path("book", "book", "Alpha").html).get
      val link: Link = Link.resolve("index.html", None, alpha).get
      assert(link.url == "/book/book/index.html")
  }

  test("permalink prefix /short/child and wiki short/child resolve under the aliased directory") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val absolute: Link = Link.resolve("/short/child", None, home).get
      assert(absolute.url == "/short/child.html")
      val wiki: Link = Link.resolve("short/child", None, home).get
      assert(wiki.url == "/short/child.html")
      val collection: Link = Link.resolve("/short", None, home).get
      assert(collection.url == "/short.html")
      assert(Link.resolve("/short/missing", None, home).isEmpty)
      val dotted: Link = Link.resolve("/short/255.2", None, home).get
      assert(dotted.url == "/short/255.2.html")
      val dottedWiki: Link = Link.resolve("short/255.2", None, home).get
      assert(dottedWiki.url == "/short/255.2.html")
  }

  test("site-config alias prefix resolves a TEI collection and shortens hrefs") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val doc: Link = Link.resolve("/rgada/003", None, home).get
      assert(doc.url == "/rgada/003.html")
      val collection: Link = Link.resolve("/rgada", None, home).get
      assert(collection.url == "/rgada.html")
      val long: Link = Link.resolve(
        "/archive/rgada/category/VII/inventory/2/case/3140/003",
        None,
        home
      ).get
      assert(long.url == "/rgada/003.html")
      assert(!site.pages.pages.exists(_.path == Path("rgada").html))
  }

  test("leaf alias does not prefix-resolve a remainder") {
    withSite: site =>
      val home: Page = pageNamed(site, "Home")
      val alias: Link = Link.resolve("/notes-alias", None, home).get
      assert(alias.url == "/notes.html")
      assert(Link.resolve("/notes-alias/nope", None, home).isEmpty)
  }

  test("mailto is not a self-link when site url has no host") {
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-mailto")
    try
      val dir: File = path.toFile
      Files.write(
        File(dir, "_site_config.yml"),
        """title: Mailto Fixture
          |description: mailto must not be a self-link
          |url: www.alter-rebbe.org
          |author: Test
          |email: test@example.test
          |""".stripMargin
      )
      Files.write(
        File(dir, "index.md"),
        "Write [olga](mailto:olga-minkina@yandex.ru) or olga-minkina@yandex.ru.\n"
      )
      val target: File = File(dir, "_site")
      Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(target.getAbsolutePath),
        treatErrorsAsWarnings = true,
        logLevelOpt = Some("WARN")
      )).generate()
      val errors: String = Files.read(File(target, "errors.html"))
      assert(!errors.contains("spurious external"), errors)
      assert(!errors.contains("olga-minkina"), errors)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))
  }
