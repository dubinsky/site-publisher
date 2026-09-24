package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class PageScanSpec extends AnyFunSuite:
  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: Scan Errors
        |description: scan-time page errors
        |url: http://scan.test
        |author: Test
        |email: test@scan.test
        |lang: en
        |""".stripMargin,
    "index.md" -> "Home.\n",
    "pair.html" -> "<p>FROM-HTML</p>\n",
    "pair.md" -> "FROM-MD\n",
    "titled.md" -> "Titled body.\n",
    "titled.yaml" -> "title: From Yaml\n",
    "titled.yml" -> "title: From Yml\n",
    "notes.md" -> "FROM-BESIDE\n",
    "notes/index.md" -> "FROM-INSIDE\n",
    "_posts/index.md" -> "NOT-A-POST\n",
    "_posts/2026-01-02-real.md" -> "REAL-POST\n",
    "_posts/2026-05-02-hello.md" -> "FROM-ORIGINAL\n",
    "_posts/extra/2026-05-02-hello.md" -> "FROM-INTRUDER\n",
    "stray.xml" -> "<nope>FROM-STRAY</nope>\n",
    "broken.xml" -> "<tei><p>FROM-BROKEN\n",
    "ok.xml" ->
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><title>OK</title></titleStmt></fileDesc></teiHeader>
        |  <text><body><p>FROM-TEI</p></body></text>
        |</TEI>
        |""".stripMargin
  )

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-scan")
    try
      val dir: File = path.toFile
      files.foreach: (relative, content) =>
        Files.write(File(dir, relative), content)
      val target: File = File(dir, "_site")
      val site: Site = Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(target.getAbsolutePath),
        treatErrorsAsWarnings = true,
        logLevelOpt = Some("WARN")
      ))
      site.generate()
      body(site, target)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  private def sources(site: Site): Seq[String] =
    site.pages.pages.flatMap(_.sourcePath.map(_.toString))

  test("two markup files for one name keep the path-sorted file") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      val page: String = html(target, "pair.html")
      assert(errors.contains("multiple markup files for pair"), errors)
      assert(errors.contains("/pair.html"), errors)
      assert(errors.contains("/pair.md"), errors)
      assert(errors.contains("""id="duplicate""""), errors)
      assert(page.contains("FROM-HTML"), page)
      assert(!page.contains("FROM-MD"), page)
  }

  test("two sidecars keep the path-sorted file") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      val page: String = html(target, "titled.html")
      assert(errors.contains("multiple standalone front-matter files for titled"), errors)
      assert(errors.contains("/titled.yaml"), errors)
      assert(errors.contains("/titled.yml"), errors)
      assert(errors.contains("""id="ambiguous-front-matter""""), errors)
      assert(page.contains("From Yaml"), page)
      assert(!page.contains("From Yml"), page)
  }

  test("an inner directory index wins over the beside-directory file") {
    withSite: (site, target) =>
      val errors: String = html(target, "errors.html")
      val page: String = html(target, "notes/index.html")
      assert(errors.contains("both claim this directory"), errors)
      assert(errors.contains("/notes/index.md"), errors)
      assert(errors.contains("/notes.md"), errors)
      assert(page.contains("FROM-INSIDE"), page)
      assert(!page.contains("FROM-BESIDE"), page)
      assert(!sources(site).contains("/notes.md"))
      assert(!File(target, "notes.html").isFile)
  }

  test("an index in _posts is a file-name error and is not published") {
    withSite: (site, target) =>
      val errors: String = html(target, "errors.html")
      val post: String = html(target, "2026/01/02/real.html")
      assert(errors.contains("index file in a directory that has no page"), errors)
      assert(errors.contains("/_posts/index.md"), errors)
      assert(errors.contains("""id="file-name""""), errors)
      assert(post.contains("REAL-POST"), post)
      assert(!post.contains("NOT-A-POST"), post)
      assert(!sources(site).contains("/_posts/index.md"))
      val listing: File = File(target, "_posts/index.html")
      if listing.isFile then assert(!Files.read(listing).contains("NOT-A-POST"), Files.read(listing))
  }

  test("unknown XML root is an error and malformed XML does not abort") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      val ok: String = html(target, "ok.html")
      assert(errors.contains("unknown XML root 'nope'"), errors)
      assert(errors.contains("/stray.xml"), errors)
      assert(errors.contains("""id="unknown-xml""""), errors)
      assert(PageError.UnknownXml.id == "unknown-xml")
      assert(PageError.all.contains(PageError.UnknownXml))
      assert(!File(target, "stray.html").isFile)
      assert(!errors.contains("FROM-STRAY"), errors)
      assert(errors.contains("malformed XML"), errors)
      assert(errors.contains("/broken.xml"), errors)
      assert(ok.contains("FROM-TEI"), ok)
      assert(!File(target, "broken.html").isFile)
  }

  test("a second file at an occupied path does not replace the page") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      val page: String = html(target, "2026/05/02/hello.html")
      assert(errors.contains("/_posts/extra/2026-05-02-hello.md collides with /2026/05/02/hello.html"), errors)
      assert(page.contains("FROM-ORIGINAL"), page)
      assert(!page.contains("FROM-INTRUDER"), page)
  }
