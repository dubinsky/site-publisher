package org.podval.tools.publish.site

import org.apache.pdfbox.Loader
import org.apache.pdfbox.text.PDFTextStripper
import org.podval.tools.publish.util.{Files, PdfNamedDestinations, SiteOptions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class SiteSpec extends AnyFunSuite, BeforeAndAfterAll:
  private val repositoryRoot: File =
    val anchor = getClass.getResource("/anchor.txt")
    require(anchor != null, "missing test resource /anchor.txt")
    File(anchor.toURI)
      .getParentFile // resources
      .getParentFile // test
      .getParentFile // src
      .getParentFile // root

  private val sourceDirectory: File = File(repositoryRoot, "src/test/site")
  private val targetDirectory: File = File(repositoryRoot, "build/test-site")

  override def beforeAll(): Unit =
    super.beforeAll()
    Site(SiteOptions(
      sourceDirectoryPath = sourceDirectory.getAbsolutePath,
      targetDirectoryNameOpt = Some(targetDirectory.getAbsolutePath),
      treatErrorsAsWarnings = true,
      logLevelOpt = Some("WARN")
    )).generate()

  private def html(relative: String): String =
    val file: File = File(targetDirectory, relative)
    assert(file.isFile, s"missing $relative under $targetDirectory")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  private def exists(relative: String): Unit =
    assert(File(targetDirectory, relative).isFile, s"missing $relative under $targetDirectory")

  test("generates into build/test-site, not _site under the fixture") {
    assert(targetDirectory.isDirectory, targetDirectory)
    assert(targetDirectory.getAbsolutePath.contains(s"${File.separator}build${File.separator}test-site"))
    assert(!File(sourceDirectory, "_site").exists)
  }

  test("writes synthetic pages, CSS, sitemap, robots, and feed") {
    exists("index.html")
    exists("errors.html")
    exists("tags.html")
    exists("posts.html")
    exists("sitemap.xml")
    exists("robots.txt")
    exists("feed.xml")
    exists("assets/css/style.css")
  }

  test("home page wiki links resolve") {
    val page: String = html("index.html")
    assert(page.contains("Site Publisher Fixture"), page)
    assert(page.contains("""href="/notes.html""""), page)
    assert(page.contains("""href="/glossary.html""""), page)
    assert(page.contains("""href="/cite.html""""), page)
    assert(page.contains("""href="/chunked.html""""), page)
    assert(page.contains("""href="/book.html""""), page)
    assert(page.contains("""href="/about.html""""), page)
    assert(page.contains("""href="/tei-sample.html""""), page)
    assert(!page.contains("unresolved-link"), page)
  }

  test("Markdown notes: table, task-list IR, footnotes, fenced scala") {
    val page: String = html("notes.html")
    assert(page.contains("<table"), page)
    assert(page.contains(">1</td>") || page.contains(">1</th>") || page.contains(">1<"), page)
    assert(page.contains("""class="task-list""""), page)
    assert(page.contains("""class="task-list-item""""), page)
    assert(page.contains("task-list-item-checkbox"), page)
    assert(page.contains("open the box"), page)
    assert(page.contains("done already"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("""class="footnote-tip""""), page)
    assert(page.contains("Markdown footnote body"), page)
    assert(page.contains("""class="footnotes""""), page)
    assert(page.contains("language-scala"), page)
    assert(page.contains("xs.map(f)"), page)
  }

  test("Markdown glossary IAL yields term ids and hover tips") {
    val page: String = html("glossary.html")
    assert(page.contains("""class="glossary""""), page)
    assert(page.contains("""class="glossary-item""""), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("""class="glossary-tip""""), page)
    assert(page.contains("a verse"), page)
  }

  test("Markdown APA cites become #bibl- links; unknown keys are recorded") {
    val page: String = html("cite.html")
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-lamport94""""), page)
    assert(page.contains("Knuth") || page.toLowerCase.contains("knuth"), page)
    assert(page.contains("unresolved-citation"), page)
    assert(page.contains("missing-key"), page)
    val errors: String = html("errors.html")
    assert(errors.contains("unknown citation"), errors)
    assert(errors.contains("missing-key"), errors)
  }

  test("chunked Markdown writes a TOC chunk and per-section pages") {
    exists("chunked.html")
    val toc: String = html("chunked/chunked.html")
    assert(toc.contains("Preamble of the chunked document"), toc)
    assert(toc.contains("Table of Contents"), toc)
    assert(!toc.contains("Alpha preamble, before subsections"), toc)
    val alpha: String = html("chunked/Alpha.html")
    assert(alpha.contains("Alpha preamble, before subsections"), alpha)
    assert(!alpha.contains("First leaf"), alpha)
    val one: String = html("chunked/Alpha-One.html")
    assert(one.contains("First leaf"), one)
    assert(!one.contains("Second leaf"), one)
    val two: String = html("chunked/Alpha-Two.html")
    assert(two.contains("Second leaf"), two)
    val beta: String = html("chunked/Beta.html")
    assert(beta.contains("Beta has no children"), beta)
  }

  test("AsciiDoc book: checklist IR, table, footnote, citation, glossary") {
    val page: String = html("book.html")
    assert(page.contains("""class="task-list""""), page)
    assert(page.contains("open the chapter"), page)
    assert(page.contains("done the chapter"), page)
    assert(!page.contains("""class="checklist""""), page)
    assert(page.contains("<table"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("AsciiDoc footnote body"), page)
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("glossary-item"), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("verse"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("""class="callout""""), page)
    assert(page.contains("""class="callout-list""""), page)
    assert(page.contains("Library import"), page)
  }

  test("HTML about: table, task-list IR, glossary, header nav, citation IR") {
    val page: String = html("about.html")
    assert(page.contains("<table"), page)
    assert(page.contains("""class="task-list""""), page)
    assert(page.contains("html-open"), page)
    assert(page.contains("html-done"), page)
    assert(page.contains("""id="html-term""""), page)
    assert(page.contains("defined in HTML"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    val home: String = html("index.html")
    assert(home.contains("""href="/about.html""""), home)
  }

  test("TEI sample: endnote, table, glossary, and code") {
    val page: String = html("tei-sample.html")
    assert(page.contains("TEI endnote body"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("<table"), page)
    assert(page.contains(">A<") || page.contains(">A</"), page)
    assert(page.contains("glossary-item"), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("verse"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("language-scala"), page)
    assert(page.contains("xs.map(f)"), page)
  }

  test("dated post is published and listed") {
    val post: String = html("2026/08/01/hello.html")
    assert(post.contains("Hello from a post"), post)
    assert(post.contains("""href="/notes.html""""), post)
    val posts: String = html("posts.html")
    assert(posts.contains("/2026/08/01/hello.html"), posts)
    val tags: String = html("tags.html")
    assert(tags.contains("fixture"), tags)
  }

  test("Playwright PDF of glossary: page count, named dest, text") {
    val pdf: File = File(targetDirectory, "glossary.pdf")
    assert(pdf.isFile, s"missing glossary.pdf under $targetDirectory")
    val document = Loader.loadPDF(pdf)
    try
      val pages: Int = document.getNumberOfPages
      assert(pages >= 1, s"expected at least one page, got $pages")
      val text: String = PDFTextStripper().getText(document).replaceAll("\\s+", " ")
      assert(text.contains("a verse"), text)
    finally
      document.close()
    val dests: Map[String, Int] = PdfNamedDestinations.pageByName(pdf)
    assert(dests.get("posuk").exists(_ >= 1), dests)
  }

  test("sitemap, robots, and feed mention generated pages") {
    val sitemap: String = html("sitemap.xml")
    assert(sitemap.contains("http://fixture.test/index.html"), sitemap)
    assert(sitemap.contains("http://fixture.test/notes.html"), sitemap)
    val robots: String = html("robots.txt")
    assert(robots.contains("http://fixture.test/sitemap.xml"), robots)
    val feed: String = html("feed.xml")
    assert(feed.contains("hello"), feed)
  }
