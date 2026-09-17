package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class ReportsSpec extends AnyFunSuite:
  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: Reports Fixture
        |description: report harvest
        |url: http://reports.test
        |author: Test
        |email: test@reports.test
        |lang: en
        |""".stripMargin,
    "index.md" -> "Home.\n",
    "col.xml" ->
      """<collection n="1">
        |  <title>About <persName>Unlinked hero</persName></title>
        |</collection>
        |""".stripMargin,
    "col/000.xml" ->
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><title>000</title></titleStmt></fileDesc></teiHeader>
        |  <text><body>
        |    <p>See <persName>Nobody</persName> and <unclear>smudge</unclear>
        |    and <persName ref="ab">A</persName>.</p>
        |  </body></text>
        |</TEI>
        |""".stripMargin,
    "people/ab.xml" ->
      """<person>
        |  <persName>A B</persName>
        |</person>
        |""".stripMargin
  )

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-reports")
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

  test("no-refs lists empty @ref from documents, stores, and entity files") {
    withSite: (_, target) =>
      val page: String = html(target, "report/no-refs.html")
      assert(page.contains("Nobody"), page)
      assert(page.contains("Unlinked hero"), page)
      assert(page.contains("A B"), page)
      assert(page.contains("""href="/col/000.html""""), page)
      assert(page.contains("""href="/col/index.html""""), page)
      assert(!page.contains(">A<"), page)
  }

  test("unclears lists TEI unclear with source") {
    withSite: (_, target) =>
      val page: String = html(target, "report/unclears.html")
      assert(page.contains("smudge"), page)
      assert(page.contains("""href="/col/000.html""""), page)
  }

  test("misnamed-entities compares file name to underscored main name") {
    withSite: (_, target) =>
      val page: String = html(target, "report/misnamed-entities.html")
      assert(page.contains("should be named 'A_B'"), page)
      assert(page.contains("""href="/people/ab.html""""), page)
      assert(Reports.spacesToUnderscores("A B") == "A_B")
  }

  test("index lists the three reports") {
    withSite: (_, target) =>
      val page: String = html(target, "report.html")
      assert(page.contains("Names without @ref"), page)
      assert(page.contains("""href="/report/no-refs.html""""), page)
      assert(page.contains("""href="/report/unclears.html""""), page)
      assert(page.contains("""href="/report/misnamed-entities.html""""), page)
  }
