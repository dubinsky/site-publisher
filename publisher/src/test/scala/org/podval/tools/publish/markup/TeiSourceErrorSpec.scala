package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{CollectionAliases, PageError, Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class TeiSourceErrorSpec extends AnyFunSuite:
  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: TEI Source Errors
        |description: first-parse TEI page errors
        |url: http://tei-errors.test
        |author: Test
        |email: test@tei-errors.test
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
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-tei-errors")
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

  test("names without @ref in documents and store chrome are page errors") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("name without @ref"), errors)
      assert(errors.contains("Nobody"), errors)
      assert(errors.contains("Unlinked hero"), errors)
      assert(errors.contains("/col/000.xml"), errors)
      assert(errors.contains("/col.xml"), errors)
      assert(!errors.contains("name without @ref: A B"), errors)
      assert(errors.contains("""id="no-ref""""), errors)
      assert(PageError.NoRef.id == "no-ref")
  }

  test("unclear in a TEI document is a page error") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("unclear"), errors)
      assert(errors.contains("smudge"), errors)
      assert(errors.contains("/col/000.xml"), errors)
      assert(errors.contains("""id="unclear""""), errors)
      assert(PageError.Unclear.id == "unclear")
  }

  test("entity file name vs underscored first name is a page error") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("misnamed entity"), errors)
      assert(errors.contains("should be named 'A_B'"), errors)
      assert(errors.contains("/people/ab.xml"), errors)
      assert(TeiMarkup.expectedEntityFileName("A B") == "A_B")
      assert(errors.contains("""id="misnamed-entity""""), errors)
      assert(PageError.MisnamedEntity.id == "misnamed-entity")
  }

  test("errors page has a TOC when more than one kind is present") {
    withSite: (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("""class="site-errors-toc""""), errors)
      assert(errors.contains("""href="#no-ref""""), errors)
      assert(errors.contains("""href="#unclear""""), errors)
      assert(errors.contains("""href="#misnamed-entity""""), errors)
  }

  test("does not write /report pages or inbound /report rewrites") {
    withSite: (site, target) =>
      assert(!File(target, "report.html").isFile)
      assert(!File(target, "report/no-refs.html").isFile)
      assert(site.pages.rewriteRequest(Path.fromHref("/report")).isEmpty)
      assert(site.pages.rewriteRequest(Path.fromHref("/report/no-refs")).isEmpty)
      assert(!CollectionAliases.entries(site.pages).exists(_.from == Seq("report")))
  }
