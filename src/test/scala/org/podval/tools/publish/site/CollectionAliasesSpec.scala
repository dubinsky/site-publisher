package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class CollectionAliasesSpec extends AnyFunSuite:
  private def tei(title: String, lang: String, extra: String = ""): String =
    s"""<TEI>
       |  <teiHeader><fileDesc><titleStmt><title>$title</title></titleStmt></fileDesc></teiHeader>
       |  <text xml:lang="$lang"><body>$extra<p>Body of $title.</p></body></text>
       |</TEI>
       |""".stripMargin

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-aliases")
    try
      val dir: File = path.toFile
      Files.write(
        File(dir, "_site_config.yml"),
        """title: Alias Fixture
          |description: collection alias Worker table
          |url: http://alias.test
          |author: Test
          |email: test@alias.test
          |facsimiles-url: https://facsimiles.test/
          |""".stripMargin
      )
      Files.write(File(dir, "index.md"), "Home.\n")
      Files.write(
        File(dir, "archive/lvia/1799/2.xml"),
        """<collection n="2" alias="lvia1799-2">
          |  <title>Two</title>
          |</collection>
          |""".stripMargin
      )
      Files.write(
        File(dir, "archive/lvia/1799/2-2.xml"),
        """<collection n="2-2" alias="lvia1799-2-2">
          |  <title>Two-two</title>
          |</collection>
          |""".stripMargin
      )
      Files.write(
        File(dir, "col.xml"),
        """<collection n="1" alias="col">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin
      )
      Files.write(
        File(dir, "col/000.xml"),
        tei("000", "ru", """<pb n="000-1"/>""")
      )
      Files.write(
        File(dir, "col/001.xml"),
        tei("001", "he", """<pb n="001-1"/>""")
      )
      Files.write(
        File(dir, "col/001-ru.xml"),
        tei("001-ru", "ru", """<pb n="001-1"/>""")
      )
      Files.write(
        File(dir, "col/255.2.xml"),
        tei("255.2", "ru")
      )
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

  private def rewritten(site: Site, href: String): Option[String] =
    site.pages.rewriteRequest(Path.fromHref(href)).map(_.toString)

  private def tableRewrite(table: Seq[CollectionAliases.Entry], href: String): Option[String] =
    CollectionAliases.rewrite(Path.fromHref(href), table).map(_.toString)

  test("writes collection-aliases.json from store aliases") {
    withSite: (site, target) =>
      val json: String = Files.read(File(target, CollectionAliases.fileName))
      assert(json.contains("""["col"]"""), json)
      assert(json.contains("lvia1799-2-2"), json)
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(table.exists(_.from == Seq("col")))
      assert(table.exists(_.from == Seq("lvia1799-2-2")))
      val col: CollectionAliases.Entry = table.find(_.from == Seq("col")).get
      assert(col.to == Seq("col"))
  }

  test("table rewrite matches Pages.rewriteRequest") {
    withSite: (site, _) =>
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      val hrefs: Seq[String] = Seq(
        "/col",
        "/col.html",
        "/col/000",
        "/col/000.html",
        "/col/255.2",
        "/col/facsimile/000",
        "/col/facsimile/000.html",
        "/col/000/facsimile",
        "/col/facsimile/001-ru",
        "/lvia1799-2",
        "/lvia1799-2-2"
      )
      hrefs.foreach: href =>
        val viaFind: Option[String] = rewritten(site, href)
        val viaTable: Option[String] = tableRewrite(table, href)
        assert(viaFind.nonEmpty, href)
        assert(viaTable == viaFind, s"$href: table=$viaTable find=$viaFind")
      assert(tableRewrite(table, "/col/001.xml").contains("/col/001.xml"))
  }

  test("slash-delimited longest prefix") {
    withSite: (site, _) =>
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(tableRewrite(table, "/lvia1799-2-2").contains("/archive/lvia/1799/2-2.html"))
      assert(tableRewrite(table, "/lvia1799-2").contains("/archive/lvia/1799/2.html"))
      assert(tableRewrite(table, "/css/style.css").isEmpty)
  }
