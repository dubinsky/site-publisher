package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{MarkupPage, Page}
import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class CollectionIndexSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Collection Fixture
      |description: Collection index tests
      |url: http://collection.test
      |author: Test
      |email: test@collection.test
      |""".stripMargin

  private def tei(
    abstractText: String,
    when: String = "1798-08-11",
    author: String = """<author><persName ref="alter-rebbe">the Rebbe</persName></author>""",
    addressee: String = """<persName role="addressee" ref="ab">Someone</persName>""",
    transcriber: String = """<editor role="transcriber"><persName ref="ab">A</persName></editor>""",
    lang: String = "ru",
    pbs: String = """<pb n="000-1"/><pb n="000-2"/>""",
    body: String = "hello"
  ): String =
    s"""<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt>
       |      $author
       |      $transcriber
       |    </titleStmt></fileDesc>
       |    <profileDesc>
       |      <abstract>$abstractText</abstract>
       |      <creation><date when="$when">11 августа 1798</date></creation>
       |      <correspDesc><correspAction>$addressee</correspAction></correspDesc>
       |    </profileDesc>
       |  </teiHeader>
       |  <text xml:lang="$lang"><body>$pbs<p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private val defaultFiles: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "index.md" ->
      """---
        |title: Home
        |---
        |Home.
        |""".stripMargin,
    "people/alter-rebbe.xml" ->
      """<person>
        |  <persName>Залман Борухович</persName>
        |</person>
        |""".stripMargin,
    "people/ab.xml" ->
      """<person>
        |  <persName>A</persName>
        |</person>
        |""".stripMargin,
    "col.xml" ->
      """<collection n="1" alias="col">
        |  <title>Case</title>
        |  <part n="1" from="000"><title>First arrest</title></part>
        |  <part n="2" from="002"><title>Second arrest</title></part>
        |</collection>
        |""".stripMargin,
    "col/000.xml" -> tei("Cover of <persName ref=\"alter-rebbe\">him</persName>."),
    "col/001.xml" -> tei(
      abstractText = "Hebrew testimony",
      lang = "he",
      pbs = """<pb n="001-1"/><pb n="001-2"/>""",
      body = "hebrew"
    ),
    "col/001-ru.xml" -> tei(
      abstractText = "Russian translation",
      lang = "ru",
      pbs = """<pb n="001-1"/>""",
      body = "russian"
    ),
    "col/002.xml" -> tei(
      abstractText = "Later document",
      pbs = """<pb n="002-1"/><pb n="002-2" missing="true"/><pb n="003-1" missing="true" empty="true"/>""",
      body = "later"
    )
  )

  private def withSite(
    extra: Map[String, String] = Map.empty
  )(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-collection")
    try
      val dir: File = path.toFile
      (defaultFiles ++ extra).foreach: (relative, content) =>
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

  private def pageNamed(site: Site, name: String): Page =
    site.pages.pages.find(page => page.title == name || page.titleFromPath == name).get

  test("collection index is a table with part rows, not a page list") {
    withSite(): (_, target) =>
      val index: String = html(target, "col/index.html")
      assert(index.contains("""class="collection-index""""), index)
      assert(index.contains("Описание"), index)
      assert(index.contains("Дата"), index)
      assert(index.contains("Кто"), index)
      assert(index.contains("Кому"), index)
      assert(index.contains("Язык"), index)
      assert(index.contains("Документ"), index)
      assert(index.contains("Страницы"), index)
      assert(index.contains("Расшифровка"), index)
      assert(index.contains("""class="part-title""""), index)
      assert(index.contains("First arrest"), index)
      assert(index.contains("Second arrest"), index)
      assert(!index.contains("""class="page-list""""), index)
      assert(index.contains("1798-08-11"), index)
      assert(index.contains("""href="/people/alter-rebbe.html""""), index)
      assert(index.contains("""href="/col/000.html""""), index)
      assert(index.contains("""href="/col/000.html#p000-1""""), index)
      assert(index.contains(">000об<"), index)
      assert(index.contains("Отсутствуют фотографии 1 непустых страниц:"), index)
      assert(index.contains("002об"), index)
      assert(index.contains("Отсутствуют фотографии 1 пустых страниц:"), index)
      assert(index.contains("003"), index)
  }

  test("translations are language links, not table rows") {
    withSite(): (_, target) =>
      val index: String = html(target, "col/index.html")
      val documentColumn: String = index.substring(index.indexOf("collection-index"))
      assert(documentColumn.contains("""class="document"><a href="/col/001.html">001</a>"""), documentColumn)
      assert(!documentColumn.contains("""class="document"><a href="/col/001-ru.html""""), documentColumn)
      assert(index.contains("""class="language">he <a href="/col/001-ru.html">ru</a>"""), index)
  }

  test("original document has [lang] nav; translation does not; prev/next skip translations") {
    withSite(): (site, target) =>
      val original: String = html(target, "col/001.html")
      assert(original.contains("""class="nav-item translation""""), original)
      assert(original.contains("[ru]"), original)
      assert(original.contains("""href="/col/001-ru.html""""), original)
      assert(original.contains("""id="p001-1""""), original)
      val translation: String = html(target, "col/001-ru.html")
      assert(!translation.contains("""class="nav-item translation""""), translation)
      val first: MarkupPage = pageNamed(site, "000").asInstanceOf[MarkupPage]
      val middle: MarkupPage = pageNamed(site, "001").asInstanceOf[MarkupPage]
      val translated: MarkupPage = pageNamed(site, "001-ru").asInstanceOf[MarkupPage]
      val last: MarkupPage = pageNamed(site, "002").asInstanceOf[MarkupPage]
      assert(middle.prev.contains(first), middle.prev)
      assert(middle.next.contains(last), middle.next)
      assert(translated.prev.contains(first), translated.prev)
      assert(translated.next.contains(last), translated.next)
      assert(!first.next.contains(translated), first.next)
  }

  test("book pageType uses numeric page names") {
    withSite(Map(
      "book.xml" ->
        """<collection pageType="book">
          |  <title>Book</title>
          |</collection>
          |""".stripMargin,
      "book/084.xml" -> tei(
        abstractText = "A page",
        pbs = """<pb n="84"/>""",
        body = "book"
      )
    )): (_, target) =>
      val index: String = html(target, "book/index.html")
      assert(index.contains("""href="/book/084.html#p84""""), index)
      assert(index.contains(">84<"), index)
      assert(!index.contains("об"), index)
  }

  test("language suffix must match text xml:lang") {
    withSite(Map(
      "col/004-en.xml" -> tei(abstractText = "Mismatch", lang = "ru", pbs = "", body = "x")
    )): (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("file name"), errors)
      assert(errors.contains("004-en"), errors)
  }
