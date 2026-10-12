package org.podval.tools.publish.page

import org.podval.tools.publish.site.{CollectionAliases, Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class YearIndexSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Year Fixture
      |description: year index
      |url: http://year.test
      |author: Test
      |email: test@year.test
      |lang: ru
      |tei-default-calendar: julian
      |named-windows: true
      |header-pages:
      |  - about.md
      |""".stripMargin

  private def tei(
    abstractText: String,
    date: String,
    lang: String = "ru",
    body: String = "hello"
  ): String =
    val creation: String = if date.isEmpty then "" else s"<creation>$date</creation>"
    s"""<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt><title>t</title></titleStmt></fileDesc>
       |    <profileDesc>
       |      <abstract>$abstractText</abstract>
       |      $creation
       |    </profileDesc>
       |  </teiHeader>
       |  <text xml:lang="$lang"><body><p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private val docs: Map[String, String] = Map(
    "900.xml" -> tei("Август", """<date when="1798-08-11">11 августа 1798</date>"""),
    "282.xml" -> tei(
      """Показания <persName>Шнеура Залмана</persName>""",
      """<date when="1798-12-20">20 декабря 1798</date>"""
    ),
    "283.xml" -> tei(
      "Два года",
      """<date notBefore="1798-12-01" notAfter="1799-01-02">конец 1798</date>"""
    ),
    "010.xml" -> tei("Длинный", """<date notBefore="1790" notAfter="1800"/>"""),
    "400.xml" -> tei("Граница", """<date when="1800-12-22">22 декабря 1800</date>"""),
    "500.xml" -> tei("", """<date when="1791-06-01">1 июня 1791</date>"""),
    "000.xml" -> tei("Пустая дата", ""),
    "002.xml" -> tei("Проза", "<date>около 1800</date>"),
    "001.xml" -> tei("Оригинал", """<date when="1797-01-01">1 января 1797</date>"""),
    "001-ru.xml" -> tei("Перевод которого нет", """<date when="1797-01-01">1 января 1797</date>""")
  )

  private def tree(documents: Map[String, String], config: String = siteConfig): Map[String, String] =
    Map(
      "_site_config.yml" -> config,
      "index.md" ->
        """---
          |title: Home
          |---
          |Home.
          |""".stripMargin,
      "about.md" ->
        """---
          |title: About
          |---
          |About.
          |""".stripMargin,
      "archive.xml" ->
        """<store xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <by selector="archive">
          |    <xi:include href="archive/rgada.xml"/>
          |  </by>
          |</store>
          |""".stripMargin,
      "archive/rgada.xml" ->
        """<collection n="3140">
          |  <name lang="ru" n="РГАДА"/>
          |  <title>Дело</title>
          |</collection>
          |""".stripMargin
    ) ++ documents.map((name, xml) => s"archive/rgada/$name" -> xml)

  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-year-index")
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

  private def detailsById(page: String, id: String): String =
    val marker: String = s"""id="$id""""
    val at: Int = page.indexOf(marker)
    assert(at >= 0, s"missing #$id")
    val open: Int = page.lastIndexOf("<details", at)
    assert(open >= 0, s"#$id is not on a details element")
    val startTag: String = "<details"
    val endTag: String = "</details>"
    def closeAt(from: Int, depth: Int): Int =
      val nextOpen: Int = page.indexOf(startTag, from)
      val nextClose: Int = page.indexOf(endTag, from)
      assert(nextClose >= 0, s"unclosed #$id")
      if nextOpen >= 0 && nextOpen < nextClose then closeAt(nextOpen + startTag.length, depth + 1)
      else if depth == 1 then nextClose + endTag.length
      else closeAt(nextClose + endTag.length, depth - 1)
    page.substring(open, closeAt(open + startTag.length, 1))

  test("writes archive-years.html with source-calendar rows and no hover") {
    withSite(tree(docs)): (site, target) =>
      assert(File(target, "archive-years.html").isFile)
      val year: String = html(target, "archive-years.html")
      assert(year.contains("""class="year-index""""), year)
      assert(!year.contains("""class="wide""""), year)
      assert(!year.contains("""class="collection-index""""), year)
      assert(year.contains("""<title>Годы | Year Fixture</title>"""), year)
      assert(year.contains("""<h1 class="page-title">Годы</h1>"""), year)
      assert(year.contains("1798 · 3"), year)
      assert(year.contains("Без даты · 2"), year)
      assert(!year.contains("<h2>"), year)
      assert(!year.contains("open=\"open\""), year)
      assert("""<span class="date">\s*20 декабря 1798\s*</span>""".r.findFirstIn(year).isDefined, year)
      assert(year.contains("Показания Шнеура Залмана"), year)
      assert(!year.contains("<persName"), year)
      assert(year.contains("архив РГАДА, 282"), year)
      assert(!year.contains("date-tip"), year)
      assert(!year.contains("date-ref"), year)
      assert(!year.contains("Julian"), year)
      assert(!year.contains("Gregorian"), year)
      assert(!year.contains("Calendar"), year)
      assert(!year.contains("Jewish"), year)

      val y1798: String = detailsById(year, "y1798")
      val y1799: String = detailsById(year, "y1799")
      val y1790: String = detailsById(year, "y1790")
      val y1800: String = detailsById(year, "y1800")
      val undated: String = detailsById(year, "undated")
      assert(y1798.contains("/archive/rgada/282.html"), y1798)
      assert(!y1799.contains("/archive/rgada/282.html"), y1799)
      assert(y1800.contains("/archive/rgada/400.html"), y1800)
      assert(!year.contains("""id="y1801""""), year)
      assert(y1798.contains("/archive/rgada/283.html"), y1798)
      assert(y1799.contains("/archive/rgada/283.html"), y1799)
      assert(y1790.contains("/archive/rgada/010.html"), y1790)
      assert(y1790.contains("1790"), y1790)
      assert(y1790.contains("1800"), y1790)
      assert(!y1800.contains("/archive/rgada/010.html"), y1800)
      assert(undated.contains("/archive/rgada/000.html"), undated)
      assert(undated.contains("/archive/rgada/002.html"), undated)
      assert(undated.contains("около 1800"), undated)
      assert(undated.contains("РГАДА · 2"), undated)
      assert(y1798.contains("август · 1"), y1798)
      assert(y1798.contains("декабрь · 2"), y1798)
      val decemberMonth: String = detailsById(y1798, "y1798-m12")
      assert(decemberMonth.contains("/archive/rgada/282.html"), decemberMonth)
      assert(decemberMonth.contains("/archive/rgada/283.html"), decemberMonth)
      assert(!decemberMonth.contains("/archive/rgada/900.html"), decemberMonth)
      assert(y1799.contains("январь · 1"), y1799)
      assert(y1790.contains("без месяца · 1"), y1790)
      assert(!y1790.contains("январь"), y1790)
      val beforeUndated: String = year.substring(0, year.indexOf("""id="undated""""))
      assert(!beforeUndated.contains("/archive/rgada/000.html"), beforeUndated)
      assert(!beforeUndated.contains("/archive/rgada/002.html"), beforeUndated)
      assert(!beforeUndated.contains("около 1800"), beforeUndated)
      val undatedAt: Int = year.indexOf("""id="undated"""")
      assert("""id="y\d+"""".r.findAllMatchIn(year).forall(_.start < undatedAt), year)
      val august: Int = y1798.indexOf("/archive/rgada/900.html")
      val decemberStart: Int = y1798.indexOf("/archive/rgada/283.html")
      val december: Int = y1798.indexOf("/archive/rgada/282.html")
      assert(august >= 0 && decemberStart > august && december > decemberStart, y1798)
      val y1791: String = detailsById(year, "y1791")
      assert(y1791.contains("/archive/rgada/500.html"), y1791)
      assert(!y1791.contains("description"), y1791)
      assert(year.contains("/archive/rgada/001.html"), year)
      assert(!year.contains("/archive/rgada/001-ru.html"), year)
      assert(!year.contains("Перевод которого нет"), year)

      val body: String = year.substring(year.indexOf("page-body"), year.indexOf("</article>"))
      assert(body.contains("""target="textViewer""""), body)
      assert(body.contains("""class="archive-path""""), body)
      assert(!body.contains("hierarchyViewer"), body)

      assert(!site.pages.headerPages.exists(_.path.toString.contains("years")))
      val about: String = html(target, "about.html")
      val navStart: Int = about.indexOf("site-nav")
      val navEnd: Int = about.indexOf("</nav>", navStart)
      val nav: String = about.substring(navStart, navEnd)
      assert(!nav.contains("years"), nav)
      assert(!nav.contains("Годы"), nav)
      assert(!nav.contains("archive-years"), nav)
      assert(site.pages.rewriteRequest(Path.fromHref("/years")).map(_.toString).contains("/archive-years.html"))
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(
        table.exists(entry => entry.from == Seq("years") && entry.index.toString == "/archive-years.html"),
        table.map(entry => s"${entry.from.mkString("/")} -> ${entry.index}").mkString(", ")
      )
  }

  test("does not write a year page when the root has no TEI documents") {
    withSite(tree(Map.empty)): (site, target) =>
      assert(!File(target, "archive-years.html").isFile)
      assert(site.pages.rewriteRequest(Path.fromHref("/years")).isEmpty)
      assert(!CollectionAliases.entries(site.pages).exists(_.from == Seq("years")))
  }

  test("two root stores both get a year file and /years does not resolve") {
    val files: Map[String, String] = tree(Map(
      "282.xml" -> tei("Одно", """<date when="1798-12-20">20 декабря 1798</date>""")
    )) ++ Map(
      "library.xml" ->
        """<store xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <by selector="archive">
          |    <xi:include href="library/case.xml"/>
          |  </by>
          |</store>
          |""".stripMargin,
      "library/case.xml" ->
        """<collection n="1">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "library/case/1.xml" -> tei("Второе", """<date when="1801-01-02">2 января 1801</date>""")
    )
    withSite(files): (site, target) =>
      assert(File(target, "archive-years.html").isFile)
      assert(File(target, "library-years.html").isFile)
      assert(site.pages.rewriteRequest(Path.fromHref("/years")).isEmpty)
      assert(!CollectionAliases.entries(site.pages).exists(_.from == Seq("years")))
  }

  test("english lang titles the page Years") {
    val english: String = siteConfig.replace("lang: ru", "lang: en")
    withSite(tree(docs, english)): (_, target) =>
      val year: String = html(target, "archive-years.html")
      assert(year.contains("""<title>Years | Year Fixture</title>"""), year)
      assert(year.contains("""<h1 class="page-title">Years</h1>"""), year)
      assert(year.contains("Undated · 2"), year)
      assert(year.contains("No month · 1"), year)
      assert(year.contains("December · 2"), year)
      assert(!year.contains("Без даты"), year)
      assert(!year.contains("без месяца"), year)
      assert(!year.contains(">Годы<"), year)
  }

  test("root listing does not link the year page") {
    withSite(tree(docs)): (_, target) =>
      val index: String = html(target, "index.html")
      assert(index.contains("Home."), index)
      assert(index.contains("""class="page-list""""), index)
      assert(!index.contains("archive-years.html"), index)
      assert(!index.contains("Годы"), index)

    withSite(tree(docs) - "index.md"): (_, target) =>
      val index: String = html(target, "index.html")
      assert(index.contains("""class="page-list""""), index)
      assert(index.contains("/about.html"), index)
      assert(!index.contains("archive-years.html"), index)
      assert(!index.contains("Годы"), index)
  }

  test("a repeated day is a heading and a month range is listed twice") {
    withSite(tree(Map(
      "a1.xml" -> tei("Один", """<date when="1798-10-01">1 октября 1798</date>"""),
      "a2.xml" -> tei("Два", """<date when="1798-10-01">1 октября 1798</date>"""),
      "a3.xml" -> tei("Три", """<date when="1798-10-01">1 октября 1798</date>"""),
      "b.xml" -> tei("Или", """<date notBefore="1798-10-01" notAfter="1798-10-02">1 или 2 октября</date>"""),
      "c.xml" -> tei("Второй", """<date when="1798-10-02">2 октября 1798</date>"""),
      "span.xml" -> tei("Месяцы", """<date notBefore="1800-11" notAfter="1800-12">ноябрь или декабрь</date>""")
    ))): (_, target) =>
      val year: String = html(target, "archive-years.html")
      val october: String = detailsById(year, "y1798-m10")
      assert(october.contains("октябрь · 5"), october)
      assert(october.contains("1 октября · 3"), october)
      assert(!october.contains("2 октября ·"), october)
      val heading: Int = october.indexOf("1 октября · 3")
      val first: Int = october.indexOf("/archive/rgada/a1.html")
      val range: Int = october.indexOf("/archive/rgada/b.html")
      val second: Int = october.indexOf("/archive/rgada/c.html")
      assert(heading >= 0 && heading < first && first < range && range < second, october)
      val november: String = detailsById(year, "y1800-m11")
      val december: String = detailsById(year, "y1800-m12")
      assert(november.contains("/archive/rgada/span.html"), november)
      assert(december.contains("/archive/rgada/span.html"), december)
      assert(november.contains("ноябрь · 1"), november)
      assert(december.contains("декабрь · 1"), december)
  }

  test("undated documents are grouped by archive in store order") {
    val files: Map[String, String] = tree(Map(
      "1.xml" -> tei("Ргада", "")
    )) ++ Map(
      "archive.xml" ->
        """<store xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <by selector="archive">
          |    <xi:include href="archive/rgada.xml"/>
          |    <xi:include href="archive/niab.xml"/>
          |  </by>
          |</store>
          |""".stripMargin,
      "archive/niab.xml" ->
        """<collection n="1">
          |  <name lang="ru" n="НИАБ"/>
          |  <title>Дело</title>
          |</collection>
          |""".stripMargin,
      "archive/niab/2.xml" -> tei("Ниаб", "")
    )
    withSite(files): (_, target) =>
      val year: String = html(target, "archive-years.html")
      val undated: String = detailsById(year, "undated")
      assert(undated.contains("Без даты · 2"), undated)
      val rgada: Int = undated.indexOf("РГАДА · 1")
      val niab: Int = undated.indexOf("НИАБ · 1")
      assert(rgada >= 0 && niab > rgada, undated)
      assert(detailsById(undated, "undated-rgada").contains("/archive/rgada/1.html"))
      assert(detailsById(undated, "undated-niab").contains("/archive/niab/2.html"))
  }

  test("omits the undated section when every document has a creation year") {
    withSite(tree(Map(
      "282.xml" -> tei("Только год", """<date when="1798-12-20">20 декабря 1798</date>""")
    ))): (_, target) =>
      val year: String = html(target, "archive-years.html")
      assert(year.contains("""id="y1798""""), year)
      assert(!year.contains("""id="undated""""), year)
  }
