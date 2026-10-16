package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, Json, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class SearchSpec extends AnyFunSuite:
  private def tei(date: String, body: String): String =
    val creation: String = if date.isEmpty then "" else s"<creation>$date</creation>"
    s"""<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt><title>t</title></titleStmt></fileDesc>
       |    <profileDesc>
       |      <abstract>abstract</abstract>
       |      $creation
       |    </profileDesc>
       |  </teiHeader>
       |  <text xml:lang="ru"><body>$body</body></text>
       |</TEI>
       |""".stripMargin

  private val datedOne: String =
    tei(
      """<date when="1798-12-20">20 декабря 1798</date>""",
      """<p>На вопросы въ тайной <choice><sic>сицслово</sic><corr>коррслово</corr></choice> Борухо-<lb break="no"/>вичъ.</p>
        |<note>сносканеиндекс</note>
        |""".stripMargin
    )

  private val datedTwo: String =
    tei(
      """<date when="1800-01-02">2 января 1800</date>""",
      "<p>Только в теле документа.</p>"
    )

  private val ranged: String =
    tei(
      """<date notBefore="1798-12-01" notAfter="1799-01-02">конец 1798</date>""",
      "<p>диапазон</p>"
    )

  private val undated: String =
    tei("", "<p>без даты</p>")

  private val skipped: String =
    tei(
      """<date when="1795-01-01">1 января 1795</date>""",
      "<p>пропущенноеслово</p>"
    )

  private val commaHyphen: String =
    tei(
      """<date when="1798-12-21">21 декабря 1798</date>""",
      """<p>Префикс Борухо-<lb break="no"/>вичъ, суффикс.</p>"""
    )

  private val consecutiveHyphen: String =
    tei(
      """<date when="1798-12-22">22 декабря 1798</date>""",
      """<p>Тут на-<lb break="no"/>хо-<lb break="no"/>дилось слово.</p>"""
    )

  private val maqafHyphen: String =
    tei(
      """<date when="1798-12-23">23 декабря 1798</date>""",
      "<p>Тут Шалом\u05be<lb break=\"no\"/>алейхем слово.</p>"
    )

  private val plainBreak: String =
    tei(
      """<date when="1798-12-24">24 декабря 1798</date>""",
      """<p>Тут что-то и Книго-<lb/>продавец слово.</p>"""
    )

  private val authoredHyphen: String =
    tei(
      """<date when="1798-12-25">25 декабря 1798</date>""",
      "<p>кавалеру Римскому-Кор-<lb break=\"no\"/>\n сакову конец.</p>"
    )

  private val continuationSpace: String =
    tei(
      """<date when="1798-12-26">26 декабря 1798</date>""",
      "<p>Посредствомъ Могилев-<lb break=\"no\"/>\n скою губерніею конец.</p>"
    )

  private def tree(search: String): Map[String, String] = Map(
    "_site_config.yml" ->
      s"""title: Search Fixture
         |description: search tests
         |url: http://search.test
         |author: Test
         |email: test@search.test
         |lang: ru
         |$search""".stripMargin,
    "index.md" ->
      """---
        |title: Home
        |---
        |Home.
        |""".stripMargin,
    "book.md" ->
      """---
        |title: Book
        |chunk: true
        |chunk-depth: 2
        |---
        |Intro only here.
        |
        |## Alpha
        |
        |Leaf of the chunk.
        |""".stripMargin,
    "people/Zalman.xml" ->
      """<person>
        |  <persName>Zalman</persName>
        |  <p>founder</p>
        |  <note>имязаметка</note>
        |</person>
        |""".stripMargin,
    "archive.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="archive">
        |    <xi:include href="archive/rgada.xml"/>
        |    <xi:include href="archive/skip.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "archive/rgada.xml" ->
      """<collection n="3140">
        |  <name lang="ru" n="РГАДА"/>
        |  <title>Заголовок дела</title>
        |  <abstract><p>Краткое описание дела.</p></abstract>
        |</collection>
        |""".stripMargin,
    "archive/skip.xml" ->
      """<collection n="9">
        |  <name lang="ru" n="Пропуск"/>
        |  <title>Не в поиске</title>
        |</collection>
        |""".stripMargin,
    "archive/rgada/282.xml" -> datedOne,
    "archive/rgada/400.xml" -> datedTwo,
    "archive/rgada/283.xml" -> ranged,
    "archive/rgada/000.xml" -> undated,
    "archive/rgada/501.xml" -> commaHyphen,
    "archive/rgada/502.xml" -> consecutiveHyphen,
    "archive/rgada/503.xml" -> maqafHyphen,
    "archive/rgada/504.xml" -> plainBreak,
    "archive/rgada/505.xml" -> authoredHyphen,
    "archive/rgada/506.xml" -> continuationSpace,
    "archive/skip/999.xml" -> skipped
  )

  private val enabled: String =
    """search:
      |  enabled: true
      |  exclude-path-prefixes:
      |    - archive/skip
      |""".stripMargin

  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-search")
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

  private def read(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file)

  private def record(json: String, href: String): String =
    val needle: String = s""""href":${Json.string(href)}"""
    val start: Int = json.indexOf(needle)
    assert(start >= 0, s"missing $href in $json")
    val end: Int = json.indexOf('}', start)
    assert(end >= 0, json)
    json.substring(start, end + 1)

  test("omitted search writes neither file and document pages do not reference it") {
    withSite(tree("")): (_, target) =>
      assert(!File(target, "search.html").exists)
      assert(!File(target, "search.json").exists)
      assert(File(target, "assets/js/search.js").isFile)
      val doc: String = read(target, "archive/rgada/282.html")
      assert(!doc.contains("header-search"), doc)
      assert(!doc.contains("search.js"), doc)
      assert(!doc.contains("search.json"), doc)
  }

  test("enabled search writes diplomatic records and keeps excluded documents on the year page") {
    withSite(tree(enabled)): (site, target) =>
      assert(File(target, "search.html").isFile)
      assert(File(target, "search.json").isFile)
      val json: String = read(target, "search.json")
      assert(json.startsWith("""{"version":1,"records":["""), json)
      val one: String = record(json, "/archive/rgada/282.html")
      val two: String = record(json, "/archive/rgada/400.html")
      val range: String = record(json, "/archive/rgada/283.html")
      val none: String = record(json, "/archive/rgada/000.html")
      assert(one.contains(""""kind":"document""""), one)
      assert(one.contains(""""years":[1798]"""), one)
      assert(two.contains(""""years":[1800]"""), two)
      assert(range.contains(""""years":[1798,1799]"""), range)
      assert(none.contains(""""years":[]"""), none)
      assert(!json.contains("/archive/skip/999.html"), json)
      assert(!json.contains("пропущенноеслово"), json)
      val years: String = read(target, "archive-years.html")
      assert(years.contains("/archive/skip/999.html"), years)

      val doc: String = read(target, "archive/rgada/282.html")
      assert(doc.contains("""class="header-search""""), doc)
      assert(doc.contains("""action="/search""""), doc)
      assert(doc.contains("fa-magnifying-glass"), doc)
      assert(!doc.contains("search.json"), doc)
      assert(!doc.contains("/assets/js/search.js"), doc)

      val page: String = read(target, "search.html")
      assert(page.contains("/assets/js/search.js"), page)
      assert(page.contains("""id="site-search-status""""), page)
      assert(!page.contains("site-search-notice"), page)
      assert(!page.contains("site-search-needs-js"), page)
      assert(!page.contains("стемминг"), page)
      assert(!page.contains("Серверного поиска"), page)
      val scripts: Seq[String] =
        """(?s)<script type="module">(.*?)</script>""".r.findAllMatchIn(page).map(_.group(1)).toSeq
      val loader: String = scripts.find(_.contains("/assets/js/search.js")).getOrElse("")
      assert(loader.nonEmpty, page)
      assert(!loader.contains("&lt;"), loader)
      assert(!loader.contains("&amp;&amp;"), loader)

      assert(json.contains("сицслово"), json)
      assert(!json.contains("коррслово"), json)
      assert(!json.contains("сносканеиндекс"), json)
      assert(json.contains("Борухо-вичъ"), json)
      assert(one.contains("Борухо-вичъ. Боруховичъ"), one)
      assert(json.contains("Борухо-вичъ, Боруховичъ"), json)
      assert(!json.contains("Боруховичъ,"), json)
      assert(json.contains("на-хо-дилось находилось"), json)
      assert(json.split("находилось", -1).length == 2, json)
      assert(json.contains("Шалом\u05beалейхем"), json)
      assert(!json.contains("Шаломалейхем"), json)
      assert(json.contains("что-то"), json)
      assert(!json.contains("чтото"), json)
      assert(json.contains("Книго-"), json)
      assert(!json.contains("Книгопродавец"), json)
      assert(json.contains("Римскому-Кор- сакову Корсакову"), json)
      assert(!json.contains("РимскомуКорсакову"), json)
      assert(json.contains("Могилев- скою Могилевскою губерніею"), json)
      assert(!json.contains("Могилевскоюгуберніею"), json)

      val script: String = read(target, "assets/js/search.js")
      assert(script.contains("stored text \"На\" is marked when the query is \"на\""), script)
      assert(script.contains("40"), script)
      assert(script.contains("80"), script)
      assert(script.contains("""/[^\p{L}\p{N}]+/u"""), script)
      assert(!script.contains("toLocaleLowerCase"), script)
      assert(!script.contains("innerHTML"), script)

      val name: String = record(json, "/people/Zalman.html")
      assert(name.contains(""""kind":"name""""), name)

      val collection: String = record(json, "/archive/rgada/index.html")
      assert(collection.contains(""""kind":"collection""""), collection)
      assert(collection.contains(""""text":"Заголовок дела\nКраткое описание дела.""""), collection)
      assert(!collection.contains("Только в теле документа"), collection)

      assert(json.contains(""""href":"/book.html""""), json)
      assert(!json.contains("\"/book/"), json)

      assert(site.pages.rewriteRequest(Path.fromHref("/search")).map(_.toString).contains("/search.html"))
      assert(site.pages.rewriteRequest(Path.fromHref("/search.html")).map(_.toString).contains("/search.html"))
      assert(site.pages.rewriteRequest(Path.fromHref("/search.json")).isEmpty)
      val aliases: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(CollectionAliases.rewrite(Path.fromHref("/search"), aliases).map(_.toString).contains("/search.html"))
      assert(CollectionAliases.rewrite(Path.fromHref("/search.json"), aliases).isEmpty)
  }
