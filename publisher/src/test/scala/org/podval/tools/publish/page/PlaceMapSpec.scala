package org.podval.tools.publish.page

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class PlaceMapSpec extends AnyFunSuite:
  test("parseGeo reads latitude then longitude") {
    assert(PlaceMap.parseGeo("54.68722 25.28000") == Some((54.68722, 25.28)))
    assert(PlaceMap.parseGeo("  -10.5   20 ") == Some((-10.5, 20.0)))
    assert(PlaceMap.parseGeo("91 10").isEmpty)
    assert(PlaceMap.parseGeo("10 181").isEmpty)
    assert(PlaceMap.parseGeo("1 2 3").isEmpty)
    assert(PlaceMap.parseGeo("nope").isEmpty)
  }

  test("circle area follows the case count") {
    assert(PlaceMap.radius(4, 4) == 18.0)
    assert(PlaceMap.radius(1, 4) == 9.0)
    assert(PlaceMap.radius(0, 4) == 4.5)
    assert(PlaceMap.radius(1, 0) == 4.5)
  }

  test("a place far from the cluster stays outside the frame") {
    val places: Seq[PlaceMap.Place] = Seq(
      place("Калиш", 51.757, 18.083),
      place("Каменец", 48.68056, 26.58056),
      place("Вильна", 54.68722, 25.28),
      place("Петербург", 59.95, 30.31667),
      place("Ляды", 54.60139, 31.16972),
      place("Варшава", 52.23, 21.01111),
      place("Могилев", 53.90894, 30.34296),
      place("Гамбург", 53.55, 10.0),
      place("Иерусалим", 31.77668, 35.23416)
    )
    val (inside, outside) = PlaceMap.partition(places)
    assert(inside.exists(_.name == "Калиш"))
    assert(inside.exists(_.name == "Каменец"))
    assert(outside.map(_.name).toSet == Set("Гамбург", "Иерусалим"))
  }

  private def place(name: String, lat: Double, lon: Double): PlaceMap.Place =
    PlaceMap.Place(name, s"/$name", None, lat, lon, 1)

  test("the map counts a case once, draws a ring, and tables an outlier") {
    withSite(): (_, target) =>
      val page: String = html(target, "name/places.html")
      val svgEnd: Int = page.indexOf("</svg>")
      assert(svgEnd > 0, page)
      val svg: String = page.substring(0, svgEnd)
      val rest: String = page.substring(svgEnd)
      assert(svg.contains("place-map-dot"), svg)
      assert(svg.contains("place-map-ring"), svg)
      assert(svg.contains("place-map-river"), svg)
      assert(svg.contains("place-map-coast"), svg)
      assert(svg.contains("""href="/name/Вильна.html""""), svg)
      assert(svg.contains("""href="/name/Рига.html""""), svg)
      assert(svg.contains("Вильна, 2"), svg)
      assert(svg.contains("Рига, 0"), svg)
      assert(!svg.contains("Иерусалим"), svg)
      assert(!svg.contains("target="), svg)
      assert(rest.contains("Иерусалим"), rest)
      assert(rest.contains("за рамкой"), rest)
      assert(rest.contains("берег"), rest)
      assert(!File(target, "_place-map-base.txt").exists)
  }

  test("a site without a base file still draws the map") {
    withSite(includeBase = false): (_, target) =>
      val page: String = html(target, "name/places.html")
      assert(page.contains("place-map-dot"), page)
      assert(page.contains("Вильна, 2"), page)
      assert(!page.contains("place-map-river"), page)
      assert(!page.contains("place-map-coast"), page)
      assert(!page.contains("берег"), page)
  }

  private def withSite(includeBase: Boolean = true)(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-place-map")
    try
      val dir: File = path.toFile
      val written: Map[String, String] =
        if includeBase then files + ("_place-map-base.txt" -> baseText) else files
      written.foreach: (relative, content) =>
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

  private def tei(body: String): String =
    s"""<TEI>
       |  <teiHeader><fileDesc><titleStmt><title>Leaf</title></titleStmt></fileDesc></teiHeader>
       |  <text><body><p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private val baseText: String =
    """# creek
      |25.0 55.0
      |25.2 55.1
      |
      |# coast
      |24.0 57.0
      |24.5 57.2
      |""".stripMargin

  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: Place Map Fixture
        |description: place map
        |url: http://places.test
        |author: Test
        |email: test@places.test
        |lang: ru
        |""".stripMargin,
    "index.md" -> "Home.\n",
    "name.xml" ->
      """<entityLists>
        |  <title>Имена</title>
        |  <listPerson n="people"><title>Люди</title></listPerson>
        |  <listPlace n="places"><title>Места</title></listPlace>
        |</entityLists>
        |""".stripMargin,
    "name/Кто.xml" ->
      """<person>
        |  <persName>Кто</persName>
        |</person>
        |""".stripMargin,
    "name/Вильна.xml" ->
      """<place>
        |  <placeName>Вильна</placeName>
        |  <location><geo>54.68722 25.28000</geo></location>
        |</place>
        |""".stripMargin,
    "name/Рига.xml" ->
      """<place>
        |  <placeName>Рига</placeName>
        |  <location><geo>56.94750 24.10694</geo></location>
        |</place>
        |""".stripMargin,
    "name/Иерусалим.xml" ->
      """<place>
        |  <placeName>Иерусалим</placeName>
        |  <location><geo>31.77668 35.23416</geo></location>
        |</place>
        |""".stripMargin,
    "inv.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="case">
        |    <xi:include href="inv/case/one.xml"/>
        |    <xi:include href="inv/case/two.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "inv/case/one.xml" ->
      """<collection n="1">
        |  <title>One</title>
        |</collection>
        |""".stripMargin,
    "inv/case/one/000.xml" -> tei("""<placeName ref="Вильна">Вильна</placeName>"""),
    "inv/case/one/001.xml" -> tei("""<placeName ref="Вильна">Вильне</placeName>"""),
    "inv/case/two.xml" ->
      """<collection n="2">
        |  <title>Two</title>
        |</collection>
        |""".stripMargin,
    "inv/case/two/000.xml" -> tei("""<placeName ref="Вильна">Вильна</placeName>"""),
    "loose.xml" -> tei("""<placeName ref="Вильна">Вильна</placeName>"""),
    "books.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="book">
        |    <xi:include href="books/book/tale.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "books/book/tale.xml" ->
      """<collection n="tale">
        |  <title>Tale</title>
        |</collection>
        |""".stripMargin,
    "books/book/tale/000.xml" -> tei("""<placeName ref="Вильна">Вильна</placeName>""")
  )
