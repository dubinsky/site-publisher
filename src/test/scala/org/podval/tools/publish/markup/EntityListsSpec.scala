package org.podval.tools.publish.markup

import org.podval.tools.publish.page.EntityLists as EntityListHtml
import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class EntityListsSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Names Fixture
      |description: Entity list tests
      |url: http://names.test
      |author: Test
      |email: test@names.test
      |""".stripMargin

  private val defaultFiles: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "index.md" ->
      """---
        |title: Home
        |---
        |Home.
        |""".stripMargin,
    "names.xml" ->
      """<entityLists>
        |  <title>Имена</title>
        |  <listPerson n="jews" role="jew"><title>Жиды</title></listPerson>
        |  <listPerson n="officials" role="official"><title>Начальство</title></listPerson>
        |  <listPerson n="unknown"><title>Неизвестно кто</title></listPerson>
        |  <listPlace n="places"><title>Места</title></listPlace>
        |  <listOrg n="organizations"><title>Организации</title></listOrg>
        |</entityLists>
        |""".stripMargin,
    "names/ab.xml" ->
      """<person role="jew">
        |  <persName>Earlier</persName>
        |</person>
        |""".stripMargin,
    "names/alter-rebbe.xml" ->
      """<person role="jew">
        |  <persName>Залман Борухович</persName>
        |  <p>founder</p>
        |</person>
        |""".stripMargin,
    "names/plain.xml" ->
      """<person>
        |  <persName>Nobody</persName>
        |</person>
        |""".stripMargin,
    "names/Вильна.xml" ->
      """<place>
        |  <placeName>Вильна</placeName>
        |</place>
        |""".stripMargin,
    "doc.xml" ->
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><title>A document</title></titleStmt></fileDesc></teiHeader>
        |  <text><body>
        |    <p>See <persName ref="alter-rebbe">the Rebbe</persName>.</p>
        |    <listPerson n="inline"><title>Not a directory list</title></listPerson>
        |  </body></text>
        |</TEI>
        |""".stripMargin
  )

  private def withSite(
    extra: Map[String, String] = Map.empty
  )(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-entity-lists")
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

  test("combined names index groups by kind and role") {
    withSite(): (_, target) =>
      val page: String = html(target, "names/index.html")
      assert(page.contains("Имена"), page)
      assert(page.contains("Жиды"), page)
      assert(page.contains("Залман Борухович"), page)
      assert(page.contains("""href="/names/alter-rebbe.html""""), page)
      assert(page.contains("""href="/names/ab.html""""), page)
      assert(page.contains("Вильна"), page)
      assert(page.contains("""href="/names/Вильна.html""""), page)
      assert(page.contains("Nobody"), page)
      assert(page.contains("""id="jews""""), page)
      assert(page.contains("""href="#jews""""), page)
      assert(page.contains("""class="entity-lists-toc""""), page)
      assert(page.contains("<li"), page)
      assert(page.contains(EntityListHtml.expand), page)
      assert(page.contains("""href="/names/jews.html""""), page)
      assert(!page.contains("Начальство"), page)
      assert(!page.contains("Организации"), page)
      assert(!page.contains("""class="page-list""""), page)
      assert(!page.contains("Not a directory list"), page)
  }

  test("empty lists have no subpage") {
    withSite(): (_, target) =>
      assert(File(target, "names/jews.html").isFile)
      assert(File(target, "names/places.html").isFile)
      assert(File(target, "names/unknown.html").isFile)
      assert(!File(target, "names/officials.html").exists)
      assert(!File(target, "names/organizations.html").exists)
  }

  test("list label is the first name, not the file name") {
    withSite(): (_, target) =>
      val page: String = html(target, "names/index.html")
      assert(page.contains("Залман Борухович"), page)
      assert(!page.contains(">alter-rebbe<"), page)
  }

  test("members are sorted by file name") {
    withSite(): (_, target) =>
      val page: String = html(target, "names/index.html")
      val earlier: Int = page.indexOf("Earlier")
      val zalman: Int = page.indexOf("Залман Борухович")
      assert(earlier >= 0 && zalman >= 0 && earlier < zalman, page)
  }

  test("list subpage has members of that list only") {
    withSite(): (_, target) =>
      val jews: String = html(target, "names/jews.html")
      assert(jews.contains("Жиды"), jews)
      assert(jews.contains("Залман Борухович"), jews)
      assert(jews.contains("""href="/names/alter-rebbe.html""""), jews)
      assert(!jews.contains("Вильна"), jews)
      assert(!jews.contains("Nobody"), jews)
      val places: String = html(target, "names/places.html")
      assert(places.contains("Места"), places)
      assert(places.contains("Вильна"), places)
      assert(!places.contains("Залман"), places)
  }

  test("listPerson inside a TEI document is not filled from the directory") {
    withSite(): (_, target) =>
      val page: String = html(target, "doc.html")
      assert(page.contains("Not a directory list"), page)
      assert(!page.contains("Залман Борухович"), page)
  }

  test("entity page keeps document backlinks and does not list the names index") {
    withSite(): (_, target) =>
      val page: String = html(target, "names/alter-rebbe.html")
      assert(page.contains("Backlinks"), page)
      val back: Int = page.indexOf("class=\"backlinks\"")
      assert(back >= 0, page)
      val section: String = page.substring(back)
      assert(section.contains("A document") || section.contains("/doc.html"), section)
      assert(!section.contains("/names/index.html"), section)
  }
