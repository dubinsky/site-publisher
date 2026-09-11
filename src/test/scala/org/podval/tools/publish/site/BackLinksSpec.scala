package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class BackLinksSpec extends AnyFunSuite:
  private def tei(body: String): String =
    s"""<TEI>
       |  <teiHeader><fileDesc><titleStmt><title>000</title></titleStmt></fileDesc></teiHeader>
       |  <text><body><p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: Backlinks Fixture
        |description: grouped backlinks
        |url: http://backlinks.test
        |author: Test
        |email: test@backlinks.test
        |lang: en
        |""".stripMargin,
    "index.md" -> "Home.\n",
    "note.md" ->
      """---
        |title: A note
        |---
        |See [[ab]].
        |""".stripMargin,
    "people/ab.xml" ->
      """<person>
        |  <persName>A</persName>
        |</person>
        |""".stripMargin,
    "col.xml" ->
      """<collection n="1" alias="col">
        |  <title>First case</title>
        |</collection>
        |""".stripMargin,
    "col/000.xml" -> tei(
      """See <persName ref="ab">A</persName> on <date when="1798-08-11">11 августа</date>."""
    ),
    "other.xml" ->
      """<collection n="2" alias="other">
        |  <title>Second case</title>
        |</collection>
        |""".stripMargin,
    "other/000.xml" -> tei("""Also <persName ref="ab">A</persName>.""")
  )

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-backlinks")
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

  test("entity backlinks group two 000s by collection; a note stays ungrouped") {
    withSite: (_, target) =>
      val page: String = html(target, "people/ab.html")
      val back: Int = page.indexOf("class=\"backlinks\"")
      assert(back >= 0, page)
      val section: String = page.substring(back)
      assert(section.contains("""class="backlinks-collection""""), section)
      assert(section.contains("""href="/col/000.html""""), section)
      assert(section.contains("""href="/other/000.html""""), section)
      assert(section.contains("""href="/col.html""""), section)
      assert(section.contains("""href="/other.html""""), section)
      val note: Int = section.indexOf("A note")
      val collections: Int = section.indexOf("backlinks-collection")
      assert(note >= 0, section)
      assert(collections >= 0 && collections < note, section)
  }

  test("backlink snippet does not include date-tip calendar text") {
    withSite: (_, target) =>
      val page: String = html(target, "people/ab.html")
      val back: Int = page.indexOf("class=\"backlinks\"")
      val section: String = page.substring(back)
      assert(section.contains("11 августа") || section.contains("A"), section)
      assert(!section.contains("Julian"), section)
      assert(!section.contains("date-tip"), section)
      assert(!section.contains("CalendarDate"), section)
  }
