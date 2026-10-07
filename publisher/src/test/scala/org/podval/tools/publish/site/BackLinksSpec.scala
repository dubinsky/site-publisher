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
    "other/000.xml" -> tei("""Also <persName ref="ab">A</persName>."""),
    "col/001.xml" ->
      """<TEI>
        |  <teiHeader>
        |    <fileDesc><titleStmt>
        |      <title>001</title>
        |      <author><persName ref="ab">A</persName></author>
        |    </titleStmt></fileDesc>
        |    <profileDesc><abstract><p>Header phrase <persName ref="ab">A</persName>.</p></abstract></profileDesc>
        |  </teiHeader>
        |  <text><body>
        |    <p>Alpha <persName ref="ab">A</persName>.</p>
        |    <p>Beta <persName ref="ab">A</persName>.</p>
        |  </body></text>
        |</TEI>
        |""".stripMargin,
    "col/002.xml" ->
      """<TEI>
        |  <teiHeader>
        |    <fileDesc><titleStmt>
        |      <title>002</title>
        |      <author><persName ref="ab">A</persName></author>
        |    </titleStmt></fileDesc>
        |  </teiHeader>
        |  <text><body><p>No name here.</p></body></text>
        |</TEI>
        |""".stripMargin
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

  private def backlinks(page: String): String =
    val at: Int = page.indexOf("class=\"backlinks\"")
    assert(at >= 0, page)
    page.substring(at)

  private def detailsContaining(section: String, marker: String): String =
    val blocks: Seq[String] = section.split("<details>").toSeq.drop(1)
      .map(part => part.take(part.indexOf("</details>")))
    blocks.find(_.contains(marker)).getOrElse(fail(s"no details for $marker in $section"))

  private def hrefs(html: String): Seq[String] =
    val marker: String = "href=\""
    Iterator.unfold(0): from =>
      val at: Int = html.indexOf(marker, from)
      if at < 0 then None
      else
        val start: Int = at + marker.length
        val end: Int = html.indexOf('"', start)
        Some((html.substring(start, end), end + 1))
    .toSeq

  /** Visible text of the list item whose href is `href`. */
  private def itemText(block: String, href: String): String =
    val marker: String = s"""href="$href""""
    val at: Int = block.indexOf(marker)
    val from: Int = block.indexOf('>', at) + 1
    val until: Int = block.indexOf("</a>", from)
    block.substring(from, until)

  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  test("entity backlinks group two 000s by collection; a note stays ungrouped") {
    withSite: (_, target) =>
      val section: String = backlinks(html(target, "people/ab.html"))
      assert(section.contains("""class="backlinks-collection""""), section)
      assert(section.contains("href=\"/col/000.html#"), section)
      assert(section.contains("href=\"/other/000.html#"), section)
      assert(section.contains("href=\"/col.html\""), section)
      assert(section.contains("href=\"/other.html\""), section)
      val note: Int = section.indexOf("A note")
      val collections: Int = section.indexOf("backlinks-collection")
      assert(note >= 0, section)
      assert(collections >= 0 && collections < note, section)
  }

  test("summary link opens the first mention outside teiHeader") {
    withSite: (_, target) =>
      val section: String = backlinks(html(target, "people/ab.html"))
      val both: String = detailsContaining(section, "/col/001.html")
      val summaryHref: String = hrefs(both.take(both.indexOf("</summary>"))).head
      val list: String = both.substring(both.indexOf("backlinks-list"))
      val alphaHref: String = hrefs(list).find(href => itemText(list, href).contains("Alpha")).get
      assert(summaryHref == alphaHref, both)
      val published: String = html(target, "col/001.html")
      val fragment: String = summaryHref.drop(summaryHref.indexOf('#') + 1)
      assert(published.contains(s"""id="$fragment""""), published)
      val headerHref: String = hrefs(list).find(href => itemText(list, href).contains("Header phrase")).get
      assert(summaryHref != headerHref, both)
      val headerFragment: String = headerHref.drop(headerHref.indexOf('#') + 1)
      assert(!published.contains(s"""id="$headerFragment""""), published)

      val headerOnly: String = detailsContaining(section, "/col/002.html")
      val headerSummary: String = hrefs(headerOnly.take(headerOnly.indexOf("</summary>"))).head
      assert(headerSummary == "/col/002.html", headerOnly)
  }

  test("backlink snippet does not include date-tip calendar text") {
    withSite: (_, target) =>
      val section: String = backlinks(html(target, "people/ab.html"))
      assert(section.contains("11 августа") || section.contains("A"), section)
      assert(!section.contains("Julian"), section)
      assert(!section.contains("date-tip"), section)
      assert(!section.contains("CalendarDate"), section)
  }
