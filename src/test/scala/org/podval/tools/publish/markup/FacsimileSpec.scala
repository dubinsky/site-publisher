package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{FacsimilePage, MarkupPage, Page}
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class FacsimileSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Facsimile Fixture
      |description: facsimile viewer tests
      |url: http://facsimile.test
      |author: Test
      |email: test@facsimile.test
      |facsimiles-url: https://facsimiles.test/
      |""".stripMargin

  private def tei(
    abstractText: String,
    lang: String = "ru",
    pbs: String,
    body: String = "hello"
  ): String =
    s"""<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt>
       |      <author><persName ref="ab">A</persName></author>
       |    </titleStmt></fileDesc>
       |    <profileDesc>
       |      <abstract>$abstractText</abstract>
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
    "people/ab.xml" ->
      """<person>
        |  <persName>A</persName>
        |</person>
        |""".stripMargin,
    "col.xml" ->
      """<collection n="1" alias="col">
        |  <title>Case</title>
        |</collection>
        |""".stripMargin,
    "col/000.xml" -> tei(
      abstractText = "Cover",
      pbs = """<pb n="000-1"/><pb n="000-2"/>"""
    ),
    "col/001.xml" -> tei(
      abstractText = "Hebrew",
      lang = "he",
      pbs = """<pb n="001-1"/><pb n="001-2"/>""",
      body = "hebrew"
    ),
    "col/001-ru.xml" -> tei(
      abstractText = "Russian translation",
      pbs = """<pb n="001-1"/>""",
      body = "russian"
    ),
    "col/002.xml" -> tei(
      abstractText = "Later",
      pbs = """<pb n="002-1"/><pb n="002-2" missing="true"/><pb n="003-1" missing="true" empty="true"/>""",
      body = "later"
    ),
    "col/003.xml" -> tei(
      abstractText = "Override",
      pbs = """<pb n="003-1" facs="https://example.test/custom.jpg"/>"""
    )
  )

  private def withSite(
    extra: Map[String, String] = Map.empty
  )(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-facsimile")
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

  test("imageUrl joins base, source directory, and n.jpg; facs wins") {
    assert(
      Facsimile.imageUrl("https://facsimiles.test/", Seq("col"), "000-1", None) ==
        "https://facsimiles.test/col/000-1.jpg"
    )
    assert(
      Facsimile.imageUrl("https://facsimiles.test", Seq("col"), "000-1", None) ==
        "https://facsimiles.test/col/000-1.jpg"
    )
    assert(
      Facsimile.imageUrl("https://facsimiles.test/", Seq.empty, "000-1", None) ==
        "https://facsimiles.test/000-1.jpg"
    )
    assert(
      Facsimile.imageUrl(
        "https://facsimiles.test/",
        Seq("col"),
        "000-1",
        Some("https://example.test/custom.jpg")
      ) == "https://example.test/custom.jpg"
    )
  }

  test("viewer page lists non-missing photos; pb links to the viewer") {
    withSite(): (_, target) =>
      val viewer: String = html(target, "col/000/facsimile.html")
      assert(viewer.contains("""class="facsimile""""), viewer)
      assert(viewer.contains("""class="facsimile-scroller""""), viewer)
      assert(viewer.contains("""src="https://facsimiles.test/col/000-1.jpg""""), viewer)
      assert(viewer.contains("""src="https://facsimiles.test/col/000-2.jpg""""), viewer)
      assert(viewer.contains("""id="p000-1""""), viewer)
      assert(viewer.contains("""href="/col/000.html#p000-1""""), viewer)
      assert(viewer.contains("""target="text""""), viewer)
      assert(viewer.contains(">000<"), viewer)
      assert(viewer.contains(">000об<"), viewer)
      val transcription: String = html(target, "col/000.html")
      assert(transcription.contains("""id="p000-1""""), transcription)
      assert(transcription.contains("""href="/col/000/facsimile.html#p000-1""""), transcription)
      assert(transcription.contains("""target="facsimile""""), transcription)
      assert(transcription.contains("""class="icon-span grey fa-classic fa-solid fa-images""""), transcription)
      assert(!transcription.contains("tei-class="), transcription)
      assert(transcription.contains("""title="Facsimile""""), transcription)
  }

  test("collection Страницы numbers go to the transcription") {
    withSite(): (_, target) =>
      val index: String = html(target, "col/index.html")
      assert(index.contains("""href="/col/000.html#p000-1""""), index)
      assert(!index.contains("/facsimile.html"), index)
      assert(!index.contains("fa-images"), index)
      assert(!index.contains("""target="facsimile""""), index)
  }

  test("missing pb is omitted from the scroller and kept in the transcription") {
    withSite(): (_, target) =>
      val viewer: String = html(target, "col/002/facsimile.html")
      assert(viewer.contains("""src="https://facsimiles.test/col/002-1.jpg""""), viewer)
      assert(!viewer.contains("002-2.jpg"), viewer)
      assert(!viewer.contains("003-1.jpg"), viewer)
      val transcription: String = html(target, "col/002.html")
      assert(transcription.contains("""id="p002-2""""), transcription)
      assert(transcription.contains("""id="p003-1""""), transcription)
  }

  test("translation shares the original viewer") {
    withSite(): (site, target) =>
      assert(!File(target, "col/001-ru/facsimile.html").isFile)
      assert(site.pages.pages.exists:
        case page: FacsimilePage => page.document.titleFromPath == "001"
        case _ => false
      )
      assert(!site.pages.pages.exists:
        case page: FacsimilePage => page.document.titleFromPath == "001-ru"
        case _ => false
      )
      val translation: String = html(target, "col/001-ru.html")
      assert(translation.contains("""href="/col/001/facsimile.html#p001-1""""), translation)
      val original: String = html(target, "col/001.html")
      assert(original.contains("""href="/col/001/facsimile.html""""), original)
  }

  test("no facsimiles-url means no viewer and pb has no href") {
    withSite(Map(
      "_site_config.yml" ->
        """title: Facsimile Fixture
          |description: facsimile viewer tests
          |url: http://facsimile.test
          |author: Test
          |email: test@facsimile.test
          |""".stripMargin
    )): (site, target) =>
      assert(!File(target, "col/000/facsimile.html").isFile)
      assert(!site.pages.pages.exists(_.isInstanceOf[FacsimilePage]))
      val transcription: String = html(target, "col/000.html")
      assert(transcription.contains("""id="p000-1""""), transcription)
      assert(!transcription.contains("/facsimile.html"), transcription)
  }

  test("pb@facs is the image src") {
    withSite(): (_, target) =>
      val viewer: String = html(target, "col/003/facsimile.html")
      assert(viewer.contains("""src="https://example.test/custom.jpg""""), viewer)
      assert(!viewer.contains("003-1.jpg"), viewer)
  }

  test("inbound collector facsimile URL rewrites to the viewer") {
    withSite(): (site, _) =>
      def rewritten(href: String): Option[String] =
        site.pages.rewriteRequest(Path.fromHref(href)).map(_.toString)
      assert(rewritten("/col/facsimile/000").contains("/col/000/facsimile.html"), rewritten("/col/facsimile/000"))
      assert(rewritten("/col/facsimile/000.html").contains("/col/000/facsimile.html"), rewritten("/col/facsimile/000.html"))
      assert(rewritten("/col/000/facsimile").contains("/col/000/facsimile.html"), rewritten("/col/000/facsimile"))
      assert(rewritten("/col/facsimile/001-ru").contains("/col/001/facsimile.html"), rewritten("/col/facsimile/001-ru"))
  }

  test("viewer is not a directory listing sibling") {
    withSite(): (site, target) =>
      val index: String = html(target, "col/index.html")
      assert(!index.contains("""class="document"><a href="/col/000/facsimile.html""""), index)
      val first: MarkupPage = pageNamed(site, "000").asInstanceOf[MarkupPage]
      val last: MarkupPage = pageNamed(site, "003").asInstanceOf[MarkupPage]
      val firstViewer: FacsimilePage = site.pages.facsimilePage(first).get
      val lastViewer: FacsimilePage = site.pages.facsimilePage(last).get
      assert(firstViewer.next.contains(site.pages.facsimilePage(pageNamed(site, "001")).get), firstViewer.next)
      assert(lastViewer.prev.contains(site.pages.facsimilePage(pageNamed(site, "002")).get), lastViewer.prev)
  }
