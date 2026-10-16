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
      |lang: ru
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

  private def openTag(page: String, marker: String): String =
    val start: Int = page.indexOf(marker)
    assert(start >= 0, page)
    val end: Int = page.indexOf(">", start)
    assert(end >= 0, page)
    page.substring(start, end + 1)

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
      assert(viewer.contains("""href="/assets/css/facsimile.css""""), viewer)
      assert(viewer.contains("""class="facsimile-scroller""""), viewer)
      assert(!viewer.contains("facsimileWrapper"), viewer)
      assert(!viewer.contains("facsimileViewer"), viewer)
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
      assert(!transcription.contains("/assets/css/facsimile.css"), transcription)
      assert(transcription.contains("""src="/assets/js/facsimile-aside.js""""), transcription)
      val scriptAt: Int = transcription.indexOf("""src="/assets/js/facsimile-aside.js"""")
      assert(scriptAt > transcription.indexOf("</main>"), transcription)
      assert(!transcription.substring(0, transcription.indexOf("</head>")).contains("facsimile-aside.js"), transcription)
      val scriptTag: String = transcription.substring(
        transcription.lastIndexOf("<script", scriptAt),
        transcription.indexOf(">", scriptAt) + 1
      )
      assert(!scriptTag.contains("module"), scriptTag)
      assert(!html(target, "index.html").contains("/assets/js/facsimile-aside.js"))
      assert(transcription.contains("""title="Факсимиле""""), transcription)
      assert(openTag(transcription, "<html").contains("""class="facsimile-aside""""), transcription)
      val mainStart: Int = transcription.indexOf("""<main class="page-content"""")
      val mainEnd: Int = transcription.indexOf("</main>", mainStart)
      val main: String = transcription.substring(mainStart, mainEnd)
      val asideAt: Int = main.indexOf("""<aside class="facsimile-aside"""")
      assert(asideAt > main.indexOf("""<div class="wrapper""""), main)
      val asideTag: String = openTag(main, """<aside class="facsimile-aside"""")
      assert(asideTag.contains("""aria-label="Факсимиле""""), asideTag)
      assert(main.indexOf("""<aside class="facsimile-aside"""", asideAt + 1) < 0, main)
      val afterAside: String = main.substring(main.indexOf("</aside>") + "</aside>".length)
      assert(!afterAside.contains("</div>"), afterAside)
      val noscriptAt: Int = transcription.indexOf("<noscript>")
      val beforeNoscript: String = transcription.substring(0, noscriptAt)
      val noscript: String = transcription.substring(noscriptAt, transcription.indexOf("</noscript>"))
      val imgTag: String = openTag(beforeNoscript, """<img class="facsimile-aside-img"""")
      assert(imgTag.contains("""decoding="async""""), imgTag)
      assert(imgTag.contains("""alt="факсимиле листа 000""""), imgTag)
      assert(imgTag.contains("""data-facsimile-src="https://facsimiles.test/col/000-1.jpg""""), imgTag)
      assert(imgTag.contains("""data-facsimile-current="p000-1""""), imgTag)
      assert(!imgTag.contains(" src="), imgTag)
      assert(!imgTag.startsWith("<img src="), imgTag)
      assert(transcription.indexOf("""class="facsimile-aside-img"""") ==
        transcription.lastIndexOf("""class="facsimile-aside-img""""), transcription)
      assert(transcription.contains("""class="facsimile-aside-status""""), transcription)
      assert(noscript.contains("""<picture"""), noscript)
      assert(noscript.contains("""media="screen and (min-width: 1312px)""""), noscript)
      assert(noscript.contains("""srcset="https://facsimiles.test/col/000-1.jpg""""), noscript)
      assert(!noscript.contains("000-2.jpg"), noscript)
      assert(!noscript.contains("src="), noscript)
      assert(noscript.contains("""alt="факсимиле листа 000""""), noscript)
      assert(noscript.contains("""decoding="async""""), noscript)
      val second: String = openTag(transcription, """id="p000-2"""")
      assert(second.contains("""data-facsimile-src="https://facsimiles.test/col/000-2.jpg""""), second)
      assert(second.contains("""data-facsimile-alt="факсимиле листа 000об""""), second)
      assert(!transcription.contains(""" src="https://facsimiles.test/col/000-1.jpg""""), transcription)
      assert(!transcription.contains(""" src="https://facsimiles.test/col/000-2.jpg""""), transcription)
      val firstAnchor: String = openTag(transcription, """id="p000-1"""")
      assert(firstAnchor.contains("""data-facsimile-src="https://facsimiles.test/col/000-1.jpg""""), firstAnchor)
      assert(firstAnchor.contains("""data-facsimile-alt="факсимиле листа 000""""), firstAnchor)
      assert(firstAnchor.contains("""aria-describedby="p000-1-tip""""), firstAnchor)
      assert(!firstAnchor.contains("title="), firstAnchor)
      assert(transcription.contains("""class="pb-name""""), transcription)
      assert(transcription.contains(">000</span>"), transcription)
      assert(transcription.contains("000. Факсимиле."), transcription)
      assert(transcription.contains("""id="p000-1-tip""""), transcription)
      val viewerOpen: String = openTag(viewer, "<html")
      assert(viewerOpen.contains("""class="facsimile""""), viewerOpen)
      assert(!viewerOpen.contains("facsimile-aside"), viewerOpen)
      assert(!viewer.contains("""<aside class="facsimile-aside""""), viewer)
      assert(!viewer.contains("/assets/js/facsimile-aside.js"), viewer)
  }

  test("collection Страницы numbers go to the transcription") {
    withSite(): (_, target) =>
      val index: String = html(target, "col/index.html")
      assert(index.contains("""href="/col/000.html#p000-1""""), index)
      assert(!index.contains("/facsimile.html"), index)
      assert(!index.contains("data-facsimile-src"), index)
      assert(!index.contains("/assets/js/facsimile-aside.js"), index)
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
      val missing: String = openTag(transcription, """id="p002-2"""")
      assert(missing.contains("data-facsimile-missing=\"true\""), missing)
      assert(!missing.contains("href"), missing)
      assert(!missing.contains("data-facsimile-src"), missing)
      assert(transcription.contains("002об. Нет фотографии."), transcription)
      val missingEnd: Int = transcription.indexOf("""id="p003-1"""")
      val missingChunk: String = transcription.substring(transcription.indexOf("""id="p002-2""""), missingEnd)
      assert(missingChunk.contains("fa-images"), missingChunk)
      assert(missingChunk.contains("""class="pb-label""""), missingChunk)
      assert(missingChunk.contains("""class="pb-tip""""), missingChunk)
      assert(!missingChunk.contains("aria-describedby"), missingChunk)
      val emptyMissing: String = openTag(transcription, """id="p003-1"""")
      assert(emptyMissing.contains("data-facsimile-missing=\"true\""), emptyMissing)
      assert(emptyMissing.contains("data-facsimile-empty=\"true\""), emptyMissing)
      assert(!emptyMissing.contains("href"), emptyMissing)
      assert(transcription.contains("003. Нет фотографии. Пустая."), transcription)
      val present: String = openTag(transcription, """id="p002-1"""")
      assert(present.contains("""data-facsimile-src="https://facsimiles.test/col/002-1.jpg""""), present)
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
      assert(!openTag(translation, "<html").contains("facsimile-aside"), translation)
      assert(!translation.contains("""<aside class="facsimile-aside""""), translation)
      assert(!translation.contains("data-facsimile-src"), translation)
      assert(!translation.contains("/assets/js/facsimile-aside.js"), translation)
      val original: String = html(target, "col/001.html")
      assert(original.contains("""href="/col/001/facsimile.html""""), original)
      assert(original.contains("""src="/assets/js/facsimile-aside.js""""), original)
  }

  test("no facsimiles-url means no viewer and pb has no href") {
    withSite(Map(
      "_site_config.yml" ->
        """title: Facsimile Fixture
          |description: facsimile viewer tests
          |url: http://facsimile.test
          |author: Test
          |email: test@facsimile.test
          |lang: ru
          |""".stripMargin
    )): (site, target) =>
      assert(!File(target, "col/000/facsimile.html").isFile)
      assert(!site.pages.pages.exists(_.isInstanceOf[FacsimilePage]))
      val transcription: String = html(target, "col/000.html")
      assert(transcription.contains("""id="p000-1""""), transcription)
      assert(!transcription.contains("/facsimile.html"), transcription)
      assert(!openTag(transcription, "<html").contains("facsimile-aside"), transcription)
      assert(!transcription.contains("""<aside class="facsimile-aside""""), transcription)
      assert(!transcription.contains("/assets/js/facsimile-aside.js"), transcription)
  }

  test("pb@facs is the image src") {
    withSite(): (_, target) =>
      val viewer: String = html(target, "col/003/facsimile.html")
      assert(viewer.contains("""src="https://example.test/custom.jpg""""), viewer)
      assert(!viewer.contains("003-1.jpg"), viewer)
      val transcription: String = html(target, "col/003.html")
      val anchor: String = openTag(transcription, """id="p003-1"""")
      assert(anchor.contains("""data-facsimile-src="https://example.test/custom.jpg""""), anchor)
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

  test("named-windows off keeps facsimile/text targets and does not set window name") {
    withSite(): (_, target) =>
      val home: String = html(target, "index.html")
      assert(!home.contains("""data-window-name="""), home)
      assert(!home.contains("hierarchyViewer"), home)
      val transcription: String = html(target, "col/000.html")
      assert(transcription.contains("""target="facsimile""""), transcription)
      assert(!transcription.contains("facsimileViewer"), transcription)
      val viewer: String = html(target, "col/000/facsimile.html")
      assert(viewer.contains("""target="text""""), viewer)
      assert(!viewer.contains("textViewer"), viewer)
  }

  test("named-windows on uses four collector viewer names") {
    withSite(Map("_site_config.yml" -> (siteConfig + "named-windows: true\n"))): (_, target) =>
      val home: String = html(target, "index.html")
      assert(home.contains("""data-window-name="hierarchyViewer""""), home)
      assert(home.contains("""target="hierarchyViewer""""), home)
      val transcription: String = html(target, "col/000.html")
      assert(transcription.contains("""data-window-name="textViewer""""), transcription)
      assert(transcription.contains("""target="facsimileViewer""""), transcription)
      assert(transcription.contains("""target="apparatusViewer""""), transcription)
      val anchor: String = openTag(transcription, """id="p000-1"""")
      assert(anchor.contains("""target="facsimileViewer""""), anchor)
      assert(anchor.contains("""data-facsimile-src="https://facsimiles.test/col/000-1.jpg""""), anchor)
      val viewer: String = html(target, "col/000/facsimile.html")
      assert(viewer.contains("""data-window-name="facsimileViewer""""), viewer)
      assert(viewer.contains("""target="textViewer""""), viewer)
      val person: String = html(target, "people/ab.html")
      assert(person.contains("""data-window-name="apparatusViewer""""), person)
      val index: String = html(target, "col/index.html")
      assert(index.contains("""data-window-name="hierarchyViewer""""), index)
      assert(index.contains("""target="textViewer""""), index)
  }

  test("chunked transcription keeps the aside on the full page only") {
    withSite(Map(
      "col/010.xml" ->
        """---
          |chunk: true
          |---
          |<TEI>
          |  <teiHeader>
          |    <fileDesc><titleStmt>
          |      <author><persName ref="ab">A</persName></author>
          |    </titleStmt></fileDesc>
          |    <profileDesc><abstract>Chunked</abstract></profileDesc>
          |  </teiHeader>
          |  <text xml:lang="ru"><body>
          |    <div><head>Alpha</head><pb n="010-1"/><p>one</p></div>
          |    <div><head>Beta</head><pb n="010-2"/><p>two</p></div>
          |  </body></text>
          |</TEI>
          |""".stripMargin
    )): (site, target) =>
      val full: String = html(target, "col/010.html")
      assert(full.contains("""<aside class="facsimile-aside""""), full)
      assert(openTag(full, """id="p010-1"""").contains("data-facsimile-src"), full)
      val chunk: String = html(target, "col/010/Alpha.html")
      assert(!openTag(chunk, "<html").contains("facsimile-aside"), chunk)
      assert(!chunk.contains("""<aside class="facsimile-aside""""), chunk)
      assert(!chunk.contains("data-facsimile-src"), chunk)
      assert(!chunk.contains("/assets/js/facsimile-aside.js"), chunk)
      val anchor: String = openTag(chunk, """id="p010-1"""")
      assert(anchor.contains("""href="/col/010/facsimile.html#p010-1""""), anchor)
      assert(anchor.contains("""target="facsimile""""), anchor)
      assert(File(target, "col/010/facsimile.html").isFile)
      val viewers: Seq[FacsimilePage] = site.pages.pages.collect:
        case viewer: FacsimilePage if viewer.document.titleFromPath == "010" => viewer
      assert(viewers.length == 1, viewers)
  }

  test("a page with no transcription keeps its scan in the aside") {
    withSite(Map(
      "col/004.xml" ->
        """<TEI>
          |  <teiHeader>
          |    <fileDesc><titleStmt>
          |      <author><persName ref="ab">A</persName></author>
          |    </titleStmt></fileDesc>
          |    <profileDesc><abstract>Blank verso</abstract></profileDesc>
          |  </teiHeader>
          |  <text xml:lang="ru"><body>
          |    <pb n="004-1"/>
          |    <p>recto</p>
          |    <pb n="004-2"/>
          |    <pb n="004-3" missing="true"/>
          |    <pb n="004-4"/>
          |    <p>later</p>
          |    <pb n="005-2" missing="true" empty="true"/>
          |  </body></text>
          |</TEI>
          |""".stripMargin
    )): (_, target) =>
      val transcription: String = html(target, "col/004.html")
      val recto: String = openTag(transcription, """id="p004-1"""")
      assert(recto.contains("""data-facsimile-src="https://facsimiles.test/col/004-1.jpg""""), recto)
      assert(!recto.contains("data-facsimile-blank"), recto)
      val verso: String = openTag(transcription, """id="p004-2"""")
      assert(verso.contains("data-facsimile-blank=\"true\""), verso)
      assert(verso.contains("""data-facsimile-src="https://facsimiles.test/col/004-2.jpg""""), verso)
      val missing: String = openTag(transcription, """id="p004-3"""")
      assert(missing.contains("data-facsimile-missing=\"true\""), missing)
      assert(!missing.contains("data-facsimile-blank"), missing)
      assert(!missing.contains("data-facsimile-src"), missing)
      assert(!missing.contains("href"), missing)
      val missingAt: Int = transcription.indexOf("""id="p004-3"""")
      assert(transcription.lastIndexOf("<", missingAt) >= 0, transcription)
      assert(
        transcription.substring(transcription.lastIndexOf("<", missingAt)).startsWith("<span"),
        transcription
      )
      assert(transcription.contains("004-3. Нет фотографии."), transcription)
      assert(transcription.contains("005об. Нет фотографии. Пустая."), transcription)
      assert(transcription.contains("""data-missing="Нет фотографии.""""), transcription)
      assert(transcription.contains("""data-error="Этот скан не загрузился.""""), transcription)
      val later: String = openTag(transcription, """id="p004-4"""")
      assert(!later.contains("data-facsimile-blank"), later)
      assert(later.contains("""data-facsimile-src="https://facsimiles.test/col/004-4.jpg""""), later)
      val viewer: String = html(target, "col/004/facsimile.html")
      assert(viewer.contains("""src="https://facsimiles.test/col/004-2.jpg""""), viewer)
      assert(!viewer.contains("004-3.jpg"), viewer)
      assert(!viewer.contains("005-2.jpg"), viewer)
  }

  test("transcluded transcription does not grow a facsimile aside") {
    withSite(Map(
      "host.md" ->
        """---
          |title: Host
          |---
          |![[col/000]]
          |""".stripMargin
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(!openTag(host, "<html").contains("facsimile-aside"), host)
      assert(!host.contains("""<aside class="facsimile-aside""""), host)
      assert(!host.contains("/assets/js/facsimile-aside.js"), host)
      assert(!host.contains("data-facsimile-src"), host)
      val copied: String = openTag(host, """class="pb"""")
      assert(copied.contains("/col/000/facsimile.html#"), copied)
      assert(copied.contains("""target="facsimile""""), copied)
  }
