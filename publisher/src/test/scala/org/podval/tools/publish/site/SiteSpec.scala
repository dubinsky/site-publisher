package org.podval.tools.publish.site

import org.apache.pdfbox.Loader
import org.apache.pdfbox.text.PDFTextStripper
import org.podval.tools.publish.util.{Files, PdfNamedDestinations, SiteOptions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class SiteSpec extends AnyFunSuite, BeforeAndAfterAll:
  private val repositoryRoot: File =
    val anchor = getClass.getResource("/anchor.txt")
    require(anchor != null, "missing test resource /anchor.txt")
    File(anchor.toURI)
      .getParentFile // resources
      .getParentFile // test
      .getParentFile // src
      .getParentFile // root

  private val sourceDirectory: File = File(repositoryRoot, "src/test/site")
  private val targetDirectory: File = File(repositoryRoot, "build/test-site")

  override def beforeAll(): Unit =
    super.beforeAll()
    Site(SiteOptions(
      sourceDirectoryPath = sourceDirectory.getAbsolutePath,
      targetDirectoryNameOpt = Some(targetDirectory.getAbsolutePath),
      treatErrorsAsWarnings = true,
      logLevelOpt = Some("WARN")
    )).generate()

  private def html(relative: String): String =
    htmlRaw(relative).replaceAll("\\s+", " ").replace("= ", "=")

  private def htmlRaw(relative: String): String =
    val file: File = File(targetDirectory, relative)
    assert(file.isFile, s"missing $relative under $targetDirectory")
    Files.read(file)

  private def htmlOpen(page: String): String =
    val start: Int = page.indexOf("<html")
    assert(start >= 0, page)
    val end: Int = page.indexOf(">", start)
    assert(end >= 0, page)
    page.substring(start, end + 1)

  private def tagAt(page: String, id: String): String =
    val at: Int = page.indexOf(id)
    assert(at >= 0, page)
    val start: Int = page.lastIndexOf("<", at)
    val end: Int = page.indexOf(">", at)
    page.substring(start, end + 1)

  private val readerConfig: String =
    """title: T
      |description: D
      |url: http://t.test
      |author: A
      |email: a@t.test
      |""".stripMargin

  private val choiceHtml: String =
    "<p><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></p>\n"

  private def choiceTei(body: String): String =
    s"""<TEI>
       |  <teiHeader><fileDesc><titleStmt><title>t</title></titleStmt></fileDesc></teiHeader>
       |  <text><body><p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private def headerChoiceTei: String =
    """<TEI>
      |  <teiHeader>
      |    <fileDesc><titleStmt><title>t</title></titleStmt></fileDesc>
      |    <profileDesc>
      |      <abstract><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></abstract>
      |    </profileDesc>
      |  </teiHeader>
      |  <text><body><p>hello</p></body></text>
      |</TEI>
      |""".stripMargin

  private def rootStore(extra: Map[String, String]): Map[String, String] =
    Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "_terms.xml" ->
        """<?xml version="1.0" encoding="UTF-8"?>
          |<Terms>
          |  <term id="archive" selector="yes">
          |    <name lang="en" n="archive" plural="archives"/>
          |  </term>
          |</Terms>
          |""".stripMargin,
      "archive.xml" ->
        """<store xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <by selector="archive">
          |    <xi:include href="archive/rgada.xml"/>
          |  </by>
          |</store>
          |""".stripMargin,
      "archive/rgada.xml" ->
        """<collection n="1">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "archive/rgada/900.xml" -> choiceTei("hello")
    ) ++ extra

  private def withReaderSite(files: Map[String, String])(body: File => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-reader")
    try
      val dir: File = path.toFile
      files.foreach: (relative, content) =>
        Files.write(File(dir, relative), content)
      val target: File = File(dir, "_site")
      Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(target.getAbsolutePath),
        treatErrorsAsWarnings = true,
        logLevelOpt = Some("WARN")
      )).generate()
      body(target)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def readerHtml(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  private def assertExpansions(page: String): Unit =
    val tag: String = tagAt(page, """id="setting-abbr-expand"""")
    assert(tag.contains("""checked="checked""""), tag)

  private def assertNoExpansions(page: String): Unit =
    assert(!page.contains("""id="setting-abbr-expand""""), page)

  private def assertAbsorbedChoice(target: File, relative: String, marker: String): Unit =
    val page: String = readerHtml(target, relative)
    assert(page.contains(marker), page)
    assert(page.contains("ChoiceMarker"), page)
    assertExpansions(page)
    val home: String = readerHtml(target, "index.html")
    assert(!home.contains("ChoiceMarker"), home)
    assertExpansions(home)

  private def exists(relative: String): Unit =
    assert(File(targetDirectory, relative).isFile, s"missing $relative under $targetDirectory")

  test("generates into build/test-site, not _site under the fixture") {
    assert(targetDirectory.isDirectory, targetDirectory)
    assert(targetDirectory.getAbsolutePath.contains(s"${File.separator}build${File.separator}test-site"))
    assert(!File(sourceDirectory, "_site").exists)
  }

  test("writes synthetic pages, CSS, sitemap, robots, and feed") {
    exists("index.html")
    exists("errors.html")
    exists("tags.html")
    exists("posts.html")
    exists("sitemap.xml")
    exists("robots.txt")
    exists("feed.xml")
    exists("assets/css/style.css")
  }

  test("conflicting front matter and content titles are recorded") {
    val errors: String = html("errors.html")
    assert(errors.contains("ambiguous title"), errors)
    assert(errors.contains("title-conflict"), errors)
    assert(errors.contains("Front Matter Title"), errors)
    assert(errors.contains("Content Title"), errors)
    assert(!errors.contains("title-same"), errors)
    val page: String = html("title-conflict.html")
    assert(page.contains("Content Title"), page)
  }

  test("site header has a settings menu with glossary checkbox") {
    val home: String = html("index.html")
    assert(home.contains("""class="site-settings""""), home)
    assert(home.contains("""class="site-settings-toggle""""), home)
    assert(home.contains("fa-gear"), home)
    assert(home.contains("""id="setting-glossary-expand""""), home)
    assert(home.contains("""data-setting="glossary-expand""""), home)
    assert(home.contains("Term definitions and citations"), home)
    assert(!home.contains("Show glossary definitions in the text"), home)
    assert(!tagAt(home, """id="setting-glossary-expand"""").contains("checked"), home)
    assert(!home.contains("""id="setting-abbr-expand""""), home)
    assert(!home.contains("Abbreviation expansions and corrections"), home)
    assert(!home.contains("Show expansions and corrections in the text"), home)
    assert(home.contains("""class="site-settings-heading""""), home)
    assert(home.contains("""id="setting-in-text-label""""), home)
    assert(home.contains("""aria-labelledby="setting-in-text-label""""), home)
    assert(home.contains("In the text"), home)
    val headingAt: Int = home.indexOf("""id="setting-in-text-label"""")
    val headingBefore: String = home.slice((headingAt - 80).max(0), headingAt)
    assert(headingBefore.contains("<div"), headingBefore)
    assert(headingBefore.contains("""class="site-settings-heading""""), headingBefore)
    assert(!headingBefore.contains("<h2"), headingBefore)
    assert(!headingBefore.contains("<label"), headingBefore)
    val inTextSection: Int = home.indexOf("""aria-labelledby="setting-in-text-label"""")
    val inTextBefore: String = home.slice((inTextSection - 90).max(0), inTextSection)
    assert(inTextBefore.contains("""class="site-settings-section""""), inTextBefore)
    assert(inTextBefore.contains("""role="group""""), inTextBefore)
    assert(home.contains("""id="setting-transclusion-clean""""), home)
    assert(home.contains("""data-setting="transclusion-clean""""), home)
    assert(!tagAt(home, """id="setting-transclusion-clean"""").contains("checked"), home)
    assert(home.contains("Seamless transclusions"), home)
    assert(!home.contains("""id="setting-facsimile-aside""""), home)
    assert(!home.contains("Show the facsimile beside the text"), home)
    val colorAt: Int = home.indexOf("""id="setting-color-scheme-label"""")
    val glossaryAt: Int = home.indexOf("""id="setting-glossary-expand"""")
    val transclusionAt: Int = home.indexOf("""id="setting-transclusion-clean"""")
    assert(colorAt >= 0 && colorAt < headingAt && headingAt < glossaryAt && glossaryAt < transclusionAt, home)
    assert(home.split("""class="site-settings-section"""").length - 1 == 3, home)
    assert(!home.contains("glossary-expand-toggle"), home)
    assert(home.contains("""id="setting-color-scheme-label""""), home)
    assert(home.contains("Color scheme"), home)
    assert(home.contains("""name="color-scheme" value="system" checked="checked""""), home)
    assert(home.contains("""name="color-scheme" value="light""""), home)
    assert(home.contains("""name="color-scheme" value="dark""""), home)
    val head: String = home.substring(0, home.indexOf("</head>"))
    assert(head.contains("abbr-expand"), head)
    assert(head.contains("""["abbr-expand", "facsimile-aside"]"""), head)
    assert(!head.contains("/assets/js/facsimile-aside.js"), head)
    assert(!home.contains("/assets/js/facsimile-aside.js"), home)
    assert(head.contains("[data-setting]"), head)
    assert(head.contains("localStorage"), head)
    assert(head.contains("prefers-color-scheme"), head)
    assert(head.contains("color-scheme-dark"), head)
    assert(head.contains("site-color-scheme"), head)
    assert(head.contains("mathjax") || home.contains("mathjax"), home)
    val tokens: String = html("assets/css/tokens.css")
    assert(tokens.contains("html.color-scheme-dark"), tokens)
    assert(tokens.contains("@media screen"), tokens)
    assert(tokens.contains("#1e69d8"), tokens)
    assert(tokens.contains("#5b93e6"), tokens)
    assert(tokens.contains("--note-editorial: var(--important)"), tokens)
    assert(!tokens.contains("--note-color: var(--danger)"), tokens)
    val graph: String = html("assets/js/graph.js")
    assert(graph.contains("--graph-label-color"), graph)
    assert(graph.contains("site-color-scheme"), graph)
  }

  test("fixture home has no apparatus menu and no data-apparatus attribute") {
    val home: String = html("index.html")
    assert(!home.contains("""name="apparatus-add""""), home)
    assert(!home.contains("""name="apparatus-del""""), home)
    assert(!home.contains("""name="apparatus-see""""), home)
    assert(!home.contains("data-apparatus-enabled"), home)
    assert(!home.contains("""data-apparatus=""""), home)
    assert(!htmlOpen(home).contains("data-apparatus-kinds"), htmlOpen(home))
    val head: String = home.substring(0, home.indexOf("</head>"))
    assert(head.contains("data-apparatus-kinds"), head)
    assert(head.contains("apparatus-add"), head)
    val prose: String = html("assets/css/prose.css")
    assert(prose.contains("html.apparatus-add-hidden"), prose)
    assert(prose.contains("--note-editorial"), prose)
    assert(prose.contains("font-style: italic"), prose)
    assert(prose.contains("[data-apparatus=\"del\"]"), prose)
    assert(prose.contains("text-decoration: line-through"), prose)
    assert(!prose.contains("content: \"(\""), prose)
    val tei: String = html("assets/css/tei.css")
    val pbAt: Int = tei.indexOf("a.pb {")
    assert(pbAt >= 0, tei)
    val pbBlock: String = tei.substring(pbAt, tei.indexOf("}", pbAt))
    assert(pbBlock.contains("white-space: nowrap"), pbBlock)
    assert(tei.contains("choice > expan"), tei)
    assert(tei.contains("font-style: italic"), tei)
    assert(tei.contains("html.abbr-expand-off"), tei)
    val tipAt: Int = tei.indexOf("html.abbr-expand-off choice > expan,")
    assert(tipAt >= 0, tei)
    val tipBlock: String = tei.substring(tipAt, tei.indexOf("}", tipAt))
    assert(tipBlock.contains("html.abbr-expand-off choice > reg"), tipBlock)
    assert(tipBlock.contains("display: none"), tipBlock)
    assert(tipBlock.contains("position: absolute"), tipBlock)
    assert(!tipBlock.contains("font-style: italic"), tipBlock)
    val clearedAt: Int = tei.indexOf("html.abbr-expand-off choice > expan:before")
    val clearedBlock: String = tei.substring(clearedAt, tei.indexOf("}", clearedAt))
    assert(clearedBlock.contains("choice orig + reg:before"), clearedBlock)
    assert(clearedBlock.contains("choice reg + orig:before"), clearedBlock)
    assert(clearedBlock.contains("choice orig + reg:after"), clearedBlock)
    assert(clearedBlock.contains("choice reg + orig:after"), clearedBlock)
    assert(clearedBlock.contains("content: none"), clearedBlock)
    val hoverAt: Int = tei.indexOf("@media (hover: hover) and (pointer: fine)")
    assert(hoverAt >= 0, tei)
    val hoverOpen: Int = tei.indexOf("{", hoverAt)
    val hoverInner: Int = tei.indexOf("}", hoverOpen)
    val hoverClose: Int = tei.indexOf("}", hoverInner + 1)
    val hoverBlock: String = tei.substring(hoverAt, hoverClose)
    assert(hoverBlock.contains("choice:hover > reg"), hoverBlock)
    assert(!hoverBlock.contains("focus-within"), hoverBlock)
    assert(tei.indexOf("choice:focus-within > reg") > hoverClose, tei)
    val expanItalic: Int = tei.indexOf("choice > expan {")
    assert(expanItalic >= 0, tei)
    val expanItalicBlock: String = tei.substring(expanItalic, tei.indexOf("}", expanItalic))
    assert(expanItalicBlock.contains("font-style: italic"), expanItalicBlock)
    assert(!expanItalicBlock.contains("reg"), expanItalicBlock)
    val chromeCss: String = html("assets/css/chrome.css")
    assert(chromeCss.contains("""[data-apparatus-kind="del"]"""), chromeCss)
    assert(chromeCss.contains("text-decoration: line-through"), chromeCss)
    val headingCss: Int = chromeCss.indexOf(".site-settings-heading {")
    assert(headingCss >= 0, chromeCss)
    val headingBlock: String = chromeCss.substring(headingCss, chromeCss.indexOf("}", headingCss))
    assert(headingBlock.contains("margin: 0 0 var(--space-2)"), headingBlock)
    assert(!headingBlock.contains("cursor"), headingBlock)
    val printCss: String = html("assets/css/print.css")
    assert(printCss.contains("html.abbr-expand-off choice > expan"), printCss)
    val printTip: Int = printCss.indexOf("html.abbr-expand-off choice > expan,")
    assert(printTip >= 0, printCss)
    val printTipBlock: String = printCss.substring(printTip, printCss.indexOf("}", printTip))
    assert(printTipBlock.contains("html.abbr-expand-off choice > reg"), printTipBlock)
    assert(printTipBlock.contains("display: inline"), printTipBlock)
    assert(printTipBlock.contains("position: static"), printTipBlock)
    assert(!printTipBlock.contains("font-style: italic"), printTipBlock)
    val printItalic: Int = printCss.indexOf("html.abbr-expand-off choice > expan {")
    assert(printItalic >= 0, printCss)
    val printItalicBlock: String = printCss.substring(printItalic, printCss.indexOf("}", printItalic))
    assert(printItalicBlock.contains("font-style: italic"), printItalicBlock)
    assert(!printItalicBlock.contains("reg"), printItalicBlock)
    val parenBefore: Int = printCss.indexOf("html.abbr-expand-off choice abbr + expan:before")
    val parenBeforeBlock: String = printCss.substring(parenBefore, printCss.indexOf("}", parenBefore))
    assert(parenBeforeBlock.contains("choice orig + reg:before"), parenBeforeBlock)
    assert(parenBeforeBlock.contains("choice reg + orig:before"), parenBeforeBlock)
    assert(parenBeforeBlock.contains("""content: " (""""), parenBeforeBlock)
    val parenAfter: Int = printCss.indexOf("html.abbr-expand-off choice abbr + expan:after")
    val parenAfterBlock: String = printCss.substring(parenAfter, printCss.indexOf("}", parenAfter))
    assert(parenAfterBlock.contains("choice orig + reg:after"), parenAfterBlock)
    assert(parenAfterBlock.contains("choice reg + orig:after"), parenAfterBlock)
    assert(parenAfterBlock.contains("""content: ")""""), parenAfterBlock)
  }

  test("collection column titles stick to the page") {
    val chrome: String = html("assets/css/chrome.css")
    val start: Int = chrome.indexOf(".collection-scroll {")
    val block: String = chrome.substring(start, chrome.indexOf("}", start))
    assert(!block.contains("overflow"), block)
    assert(chrome.contains("max-width: 960px"), chrome)
    assert(chrome.contains("overflow: visible"), chrome)
  }

  test("in-page jumps clear the sticky header") {
    val base: String = html("assets/css/base.css")
    assert(base.contains("scroll-margin-top: calc(var(--header-height) + 6px + 0.75rem)"), base)
    assert(base.contains("top: calc(var(--header-height) + 6px)"), base)
    assert(base.contains("z-index: 1"), base)
    assert(!base.contains("scroll-padding-top"), base)
  }

  test("media queries use literal breakpoints, not custom properties") {
    val chrome: String = html("assets/css/chrome.css")
    val prose: String = html("assets/css/prose.css")
    val base: String = html("assets/css/base.css")
    assert(chrome.contains("max-width: 600px"), chrome)
    assert(prose.contains("min-width: 800px"), prose)
    assert(!chrome.contains("max-width: var(--on-"), chrome)
    assert(!chrome.contains("min-width: var(--on-"), chrome)
    assert(!base.contains("max-width: var(--on-"), base)
    assert(chrome.contains("html.wide .page-content > .wrapper"), chrome)
    assert(chrome.contains("--content-width: 1800px"), chrome)
    assert(!html("assets/css/style.css").contains("custom-variables"), html("assets/css/style.css"))
  }

  test("facsimile aside layout is a screen row at 1312px and hidden in print") {
    val style: String = html("assets/css/style.css")
    val chromeAt: Int = style.indexOf("chrome.css")
    val asideAt: Int = style.indexOf("facsimile-aside.css")
    assert(chromeAt >= 0 && asideAt > chromeAt, style)
    val css: String = html("assets/css/facsimile-aside.css")
    assert(css.contains("@media screen and (min-width: 1312px)"), css)
    assert(css.contains("html.facsimile-aside:not(.facsimile-aside-off)"), css)
    assert(css.contains("flex: 0 0 auto"), css)
    assert(css.contains("object-fit: contain"), css)
    assert(css.contains("box-sizing: border-box"), css)
    assert(css.contains("position: sticky"), css)
    assert(css.contains("top: calc(var(--header-height) + 6px)"), css)
    assert(
      css.contains("height: calc(100dvh - var(--header-height) - 6px - var(--spacing-unit) - var(--space-3))"),
      css
    )
    assert(css.contains("max-width: var(--content-width)"), css)
    assert(css.contains("article.page:has(.toc) > .page-body > :has(> .toc)"), css)
    assert(css.contains("display: block"), css)
    assert(css.contains("article.page:has(.toc) > .page-body > :has(> .toc) > .toc"), css)
    assert(css.contains("position: static"), css)
    assert(css.contains("top: auto"), css)
    assert(css.contains("max-height: none"), css)
    assert(css.contains("overflow: visible"), css)
    assert(css.contains("align-self: auto"), css)
    assert(css.contains("a.pb[data-facsimile-blank]"), css)
    assert(
      css.contains("min-height: calc(100dvh - var(--header-height) - 6px - var(--spacing-unit) - var(--space-3))"),
      css
    )
    assert(css.contains("aside.transclusion a.pb[data-facsimile-blank]"), css)
    val blankAt: Int = css.indexOf("a.pb[data-facsimile-blank]")
    val screenAt: Int = css.indexOf("@media screen and (min-width: 1312px)")
    assert(screenAt >= 0 && blankAt > screenAt, css)
    assert(!css.contains("scroll-padding-top"), css)
    assert(!css.contains("html.facsimile "), css)
    assert(!css.contains("filter"), css)
    assert(!css.contains("invert"), css)
    assert(!css.contains("--content-width:"), css)
    assert(html("assets/css/tokens.css").contains("--content-width: 800px"))
    val printCss: String = html("assets/css/print.css")
    val hideAt: Int = printCss.indexOf("aside.facsimile-aside")
    assert(hideAt >= 0, printCss)
    assert(printCss.substring(hideAt, hideAt + 80).contains("display: none"), printCss)
    val script: String = html("assets/js/facsimile-aside.js")
    assert(script.contains("1312"), script)
    assert(script.contains("data-facsimile-src"), script)
    assert(script.contains("aside.transclusion"), script)
    assert(script.contains("preventDefault"), script)
    assert(script.contains("getComputedStyle"), script)
    assert(script.contains("requestAnimationFrame"), script)
    assert(script.contains("replaceState"), script)
    assert(!script.contains("scrollIntoView"), script)
  }

  test("header pages come from site config in listed order") {
    val home: String = html("index.html")
    val about: Int = home.indexOf("""href="/about.html"""")
    val posts: Int = home.indexOf("""href="/posts.html"""")
    assert(about >= 0, home)
    assert(posts >= 0, home)
    assert(about < posts, home)
  }

  test("home page wiki links resolve") {
    val page: String = html("index.html")
    assert(page.contains("Site Publisher Fixture"), page)
    assert(page.contains("""href="/notes.html""""), page)
    assert(page.contains("""href="/mermaid.html""""), page)
    assert(page.contains("""href="/glossary.html""""), page)
    assert(page.contains("""href="/cite.html""""), page)
    assert(page.contains("""href="/chunked.html""""), page)
    assert(page.contains("""href="/chunked/index.html""""), page)
    assert(page.contains("""href="/book.html""""), page)
    assert(page.contains("""href="/about.html""""), page)
    assert(page.contains("""href="/tei-sample.html""""), page)
    assert(page.contains("""href="/docbook-sample.html""""), page)
    assert(!page.contains("unresolved-link"), page)
  }

  test("Markdown notes: table, task-list IR, footnotes, fenced scala") {
    val page: String = html("notes.html")
    assert(page.contains("<table"), page)
    assert(page.contains(">1</td>") || page.contains(">1</th>") || page.contains(">1<"), page)
    assert(page.contains("""class="task-list""""), page)
    assert(page.contains("""class="task-list-item""""), page)
    assert(page.contains("task-list-item-checkbox"), page)
    assert(page.contains("open the box"), page)
    assert(page.contains("done already"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("""class="footnote-tip""""), page)
    assert(page.contains("Markdown footnote body"), page)
    assert(page.contains("""</strong><span class="footnote-ref""""), page)
    assert(page.contains("""class="footnotes""""), page)
    assert(page.contains("table-with-notes"), page)
    assert(page.contains("table-footnotes"), page)
    assert(page.contains("""id="_table_1_fn_a""""), page)
    assert(page.contains("""data-footnote-scope="table""""), page)
    assert(page.contains("Table cell note"), page)
    assert(page.contains("""id="_footnote_1""""), page)
    assert(page.contains("language-scala"), page)
    assert(page.contains("xs.map(f)"), page)
    assert(page.contains("styles/default.min.css"), page)
    assert(page.contains("""data-hljs-theme="light""""), page)
    assert(page.contains("styles/dark.min.css"), page)
    assert(page.contains("""data-hljs-theme="dark""""), page)
    assert(page.contains("""media="not all""""), page)
    assert(page.contains("""class="admonition""""), page)
    assert(page.contains("""data-type="tip""""), page)
    assert(page.contains("Save time"), page)
    assert(page.contains("""class="aside""""), page)
    assert(page.contains("From Markdown"), page)
    assert(page.contains("""class="quote""""), page)
    assert(page.contains("A Markdown quotation"), page)
    assert(page.contains("<del"), page)
    assert(page.contains("struck out"), page)
    assert(page.contains("""class="figure""""), page)
    assert(page.contains("""src="/pixel.svg""""), page)
    assert(page.contains("A Markdown figure"), page)
    assert(page.contains("""class="pdf-embed""""), page)
    assert(page.contains("""data="/sample.pdf""""), page)
    assert(page.contains("""type="application/pdf""""), page)
    assert(page.contains("<video"), page)
    assert(page.contains("""src="/clip.mp4""""), page)
    assert(page.contains("unresolved-link"), page)
    assert(page.contains("missing-page"), page)
    assert(page.contains("unresolved-asset"), page)
    assert(page.contains("missing.png"), page)
    assert(page.contains("""class="backlinks""""), page)
    exists("notes-alias.html")
    val alias: String = html("notes-alias.html")
    assert(alias.contains("/notes.html"), alias)
    assert(alias.toLowerCase.contains("refresh"), alias)
    val errors: String = html("errors.html")
    assert(errors.contains("unresolved"), errors)
    assert(errors.contains("missing-page"), errors)
    assert(errors.contains("missing asset"), errors)
    assert(errors.contains("missing.png"), errors)
  }

  test("fenced mermaid loads Pele and keeps the inline module raw") {
    val page: String = htmlRaw("mermaid.html")
    assert(page.contains("language-mermaid"), page)
    assert(page.contains("flowchart LR"), page)
    assert(page.contains("cdn.jsdelivr.net/npm/pele@1.8.0/dist/pele.js"), page)
    assert(page.contains("mount(el, el.textContent)"), page)
    assert(!page.contains("mermaid.esm.min.mjs"), page)
    assert(!page.contains("mermaid.initialize"), page)
    val at: Int = page.indexOf("mount(el, el.textContent)")
    assert(at >= 0, page)
    val script: String = page.substring(page.lastIndexOf("<script", at), page.indexOf("</script>", at))
    assert(!script.contains("&lt;"), script)
    assert(!script.contains("&amp;"), script)
    val notes: String = htmlRaw("notes.html")
    assert(!notes.contains("pele.js"), notes)
    assert(!notes.contains("mermaid.esm.min.mjs"), notes)
    val base: String = html("assets/css/base.css")
    assert(base.contains("--pele-font: var(--base-font-family)"), base)
  }

  test("permalink prefix /short/child resolves under the aliased directory") {
    exists("short.html")
    val shortAlias: String = html("short.html")
    assert(shortAlias.contains("/aliased/index.html"), shortAlias)
    assert(shortAlias.toLowerCase.contains("refresh"), shortAlias)
    exists("aliased/index.html")
    exists("aliased/child.html")
    exists("aliased/255.2.html")
    val aliased: String = html("aliased/index.html")
    assert(aliased.contains("""href="/short/child.html""""), aliased)
    assert(aliased.contains("""href="/short/255.2.html""""), aliased)
    assert(!aliased.contains("unresolved-link"), aliased)
    assert(!aliased.contains("""href="/aliased/child.html""""), aliased)
    assert(!aliased.contains("""href="/aliased/255.2.html""""), aliased)
    val errors: String = html("errors.html")
    assert(!errors.contains("/short/child"), errors)
    assert(!errors.contains("/short/255.2"), errors)
  }

  test("Markdown glossary IAL yields term ids and hover tips") {
    val page: String = html("glossary.html")
    assert(page.contains("""class="glossary""""), page)
    assert(page.contains("""class="glossary-item""""), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("""class="glossary-tip""""), page)
    assert(page.contains("a verse"), page)
  }

  test("Markdown APA cites become #bibl- links; unknown keys are recorded") {
    val page: String = html("cite.html")
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-lamport94""""), page)
    assert(page.contains("Knuth") || page.toLowerCase.contains("knuth"), page)
    assert(page.contains("unresolved-citation"), page)
    assert(page.contains("missing-key"), page)
    val errors: String = html("errors.html")
    assert(errors.contains("unknown citation"), errors)
    assert(errors.contains("missing-key"), errors)
    assert(!File(targetDirectory, "library.bib").exists, "library.bib must not be published")
    assert(!File(targetDirectory, "_bibliography.bib").exists)
  }

  test("chunked Markdown writes a TOC chunk and per-section pages") {
    exists("chunked.html")
    val toc: String = html("chunked/index.html")
    assert(toc.contains("Preamble of the chunked document"), toc)
    assert(toc.contains("""src="/pixel.svg""""), toc)
    assert(toc.contains("Table of Contents"), toc)
    assert(!toc.contains("Alpha preamble, before subsections"), toc)
    assert(!toc.contains("""class="directory""""), toc)
    assert(!File(targetDirectory, "chunked/chunked.html").exists)
    val alpha: String = html("chunked/Alpha.html")
    assert(alpha.contains("Alpha preamble, before subsections"), alpha)
    assert(!alpha.contains("First leaf"), alpha)
    val one: String = html("chunked/Alpha-One.html")
    assert(one.contains("First leaf"), one)
    assert(!one.contains("Second leaf"), one)
    val two: String = html("chunked/Alpha-Two.html")
    assert(two.contains("Second leaf"), two)
    val beta: String = html("chunked/Beta.html")
    assert(beta.contains("Beta has no children"), beta)
  }

  test("header format icons: chunked HTML ↔ one-page HTML; PDF when present") {
    val full: String = html("chunked.html")
    assert(full.contains("""class="nav-item page-format""""), full)
    assert(full.contains("""href="/chunked/index.html""""), full)
    assert(full.contains("""title="Chunked HTML""""), full)
    assert(full.contains("fa-table-list"), full)
    assert(!full.contains("""href="/chunked.pdf""""), full)
    assert(!full.contains("""title="One-page HTML""""), full)

    val toc: String = html("chunked/index.html")
    assert(toc.contains("""href="/chunked.html""""), toc)
    assert(toc.contains("""title="One-page HTML""""), toc)
    assert(toc.contains("fa-file-lines"), toc)
    assert(!toc.contains("""title="Chunked HTML""""), toc)
    val alpha: String = html("chunked/Alpha.html")
    assert(alpha.contains("""href="/chunked.html""""), alpha)
    assert(alpha.contains("""title="One-page HTML""""), alpha)

    val glossary: String = html("glossary.html")
    assert(glossary.contains("""href="/glossary.pdf""""), glossary)
    assert(glossary.contains("""title="PDF""""), glossary)
    assert(glossary.contains("fa-file-pdf"), glossary)
    assert(!glossary.contains("""title="Chunked HTML""""), glossary)

    val notes: String = html("notes.html")
    assert(!notes.contains("""class="nav-item page-format""""), notes)

    val home: String = html("index.html")
    assert(!home.contains("""href="/glossary.pdf""""), home)
  }

  test("AsciiDoc book: checklist IR, table, footnote, citation, glossary") {
    val page: String = html("book.html")
    assert(page.contains("""class="task-list""""), page)
    assert(page.contains("open the chapter"), page)
    assert(page.contains("done the chapter"), page)
    assert(!page.contains("""class="checklist""""), page)
    assert(page.contains("<table"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("AsciiDoc footnote body"), page)
    assert(page.contains("Clinging AsciiDoc"), page)
    assert(page.contains("""</strong><span class="footnote-ref""""), page)
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("""id="knuth-book""""), page)
    assert(page.contains("""href="#knuth-book""""), page)
    assert(page.contains("Art of Computer Programming"), page)
    assert(page.contains("""class="citation-ref""""), page)
    assert(page.contains("""class="citation-tip""""), page)
    assert(page.contains("glossary-item"), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("verse"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("""class="callout""""), page)
    assert(page.contains("""class="callout-list""""), page)
    assert(page.contains("Library import"), page)
    assert(page.contains("""class="admonition""""), page)
    assert(page.contains("""data-type="note""""), page)
    assert(page.contains("Auxiliary information"), page)
    assert(page.contains("""class="aside""""), page)
    assert(page.contains("Auxiliary content"), page)
    assert(page.contains("""class="quote""""), page)
    assert(page.contains("""class="quote-title""""), page)
    assert(page.contains("A title"), page)
    assert(page.contains("A little rebellion"), page)
    assert(page.contains("""class="quote-attribution""""), page)
    assert(page.contains("Jefferson"), page)
    assert(page.contains("Papers"), page)
    assert(page.contains("<del"), page)
    assert(page.contains("obsolete phrase"), page)
    assert(!page.contains("line-through"), page)
    assert(page.contains("""class="figure""""), page)
    assert(page.contains("pixel.svg"), page)
    assert(page.contains("A figure caption"), page)
    assert(!page.contains("imageblock"), page)
    assert(page.contains("""class="pdf-embed""""), page)
    assert(page.contains("sample.pdf"), page)
    assert(page.contains("<video"), page)
    assert(page.contains("clip.mp4"), page)
    assert(!page.contains("videoblock"), page)
  }

  test("HTML about: table, task-list IR, glossary, header nav, citation IR") {
    val page: String = html("about.html")
    assert(page.contains("<table"), page)
    assert(page.contains("""class="task-list""""), page)
    assert(page.contains("html-open"), page)
    assert(page.contains("html-done"), page)
    assert(page.contains("""id="html-term""""), page)
    assert(page.contains("defined in HTML"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("""class="aside""""), page)
    assert(page.contains("already"), page)
    assert(page.contains("""class="quote""""), page)
    assert(page.contains("HTML quotation"), page)
    assert(page.contains("""class="quote-attribution""""), page)
    assert(page.contains("Work"), page)
    assert(page.contains("<del"), page)
    assert(page.contains("HTML struck"), page)
    assert(page.contains("HTML footnote body"), page)
    assert(page.contains("""</em><span class="footnote-ref""""), page)
    assert(page.contains("""class="figure""""), page)
    assert(page.contains("pixel.svg"), page)
    assert(page.contains("HTML figure"), page)
    assert(page.contains("""class="pdf-embed""""), page)
    assert(page.contains("sample.pdf"), page)
    assert(page.contains("<video"), page)
    assert(page.contains("clip.mp4"), page)
    val home: String = html("index.html")
    assert(home.contains("""href="/about.html""""), home)
  }

  test("TEI sample: endnote, table, glossary, code, and quote") {
    val page: String = html("tei-sample.html")
    assert(page.contains("""class="page-title""""), page)
    assert(page.contains("TEI Sample"), page)
    assert(page.contains("TEI endnote body"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("Clinging note"), page)
    assert(page.contains("""this<span class="footnote-ref""""), page)
    assert(page.contains("""</hi><span class="footnote-ref""""), page)
    assert(page.contains("<table"), page)
    assert(page.contains(">A<") || page.contains(">A</"), page)
    assert(page.contains("glossary-item"), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("verse"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("language-scala"), page)
    assert(page.contains("xs.map(f)"), page)
    assert(page.contains("""class="quote""""), page)
    assert(page.contains("A TEI quotation"), page)
    assert(page.contains("""class="quote-attribution""""), page)
    assert(page.contains("Jefferson"), page)
    assert(page.contains("Papers"), page)
    assert(page.contains("<del"), page)
    assert(page.contains("TEI struck"), page)
    assert(page.contains("""class="figure""""), page)
    assert(page.contains("pixel.svg"), page)
    assert(page.contains("A TEI figure"), page)
    assert(page.contains("""class="bibliography""""), page)
    assert(page.contains("""id="knuth79""""), page)
    assert(page.contains("""id="lamport94""""), page)
    assert(page.contains("""href="#knuth79""""), page)
    assert(page.contains("Knuth 1979"), page)
    assert(page.contains("""class="citation-ref""""), page)
    assert(page.contains("""class="citation-tip""""), page)
    assert(page.contains("lamport94"), page)
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("""class="csl-entry""""), page)
  }

  test("DocBook sample: title, section, paragraph") {
    val page: String = html("docbook-sample.html")
    assert(page.contains("DocBook Sample"), page)
    assert(page.contains("A DocBook paragraph"), page)
    assert(page.contains("Introduction"), page)
    assert(page.contains("""class="section""""), page)
    assert(page.contains("<p"), page)
    assert(!page.contains("<para"), page)
    assert(page.contains("DocBook footnote body"), page)
    assert(page.contains("""class="footnote-ref""""), page)
    assert(page.contains("Clinging note"), page)
    assert(page.contains("""this<span class="footnote-ref""""), page)
    assert(page.contains("""</em><span class="footnote-ref""""), page)
    assert(page.contains("<table"), page)
    assert(page.contains(">A<") || page.contains(">A</"), page)
    assert(page.contains("glossary-item"), page)
    assert(page.contains("""id="posuk""""), page)
    assert(page.contains("verse"), page)
    assert(page.contains("""class="glossary-ref""""), page)
    assert(page.contains("language-scala"), page)
    assert(page.contains("xs.map(f)"), page)
    assert(page.contains("""class="quote""""), page)
    assert(page.contains("A DocBook quotation"), page)
    assert(page.contains("""class="quote-attribution""""), page)
    assert(page.contains("Jefferson"), page)
    assert(page.contains("<del"), page)
    assert(page.contains("DocBook struck"), page)
    assert(page.contains("""class="figure""""), page)
    assert(page.contains("pixel.svg"), page)
    assert(page.contains("A DocBook figure"), page)
    assert(page.contains("""class="admonition""""), page)
    assert(page.contains("Auxiliary information"), page)
    assert(page.contains("""class="aside""""), page)
    assert(page.contains("From DocBook"), page)
    assert(page.contains("""class="callout""""), page)
    assert(page.contains("Library import"), page)
    assert(page.contains("<video"), page)
    assert(page.contains("clip.mp4"), page)
    assert(page.contains("""class="bibliography""""), page)
    assert(page.contains("""id="knuth79""""), page)
    assert(page.contains("""id="lamport94""""), page)
    assert(page.contains("""href="#knuth79""""), page)
    assert(page.contains("Knuth 1979"), page)
    assert(page.contains("""class="citation-ref""""), page)
    assert(page.contains("""class="citation-tip""""), page)
    assert(page.contains("lamport94"), page)
    assert(page.contains("""href="#bibl-knuth79""""), page)
    assert(page.contains("""id="bibl-knuth79""""), page)
    assert(page.contains("""class="csl-entry""""), page)
  }

  test("dated post is published and listed") {
    val post: String = html("2026/08/01/hello.html")
    assert(post.contains("Hello from a post"), post)
    assert(post.contains("""href="/notes.html""""), post)
    val posts: String = html("posts.html")
    assert(posts.contains("/2026/08/01/hello.html"), posts)
    assert(posts.contains("/2026/07/01/earlier.html"), posts)
    assert(!posts.contains("/2026/06/01/oldest.html"), posts)
    assert(posts.contains("""class="pagination""""), posts)
    assert(posts.contains("""class="post-excerpt""""), posts)
    assert(posts.contains("Greeting from the newest fixture post"), posts)
    val posts2: String = html("posts/2.html")
    assert(posts2.contains("/2026/06/01/oldest.html"), posts2)
    assert(!posts2.contains("/2026/08/01/hello.html"), posts2)
    assert(!posts2.contains("""class="post-excerpt""""), posts2)
    val sitemap: String = html("sitemap.xml")
    assert(sitemap.contains("/posts.html"), sitemap)
    assert(sitemap.contains("/posts/2.html"), sitemap)
    val tags: String = html("tags.html")
    assert(tags.contains("fixture"), tags)
  }

  test("Playwright PDF of glossary: page count, named dest, text") {
    val pdf: File = File(targetDirectory, "glossary.pdf")
    assert(pdf.isFile, s"missing glossary.pdf under $targetDirectory")
    val document = Loader.loadPDF(pdf)
    try
      val pages: Int = document.getNumberOfPages
      assert(pages >= 1, s"expected at least one page, got $pages")
      val text: String = PDFTextStripper().getText(document).replaceAll("\\s+", " ")
      assert(text.contains("a verse"), text)
    finally
      document.close()
    val dests: Map[String, Int] = PdfNamedDestinations.pageByName(pdf)
    assert(dests.get("posuk").exists(_ >= 1), dests)
  }

  test("sitemap, robots, and feed mention generated pages") {
    val declaration: String = """<?xml version="1.0" encoding="UTF-8"?>"""
    val sitemapRaw: String = htmlRaw("sitemap.xml")
    assert(sitemapRaw.startsWith(s"$declaration\n<urlset"), sitemapRaw)
    val sitemap: String = html("sitemap.xml")
    assert(sitemap.contains("http://fixture.test/index.html"), sitemap)
    assert(sitemap.contains("http://fixture.test/notes.html"), sitemap)
    val robots: String = html("robots.txt")
    assert(robots.contains("http://fixture.test/sitemap.xml"), robots)
    val feedRaw: String = htmlRaw("feed.xml")
    assert(feedRaw.startsWith(s"$declaration\n<feed"), feedRaw)
    val feed: String = html("feed.xml")
    assert(feed.contains("hello"), feed)
    assert(feed.contains("Greeting from the newest fixture post"), feed)
  }

  test("pages advertise the Atom feed") {
    val page: String = html("index.html")
    assert(page.contains("""rel="alternate""""), page)
    assert(page.contains("""type="application/atom+xml""""), page)
    assert(page.contains("""href="http://fixture.test/feed.xml""""), page)
    assert(page.contains("""title="Site Publisher Fixture""""), page)
    val feed: String = html("feed.xml")
    assert(feed.contains("Site Publisher"), feed)
    assert(feed.contains("https://github.com/dubinsky/site-publisher"), feed)
  }

  test("home page SEO: site title, description, canonical, WebSite") {
    val page: String = html("index.html")
    assert(page.contains("<title>Site Publisher Fixture</title>"), page)
    assert(page.contains("""name="generator""""), page)
    assert(page.contains("Site Publisher (https://github.com/dubinsky/site-publisher)"), page)
    assert(page.contains("""name="description""""), page)
    assert(page.contains("End-to-end fixture for site-publisher tests"), page)
    assert(page.contains("""rel="canonical""""), page)
    assert(page.contains("""href="http://fixture.test/index.html""""), page)
    assert(page.contains("""property="og:type""""), page)
    assert(page.contains("""content="website""""), page)
    assert(page.contains("""property="og:locale""""), page)
    assert(page.contains("""content="en_US""""), page)
    assert(page.contains("""name="twitter:card""""), page)
    assert(page.contains("""content="summary""""), page)
    assert(page.contains(""""@type":"WebSite""""), page)
    assert(!page.contains("itemtype="), page)
  }

  test("post SEO: titled with site, article type, BlogPosting") {
    val page: String = html("2026/08/01/hello.html")
    assert(page.contains("<title>hello | Site Publisher Fixture</title>"), page)
    assert(page.contains("""property="og:type""""), page)
    assert(page.contains("""content="article""""), page)
    assert(page.contains("""property="article:published_time""""), page)
    assert(page.contains("2026-08-01"), page)
    assert(page.contains(""""@type":"BlogPosting""""), page)
    assert(!page.contains("itemtype="), page)
    assert(page.contains("""href="http://fixture.test/2026/08/01/hello.html""""), page)
  }

  test("ordinary page SEO is WebPage") {
    val page: String = html("notes.html")
    assert(page.contains("<title>notes | Site Publisher Fixture</title>"), page)
    assert(page.contains(""""@type":"WebPage""""), page)
    assert(!page.contains("itemtype="), page)
  }

  test("root store gets collections tree and flat index") {
    exists("stores-collections.html")
    exists("stores-index.html")
    val tree: String = html("stores-collections.html")
    assert(tree.contains("""class="tree-index""""), tree)
    assert(tree.contains("<title>Tree |"), tree)
    assert(tree.contains("zebra"), tree)
    val flat: String = html("stores-index.html")
    assert(flat.contains("Collections"), flat)
  }

  test("store index lists includes in document order; extras error; hops are not pages") {
    val listing: String = html("stores/index.html")
    val zebraHref: Int = listing.indexOf("""href="/stores/note/zebra.html"""")
    val appleHref: Int = listing.indexOf("""href="/stores/note/apple.html"""")
    assert(zebraHref >= 0, listing)
    assert(appleHref >= 0, listing)
    assert(zebraHref < appleHref, listing)
    assert(listing.contains("note:"), listing)
    assert(listing.contains("store-header"), listing)
    assert(!listing.contains("<tei-head>"), listing)
    assert(listing.contains("store-name"), listing)
    assert(listing.contains("Stores Fixture"), listing)
    assert(!htmlOpen(listing).contains("""class="wide""""), htmlOpen(listing))
    val postContent: String = listing.substring(listing.indexOf("page-body"))
    assert(!postContent.contains("Stores Fixture"), postContent)
    assert(!postContent.contains("store-name"), postContent)
    assert(!postContent.contains("store-by"), postContent)
    assert(!postContent.contains("note:"), postContent)
    assert(!listing.contains("xi:include"), listing)
    assert(!listing.contains("apple body"), listing)
    assert(!listing.contains("zebra body"), listing)
    assert(!listing.contains("stray"), listing)
    assert(!listing.contains("/stores/note/ghost"), listing)
    assert(!File(targetDirectory, "stores/note/index.html").isFile)
    exists("stores/note/zebra.html")
    exists("stores/note/apple.html")
    assert(!File(targetDirectory, "stores/note/stray.html").isFile)
    val zebra: String = html("stores/note/zebra.html")
    assert(zebra.contains("store-header"), zebra)
    assert(!zebra.contains("""class="store-path""""), zebra)
    assert(!zebra.contains("""href="/stores/index.html""""), zebra)
    assert(!zebra.contains("Stores Fixture"), zebra)
    assert(zebra.contains("note zebra"), zebra)
    assert(zebra.contains("zebra body"), zebra)
    assert(zebra.contains("""href="/stores/note/apple.html""""), zebra)
    assert(!zebra.contains("""href="/stores/note/index.html""""), zebra)
    val apple: String = html("stores/note/apple.html")
    assert(apple.contains("apple body"), apple)
    assert(apple.contains("""href="/stores/note/zebra.html""""), apple)
    val errors: String = html("errors.html")
    assert(errors.contains("not in store"), errors)
    assert(errors.contains("stray"), errors)
    assert(errors.contains("unresolved"), errors)
    assert(errors.contains("ghost.md"), errors)
  }

  test("authored page links to the git file under the work tree") {
    val notes: String = html("notes.html")
    assert(notes.contains("https://github.com/dubinsky/site-publisher/blob/"), notes)
    assert(notes.contains("publisher/src/test/site/notes.md"), notes)
    assert(!notes.contains("x-access-token"), notes)
    assert(notes.contains("""class="page-meta""""), notes)
    assert(notes.contains("""class="source-link""""), notes)
    assert(notes.contains("""title="Source file on GitHub""""), notes)
    assert(!notes.contains("""class="document-links""""), notes)
    assert(!notes.contains("""class="citation-copy""""), notes)
  }

  test("collection document uses collector header with ancestor path and document selector") {
    val index: String = html("case29/index.html")
    assert(index.contains("store-header"), index)
    assert(index.contains("Case 29"), index)
    assert(index.contains("""class="collection-index""""), index)
    assert(!index.contains("""class="page-list""""), index)
    assert(!index.contains("<tei-head>"), index)
    assert(htmlOpen(index).contains("""class="wide""""), htmlOpen(index))
    val doc: String = html("case29/001.html")
    assert(doc.contains("store-header"), doc)
    assert(doc.contains("""class="store-path""""), doc)
    assert(doc.contains("""href="/case29/index.html""""), doc)
    assert(!doc.contains("Case 29"), doc)
    assert(doc.contains("document 001"), doc)
    assert(doc.contains("""class="store-current""""), doc)
    assert(doc.contains("""class="store-path-bar""""), doc)
    assert(doc.contains("""class="document-links""""), doc)
    assert(doc.contains("""class="source-link""""), doc)
    assert(!doc.contains("""class="citation-copy""""), doc)
    assert(!doc.contains("""class="page-meta""""), doc)
    assert(!doc.contains("<tei-head>document"), doc)
    assert(doc.contains("doc body"), doc)
    assert(!htmlOpen(doc).contains("""class="wide""""), htmlOpen(doc))
    val postContent: String = doc.substring(doc.indexOf("page-body"))
    assert(!postContent.contains("store-header"), postContent)
  }

  test("transclusion copies a section into a boxed embed") {
    val host: String = html("embed-host.html")
    assert(host.contains("""class="transclusion""""), host)
    assert(host.contains("""data-region="section""""), host)
    assert(host.contains("""data-source="/embed-target.html#Alpha""""), host)
    assert(host.contains("Embed Target § Alpha"), host)
    assert(host.contains("Alpha body"), host)
    assert(host.contains("transclusion-source"), host)
    assert(!host.contains("a.transclusion-link"), host)
    val css: String = html("assets/css/prose.css")
    assert(css.contains("html.transclusion-clean"), css)
    val notes: String = html("notes.html")
    assert(notes.contains("""data="/sample.pdf""""), notes)
    assert(!notes.contains("""class="transclusion""""), notes)
    val errors: String = html("errors.html")
    assert(errors.contains("unresolved transclusion"), errors)
    assert(errors.contains("NoSuch"), errors)
    val target: String = html("embed-target.html")
    assert(target.contains("Embedded in"), target)
    assert(target.contains("Embed Host"), target)
  }

  test("only [.add] shows Additions on a page that has no role") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "marked.adoc" -> "[.add]#added words#\n"
    )): target =>
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("added words"), home)
      assert(home.contains("Additions"), home)
      assert(!home.contains("Deletions"), home)
      assert(!home.contains("Sources"), home)
      assert(home.contains("""name="apparatus-add" value="distinct" checked="checked""""), home)
      assert(home.contains("[Marked]"), home)
      assert(!home.contains("""name="apparatus-del""""), home)
      assert(!home.contains("""name="apparatus-see""""), home)
      val open: String = htmlOpen(home)
      assert(open.contains("""data-apparatus-kinds="add""""), open)
      assert(!open.contains("""name="apparatus-add""""), open)
      assert(
        !open.split("\\s+").exists(token => token == "data-apparatus" || token.startsWith("data-apparatus=")),
        open
      )
      assert(home.contains("""name="apparatus-add""""), home)
  }

  test("supplied without [.add] has no apparatus group") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "doc.xml" -> choiceTei("<supplied>added words</supplied>")
    )): target =>
      val doc: String = readerHtml(target, "doc.html")
      assert(doc.contains("<supplied>"), doc)
      assert(!doc.contains("""name="apparatus-add""""), doc)
      assert(!doc.contains("Additions"), doc)
      assert(!htmlOpen(doc).contains("data-apparatus-kinds"), htmlOpen(doc))
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("""name="apparatus-add""""), home)
      assert(!htmlOpen(home).contains("data-apparatus-kinds"), htmlOpen(home))
  }

  test("a choice inside code does not show expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "doc.xml" -> choiceTei("<code><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></code>")
    )): target =>
      val doc: String = readerHtml(target, "doc.html")
      val codeAt: Int = doc.indexOf("<code>")
      val choiceAt: Int = doc.indexOf("<choice>")
      assert(codeAt >= 0 && choiceAt > codeAt, doc)
      assertNoExpansions(doc)
      assertNoExpansions(readerHtml(target, "index.html"))
  }

  test("a choice on one document shows expansions on another") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "one.xml" -> choiceTei("<choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice>"),
      "two.md" -> "Plain page.\n"
    )): target =>
      val one: String = readerHtml(target, "one.html")
      assert(one.contains("ChoiceMarker"), one)
      val two: String = readerHtml(target, "two.html")
      assert(!two.contains("ChoiceMarker"), two)
      assertExpansions(two)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
  }

  test("a choice in an absorbed years file shows expansions") {
    withReaderSite(rootStore(Map("archive-years.html" -> choiceHtml))): target =>
      assertAbsorbedChoice(target, "archive-years.html", """class="year-index"""")
  }

  test("a choice in an absorbed collections file shows expansions") {
    withReaderSite(rootStore(Map("archive-collections.html" -> choiceHtml))): target =>
      assertAbsorbedChoice(target, "archive-collections.html", """class="tree-index"""")
  }

  test("a choice in an absorbed flat index file shows expansions") {
    withReaderSite(rootStore(Map("archive-index.html" -> choiceHtml))): target =>
      assertAbsorbedChoice(target, "archive-index.html", """<h1 class="page-title">Collections</h1>""")
  }

  test("a choice in absorbed map prose shows expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "_terms.xml" ->
        """<?xml version="1.0" encoding="UTF-8"?>
          |<Terms>
          |  <term id="case" selector="yes">
          |    <name lang="en" n="case" plural="cases"/>
          |  </term>
          |</Terms>
          |""".stripMargin,
      "name.xml" ->
        """<entityLists>
          |  <title>Names</title>
          |  <listPlace n="places"><title>Places</title></listPlace>
          |</entityLists>
          |""".stripMargin,
      "name/Riga.xml" ->
        """<place>
          |  <placeName>Riga</placeName>
          |  <location><geo>56.94750 24.10694</geo></location>
          |</place>
          |""".stripMargin,
      "map.html" -> choiceHtml
    )): target =>
      assertAbsorbedChoice(target, "map.html", "<svg")
  }

  test("a choice in absorbed multi-list prose shows expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "name.xml" ->
        """<entityLists>
          |  <title>Names</title>
          |  <listPerson n="people"><title>People</title></listPerson>
          |  <listPlace n="places"><title>Places</title></listPlace>
          |</entityLists>
          |""".stripMargin,
      "name/Ann.xml" ->
        """<person>
          |  <persName>Ann</persName>
          |</person>
          |""".stripMargin,
      "name/Riga.xml" ->
        """<place>
          |  <placeName>Riga</placeName>
          |</place>
          |""".stripMargin,
      "name/places.html" -> choiceHtml
    )): target =>
      val page: String = readerHtml(target, "name/places.html")
      assert(page.contains("Riga"), page)
      assert(page.contains("ChoiceMarker"), page)
      assertExpansions(page)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
  }

  test("a choice in inline list prose shows expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "lists.xml" ->
        """<entityLists>
          |  <title>Catalog</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |</entityLists>
          |""".stripMargin,
      "lists/Riga.xml" ->
        """<place>
          |  <placeName>Riga</placeName>
          |</place>
          |""".stripMargin,
      "lists/cities.html" -> choiceHtml
    )): target =>
      assert(!File(target, "lists/cities.html").exists)
      val page: String = readerHtml(target, "lists/index.html")
      assert(page.contains("Riga"), page)
      assert(page.contains("ChoiceMarker"), page)
      assertExpansions(page)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
  }

  test("a choice in a collection abstract shows expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "col.xml" ->
        """<collection n="1" alias="col">
          |  <title>Case</title>
          |  <abstract><p><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></p></abstract>
          |</collection>
          |""".stripMargin,
      "col/000.xml" -> choiceTei("hello")
    )): target =>
      val index: String = readerHtml(target, "col/index.html")
      assert(index.contains("ChoiceMarker"), index)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
      assertExpansions(index)
  }

  test("a choice in a collection document abstract shows expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "col.xml" ->
        """<collection n="1" alias="col">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "col/000.xml" -> headerChoiceTei
    )): target =>
      val doc: String = readerHtml(target, "col/000.html")
      assert(doc.contains("hello"), doc)
      assert(doc.contains("ChoiceMarker"), doc)
      assertExpansions(doc)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
      assertExpansions(readerHtml(target, "col/index.html"))
  }

  test("a choice only in a collection encodingDesc does not show expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "col.xml" ->
        """<collection n="1" alias="col">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "col/000.xml" ->
        """<TEI>
          |  <teiHeader>
          |    <fileDesc><titleStmt><title>t</title></titleStmt></fileDesc>
          |    <encodingDesc>
          |      <p><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></p>
          |    </encodingDesc>
          |  </teiHeader>
          |  <text><body><p>hello</p></body></text>
          |</TEI>
          |""".stripMargin
    )): target =>
      val doc: String = readerHtml(target, "col/000.html")
      assert(doc.contains("hello"), doc)
      assert(!doc.contains("ChoiceMarker"), doc)
      assertNoExpansions(doc)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertNoExpansions(home)
      val index: String = readerHtml(target, "col/index.html")
      assert(!index.contains("ChoiceMarker"), index)
      assertNoExpansions(index)
  }

  test("a choice only in a note shows expansions on a page without the note") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "doc.xml" -> choiceTei(
        """See <note place="end"><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></note>"""
      )
    )): target =>
      val doc: String = readerHtml(target, "doc.html")
      assert(doc.contains("ChoiceMarker"), doc)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
  }

  test("an [.add] only in a note shows Additions on a page without the note") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "marked.adoc" -> "See footnote:[ [.add]#added words# ].\n"
    )): target =>
      val marked: String = readerHtml(target, "marked.html")
      assert(marked.contains("added words"), marked)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("added words"), home)
      assert(home.contains("Additions"), home)
      assert(home.contains("""name="apparatus-add" value="distinct" checked="checked""""), home)
      assert(htmlOpen(home).contains("""data-apparatus-kinds="add""""), htmlOpen(home))
  }

  test("a bibliography link only in a note shows the terms row on a page without the note") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "note.adoc" ->
        """See footnote:[<<knuth-book>>].

          |[bibliography]
          |* [[[knuth-book]]] Knuth, Donald E. The Art of Computer Programming. 1968.
          |""".stripMargin
    )): target =>
      val note: String = readerHtml(target, "note.html")
      assert(note.contains("knuth-book"), note)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("knuth-book"), home)
      assert(home.contains("""id="setting-glossary-expand""""), home)
  }

  test("a choice in a standalone TEI teiHeader shows expansions") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "doc.xml" -> headerChoiceTei
    )): target =>
      val doc: String = readerHtml(target, "doc.html")
      assert(doc.contains("<choice>"), doc)
      assert(doc.contains("ChoiceMarker"), doc)
      assertExpansions(doc)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("ChoiceMarker"), home)
      assertExpansions(home)
  }

  test("cite:[key] with no glossary and no bibliography link has no terms row") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "cite.adoc" ->
        """cite:[knuth79]

          |bibliography::[]
          |""".stripMargin
    )): target =>
      val cite: String = readerHtml(target, "cite.html")
      assert(cite.contains("knuth79"), cite)
      assert(!cite.contains("""id="setting-glossary-expand""""), cite)
      assert(!readerHtml(target, "index.html").contains("""id="setting-glossary-expand""""))
  }

  test("a glossary list with no refs still has the terms row") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "gloss.md" ->
        """posuk
          |: a verse
          |
          |{:.glossary}
          |""".stripMargin
    )): target =>
      val gloss: String = readerHtml(target, "gloss.html")
      assert(gloss.contains("posuk"), gloss)
      assert(!tagAt(gloss, """id="setting-glossary-expand"""").contains("checked"), gloss)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("posuk"), home)
      assert(home.contains("""id="setting-glossary-expand""""), home)
      assert(!tagAt(home, """id="setting-glossary-expand"""").contains("checked"), home)
      assertNoExpansions(home)
  }

  test("a translator, author, addressee, or transcriber cell sets choice and apparatus") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "col.xml" ->
        """<collection n="1" alias="col">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "col/000.xml" ->
        """<TEI>
          |  <teiHeader>
          |    <fileDesc><titleStmt>
          |      <title>t</title>
          |      <author><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></author>
          |      <editor role="transcriber"><span data-apparatus="del">gone</span></editor>
          |      <editor role="translator"><span data-apparatus="add">added</span></editor>
          |    </titleStmt></fileDesc>
          |    <profileDesc>
          |      <correspDesc><correspAction type="received">
          |        <persName><span data-apparatus="see">src</span></persName>
          |      </correspAction></correspDesc>
          |    </profileDesc>
          |  </teiHeader>
          |  <text><body><p>hello</p></body></text>
          |</TEI>
          |""".stripMargin
    )): target =>
      val doc: String = readerHtml(target, "col/000.html")
      assert(doc.contains("hello"), doc)
      val home: String = readerHtml(target, "index.html")
      assertExpansions(home)
      assert(home.contains("""name="apparatus-add""""), home)
      assert(home.contains("""name="apparatus-del""""), home)
      assert(home.contains("""name="apparatus-see""""), home)
  }

  test("a choice in a store title or body sets choice and apparatus") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "shelf.xml" ->
        """<store n="shelf">
          |  <title><choice><abbr>W</abbr><expan>ChoiceMarker</expan></choice></title>
          |  <body><p><span data-apparatus="add">added words</span></p></body>
          |</store>
          |""".stripMargin
    )): target =>
      val home: String = readerHtml(target, "index.html")
      assertExpansions(home)
      assert(home.contains("""name="apparatus-add""""), home)
      assert(!home.contains("""name="apparatus-del""""), home)
      assert(!home.contains("""name="apparatus-see""""), home)
  }

  test("a fragment that is both a gloss id and a bibliography id does not count as a citation") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "both.html" ->
        """<p><a href="#shared">shared</a></p>
          |<div class="glossary-item" id="shared"><dt>shared</dt><dd>gloss text</dd></div>
          |<div class="bibliography-item" id="shared"><p>Book text.</p></div>
          |""".stripMargin
    )): target =>
      val page: String = readerHtml(target, "both.html")
      assert(page.contains("gloss text"), page)
      assert(page.contains("Book text."), page)
      assert(page.contains("glossary-ref"), page)
      assert(!page.contains("citation-ref"), page)
      assert(page.contains("""id="setting-glossary-expand""""), page)
      val home: String = readerHtml(target, "index.html")
      assert(home.contains("""id="setting-glossary-expand""""), home)
      assertNoExpansions(home)
  }

  test("a transclusion that exists only in a footnote does not turn transclusions on") {
    withReaderSite(rootStore(Map(
      "archive-years.html" ->
        """<p>See<span class="footnote-link" footnote-correlation-id="only"></span>.</p>
          |<span class="footnote" footnote-correlation-id="only"><a class="wiki-link transclude" href="archive/rgada/900">![[archive/rgada/900]]</a></span>
          |""".stripMargin
    ))): target =>
      val years: String = readerHtml(target, "archive-years.html")
      assert(years.contains("""class="year-index""""), years)
      assert(years.contains("transclude"), years)
      assert(years.contains("archive/rgada/900"), years)
      assert(!years.contains("""id="setting-transclusion-clean""""), years)
      val home: String = readerHtml(target, "index.html")
      assert(!home.contains("""id="setting-transclusion-clean""""), home)
  }

  test("a relative link off inline-list prose resolves from the catalog") {
    withReaderSite(Map(
      "_site_config.yml" -> readerConfig,
      "index.md" -> "Home.\n",
      "lists.xml" ->
        """<entityLists>
          |  <title>Catalog</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |</entityLists>
          |""".stripMargin,
      "lists/Riga.xml" ->
        """<place>
          |  <placeName>Riga</placeName>
          |</place>
          |""".stripMargin,
      "lists/cities.html" ->
        """<p><a href="index.html#knuth">Knuth</a></p>
          |<div class="bibliography-item" id="knuth"><p>The Art of Computer Programming.</p></div>
          |""".stripMargin
    )): target =>
      assert(!File(target, "lists/cities.html").exists)
      val page: String = readerHtml(target, "lists/index.html")
      assert(page.contains("Riga"), page)
      assert(page.contains("The Art of Computer Programming."), page)
      assert(page.contains("citation-ref"), page)
      assert(!page.contains("glossary-ref"), page)
      assert(page.contains("""id="setting-glossary-expand""""), page)
      val home: String = readerHtml(target, "index.html")
      assert(home.contains("""id="setting-glossary-expand""""), home)
  }
