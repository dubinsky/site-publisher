package org.podval.tools.publish.page

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class PageLanguageSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Language Fixture
      |description: Page language
      |url: http://language.test
      |author: Test
      |email: test@language.test
      |lang: ru
      |facsimiles-url: https://facsimiles.example/bucket/
      |""".stripMargin

  private def tei(
    lang: String,
    extraTextAttrs: String = "",
    frontMatter: String = "",
    body: String =
      """<pb n="000-1"/>
        |<pb n="000-2" missing="true"/>
        |<p>hebrew <foreign xml:lang="sv">svensk</foreign> <code lang="scala">map</code></p>
        |<p><date when="1798-08-11">11 августа 1798</date></p>
        |""".stripMargin
  ): String =
    val fence: String =
      if frontMatter.isEmpty then ""
      else s"---\n$frontMatter---\n"
    s"""$fence<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt><title>Leaf</title></titleStmt></fileDesc>
       |    <profileDesc>
       |      <abstract>A leaf</abstract>
       |      <creation><date when="1798-08-11">11 августа 1798</date></creation>
       |    </profileDesc>
       |  </teiHeader>
       |  <text xml:lang="$lang"$extraTextAttrs><body>
       |    $body
       |  </body></text>
       |</TEI>
       |""".stripMargin

  private val files: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "index.md" -> "Home.\n",
    "about.md" ->
      """---
        |title: A note
        |lang: he
        |---
        |See [leaf](/col/000.html).
        |""".stripMargin,
    "col.xml" ->
      """---
        |lang: he
        |---
        |<collection n="1">
        |  <name lang="en" n="Case"/>
        |  <title>Shelf title</title>
        |  <abstract><p>Shelf abstract</p></abstract>
        |  <body><p>Shelf body</p></body>
        |</collection>
        |""".stripMargin,
    "col/000.xml" -> tei("he"),
    "col/equal.xml" -> tei(
      "ru",
      body =
        """<pb n="010-1"/>
          |<p><date when="1799-01-02">2 января 1799</date></p>
          |""".stripMargin
    ),
    "col/disagree.xml" -> tei("ru", frontMatter = "lang: en\n"),
    "col/agree.xml" -> tei("ru", frontMatter = "lang: ru\n", body = "<p>agreed</p>"),
    "col/attrs.xml" -> tei("ru", extraTextAttrs = """ lang="he"""", body = "<p>both</p>")
  )

  private def withFiles(sources: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-language")
    try
      val dir: File = path.toFile
      sources.foreach: (relative, content) =>
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
    val tags: Seq[String] = openTags(page, marker)
    assert(tags.nonEmpty, s"missing $marker")
    tags.head

  private def openTags(page: String, marker: String): Seq[String] =
    Iterator.unfold(0): from =>
      val at: Int = page.indexOf(marker, from)
      if at < 0 then None
      else
        val start: Int = page.lastIndexOf("<", at)
        val end: Int = page.indexOf(">", at)
        Some((page.substring(start, end + 1), end + 1))
    .toSeq

  private def assertInterface(tag: String): Unit =
    assert(tag.contains("""lang="ru""""), tag)
    assert(tag.contains("""dir="ltr""""), tag)

  private def refuteLang(tag: String): Unit =
    assert(!tag.contains("lang="), tag)

  private def assertWrapped(page: String, text: String): Unit =
    val header: Int = page.indexOf("""class="store-header"""")
    val at: Int = page.indexOf(text, header)
    assert(at >= 0, s"missing $text")
    val window: String = page.substring(math.max(header, at - 80), at)
    assert(window.contains("""lang="he""""), window)

  test("document language, interface direction, and disagreeing signals") {
    withFiles(files): (_, target) =>
      val css: String = html(target, "assets/css/base.css")
      val ltrAt: Int = css.indexOf("""html[dir="ltr"]""")
      val ltrBlock: String = css.substring(ltrAt, css.indexOf("}", ltrAt))
      assert(ltrBlock.contains("direction: ltr;"), ltrBlock)
      assert(ltrBlock.contains("text-align: left;"), ltrBlock)
      val rtlAt: Int = css.indexOf("""html[dir="rtl"]""")
      val rtlBlock: String = css.substring(rtlAt, css.indexOf("}", rtlAt))
      assert(rtlBlock.contains("direction: rtl;"), rtlBlock)
      assert(rtlBlock.contains("text-align: right;"), rtlBlock)
      assert(css.contains("""[lang="sv"]"""), css)
      assert(css.contains("""[lang="lt"]"""), css)
      assert(css.contains("""[lang="la"]"""), css)

      val doc: String = html(target, "col/000.html")
      val root: String = openTag(doc, "<html")
      assert(root.contains("""lang="he""""), root)
      assert(root.contains("""dir="ltr""""), root)
      assert(root.contains("""data-interface-lang="ru""""), root)
      assertInterface(openTag(doc, """class="site-header""""))
      assertInterface(openTag(doc, """class="site-footer""""))
      assertInterface(openTag(doc, """class="store-header""""))
      assertInterface(openTag(doc, """class="facsimile-aside""""))
      assert(doc.contains("""class="backlinks""""), doc)
      assertInterface(openTag(doc, """class="backlinks""""))
      assert(openTag(doc, """class="page-body"""").contains("""lang="he""""), doc)
      openTags(doc, """class="value"""").foreach: tag =>
        assert(tag.contains("""lang="he""""), tag)
      openTags(doc, """class="heading"""").foreach: tag =>
        assert(!tag.contains("""lang="he""""), tag)
      refuteLang(openTag(doc, """class="store-current""""))
      assert(!doc.contains("""class="page-title""""), doc)
      val breaks: Seq[String] = openTags(doc, """class="pb"""")
      assert(breaks.exists(_.startsWith("<a")), breaks.mkString("\n"))
      assert(breaks.exists(_.startsWith("<span")), breaks.mkString("\n"))
      breaks.foreach(assertInterface)
      val tips: Seq[String] = openTags(doc, """class="date-tip"""")
      assert(tips.nonEmpty, doc)
      tips.foreach(assertInterface)
      openTags(doc, """class="date-ref"""").foreach: tag =>
        val openEnd: Int = doc.indexOf(tag) + tag.length
        val nextAt: Int = doc.indexOf("<", openEnd)
        val nextEnd: Int = doc.indexOf(">", nextAt)
        val visible: String = doc.substring(nextAt, nextEnd + 1)
        assert(!visible.contains("""lang="ru""""), visible)
      val foreign: String = openTag(doc, """xml:lang="sv"""")
      assert(foreign.contains("""lang="sv""""), foreign)
      val code: String = openTag(doc, """tei-lang="scala"""")
      assert(!code.contains(""" lang=""""), code)

      val note: String = html(target, "about.html")
      assertInterface(openTag(note, """class="page-header""""))
      assert(openTag(note, """class="page-title"""").contains("""lang="he""""), note)
      assert(!note.contains("""class="store-header""""), note)

      val store: String = html(target, "col/index.html")
      assert(openTag(store, "<html").contains("""lang="he""""), store)
      refuteLang(openTag(store, """class="store-current""""))
      assert(openTag(store, """class="store-name"""").contains("""lang="en""""), store)
      assertWrapped(store, "Shelf title")
      assertWrapped(store, "Shelf body")
      assertWrapped(store, "Shelf abstract")

      val equal: String = html(target, "col/equal.html")
      assert(openTag(equal, "<html").contains("""lang="ru""""), equal)
      refuteLang(openTag(equal, """class="store-header""""))
      openTags(equal, """class="pb"""").foreach(refuteLang)
      openTags(equal, """class="date-tip"""").foreach(refuteLang)
      assert(openTags(equal, """class="date-tip"""").nonEmpty, equal)

      val conflict: String = html(target, "col/disagree.html")
      val conflictRoot: String = openTag(conflict, "<html")
      assert(conflictRoot.contains("""lang="ru""""), conflictRoot)
      assert(!conflictRoot.contains("""lang="en""""), conflictRoot)
      val both: String = html(target, "col/attrs.html")
      val bothRoot: String = openTag(both, "<html")
      assert(bothRoot.contains("""lang="ru""""), bothRoot)
      assert(!bothRoot.contains("""lang="he""""), bothRoot)
      val agreed: String = html(target, "col/agree.html")
      assert(openTag(agreed, "<html").contains("""lang="ru""""), agreed)

      val stamped: String = html(target, "col/attrs.xml")
      assert(stamped.contains("""ident="ru""""), stamped)
      assert(!stamped.contains("""ident="he""""), stamped)

      val errors: String = html(target, "errors.html")
      assert(errors.contains("Wrong language in disagree"), errors)
      assert(errors.contains("Wrong language in attrs"), errors)
      assert(!errors.contains("Wrong language in agree"), errors)
      assert(!errors.contains("Wrong language in 000"), errors)
      assert(!errors.contains("Wrong language in equal"), errors)
  }

  test("site lang Russian with no document language is ru and does not stamp chrome") {
    withFiles(Map(
      "_site_config.yml" -> siteConfig.replace("lang: ru", "lang: Russian"),
      "plain.md" ->
        """---
          |title: Plain
          |---
          |Words.
          |""".stripMargin
    )): (_, target) =>
      val page: String = html(target, "plain.html")
      val root: String = openTag(page, "<html")
      assert(root.contains("""lang="ru""""), root)
      assert(root.contains("""dir="ltr""""), root)
      assert(root.contains("""data-interface-lang="ru""""), root)
      refuteLang(openTag(page, """class="page-header""""))
      refuteLang(openTag(page, """class="page-title""""))
      refuteLang(openTag(page, """class="site-header""""))
      refuteLang(openTag(page, """class="site-footer""""))
  }
