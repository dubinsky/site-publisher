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
        |""".stripMargin,
    abstractXml: String = "<abstract>A leaf</abstract>",
    authors: String = "",
    profileExtra: String = ""
  ): String =
    val fence: String =
      if frontMatter.isEmpty then ""
      else s"---\n$frontMatter---\n"
    val extra: String = if profileExtra.isEmpty then "" else s"\n      $profileExtra"
    s"""$fence<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt><title>Leaf</title>$authors</titleStmt></fileDesc>
       |    <profileDesc>
       |      $abstractXml
       |      <creation><date when="1798-08-11">11 августа 1798</date></creation>$extra
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
        |See [leaf](/col/000.html).[^he]
        |
        |[^he]: הערה
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
    "col/000.xml" -> tei(
      "he",
      body =
        """<pb n="000-1"/>
          |<pb n="000-2" missing="true"/>
          |<p>hebrew <foreign xml:lang="sv">svensk</foreign> <code lang="scala">map</code></p>
          |<p><date when="1798-08-11">11 августа 1798</date></p>
          |<note place="end">plain note</note>
          |<note place="end" xml:lang="he">עברית</note>
          |""".stripMargin
    ),
    "letter.xml" ->
      """---
        |chunk: true
        |---
        |<TEI>
        |  <teiHeader>
        |    <fileDesc><titleStmt><title>Letter</title></titleStmt></fileDesc>
        |  </teiHeader>
        |  <text xml:lang="he"><body>
        |    <div xml:lang="fr"><head>French</head><p>bonjour</p></div>
        |    <div><head>Bare</head><p>words</p></div>
        |  </body></text>
        |</TEI>
        |""".stripMargin,
    "col/equal.xml" -> tei(
      "ru",
      body =
        """<pb n="010-1"/>
          |<p><date when="1799-01-02">2 января 1799</date></p>
          |""".stripMargin
    ),
    "col/disagree.xml" -> tei("ru", frontMatter = "lang: en\n"),
    "col/agree.xml" -> tei("ru", frontMatter = "lang: ru\n", body = "<p>agreed</p>"),
    "col/attrs.xml" -> tei("ru", extraTextAttrs = """ lang="he"""", body = "<p>both</p>"),
    "col/who.xml" -> tei(
      "he",
      body = "<p>names</p>",
      authors = "<author><persName>יעקב</persName></author><author><persName>שמואל</persName></author>"
    ),
    "col/to.xml" -> tei(
      "he",
      body = "<p>to</p>",
      profileExtra = """<correspAction type="received"><persName ref="Нохим_Ицкович">נחום</persName></correspAction>"""
    ),
    "col/to-sv.xml" -> tei(
      "he",
      body = "<p>to sv</p>",
      profileExtra = """<correspAction type="received"><persName xml:lang="sv" ref="Sven_ref">Sven</persName></correspAction>"""
    ),
    "col/abs-sv.xml" -> tei(
      "he",
      body = "<p>abs</p>",
      abstractXml = """<abstract xml:lang="sv">Ett blad</abstract>"""
    ),
    "col/abs-p.xml" -> tei(
      "he",
      body = "<p>abs p</p>",
      abstractXml = """<abstract><p xml:lang="sv">Svensk mening</p></abstract>"""
    ),
    "col/who-mix.xml" -> tei(
      "he",
      body = "<p>mix</p>",
      authors = """<author xml:lang="sv"><persName>Sven</persName></author><author><persName>יעקב</persName></author>"""
    ),
    "col/who-text.xml" -> tei(
      "he",
      body = "<p>text names</p>",
      authors = """<author xml:lang="he">יעקב</author><author>Шнеур</author>"""
    ),
    "col/who-ru.xml" -> tei(
      "he",
      body = "<p>ru name</p>",
      authors = """<author><persName ref="alter-rebbe">Шнеур Залман</persName></author>"""
    ),
    "col/to-ru.xml" -> tei(
      "he",
      body = "<p>ru to</p>",
      profileExtra = """<correspAction type="received"><persName ref="Lopukhin">Лопухин</persName></correspAction>"""
    )
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

  private def tagBefore(page: String, text: String, from: Int): String =
    val at: Int = page.indexOf(text, from)
    assert(at >= 0, text)
    val start: Int = page.lastIndexOf("<span", at)
    val end: Int = page.indexOf(">", start)
    page.substring(start, end + 1)

  private def valueByHeading(page: String, heading: String): (String, String) =
    val at: Int = page.indexOf(s">$heading<")
    assert(at >= 0, s"missing heading $heading")
    val valueAt: Int = page.indexOf("""class="value"""", at)
    assert(valueAt > at, heading)
    val start: Int = page.lastIndexOf("<", valueAt)
    val tagEnd: Int = page.indexOf(">", valueAt)
    val tag: String = page.substring(start, tagEnd + 1)
    val nextHeading: Int = page.indexOf("""class="heading"""", tagEnd)
    val tableEnd: Int = page.indexOf("</table>", tagEnd)
    val end: Int = Seq(nextHeading, tableEnd).filter(_ >= 0).min
    (tag, page.substring(tagEnd + 1, end))

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
      refuteLang(openTag(doc, """class="page-body""""))
      assert(openTag(doc, "<text").contains("""lang="he""""), doc)
      val notes: String = openTag(doc, """class="footnotes"""")
      assert(notes.contains("""lang="ru""""), notes)
      assert(notes.contains("""dir="ltr""""), notes)
      val listAt: Int = doc.indexOf("""class="footnotes"""")
      val plain: String = tagBefore(doc, "plain note", listAt)
      assert(plain.contains("""class="footnote""""), plain)
      assert(!plain.contains("lang="), plain)
      assert(!plain.contains("dir="), plain)
      val marked: String = tagBefore(doc, "עברית", listAt)
      assert(marked.contains("""lang="he""""), marked)
      assert(marked.contains("""dir="rtl""""), marked)
      Seq("Описание", "Дата").foreach: heading =>
        val (tag, body) = valueByHeading(doc, heading)
        assert(heading != "Описание" || body.contains("A leaf"), body)
        assert(!tag.contains("""lang="he""""), tag)
        assert(!body.contains("""lang="he""""), s"$heading $body")
      openTags(doc, """class="heading"""").foreach: tag =>
        assert(!tag.contains("""lang="he""""), tag)
      val who: String = html(target, "col/who.html")
      val (whoTag, whoBody) = valueByHeading(who, "Кто")
      assert(whoTag.contains("""lang="he""""), whoTag)
      assert(whoBody.contains("יעקב</persName>, <persName>שמואל"), whoBody)
      assert(!whoBody.contains("<span"), whoBody)
      val to: String = html(target, "col/to.html")
      val (toTag, toBody) = valueByHeading(to, "Кому")
      assert(toTag.contains("""lang="he""""), toTag)
      assert(toBody.contains("<a"), toBody)
      assert(toBody.contains("Нохим_Ицкович"), toBody)
      assert(!toBody.contains("<span"), toBody)
      val toSv: String = html(target, "col/to-sv.html")
      val (toSvTag, toSvBody) = valueByHeading(toSv, "Кому")
      assert(!toSvTag.contains("""lang="he""""), toSvTag)
      val toSvName: String = openTag(toSvBody, "Sven")
      assert(toSvName.startsWith("<a"), toSvName)
      assert(toSvName.contains("""dir="ltr""""), toSvName)
      assert(!toSvBody.contains("<span"), toSvBody)
      val absSv: String = html(target, "col/abs-sv.html")
      val (absTag, _) = valueByHeading(absSv, "Описание")
      assert(absTag.contains("""lang="sv""""), absTag)
      assert(!absTag.contains("""lang="he""""), absTag)
      val absP: String = html(target, "col/abs-p.html")
      val (absPTag, absPBody) = valueByHeading(absP, "Описание")
      assert(!absPTag.contains("""lang="he""""), absPTag)
      val pTag: String = openTag(absPBody, "Svensk")
      assert(pTag.startsWith("<tei-p"), pTag)
      assert(pTag.contains("""lang="sv""""), pTag)
      val mix: String = html(target, "col/who-mix.html")
      val (mixTag, mixBody) = valueByHeading(mix, "Кто")
      assert(!mixTag.contains("""lang="he""""), mixTag)
      val svenAt: Int = mixBody.indexOf("Sven")
      assert(svenAt >= 0, mixBody)
      val spanAt: Int = mixBody.lastIndexOf("<span", svenAt)
      assert(spanAt >= 0, mixBody)
      val spanTag: String = mixBody.substring(spanAt, mixBody.indexOf(">", spanAt) + 1)
      assert(spanTag.contains("""lang="sv""""), spanTag)
      assert(spanTag.contains("""dir="ltr""""), spanTag)
      val spanClose: Int = mixBody.indexOf("</span>", svenAt)
      assert(!mixBody.substring(spanAt, spanClose).contains("יעקב"), mixBody)
      assert(mixBody.contains("יעקב"), mixBody)
      val textNames: String = html(target, "col/who-text.html")
      val (textTag, textBody) = valueByHeading(textNames, "Кто")
      assert(!textTag.contains("lang="), textTag)
      assert(textBody.contains("יעקב"), textBody)
      assert(textBody.contains("Шнеур"), textBody)
      val whoRu: String = html(target, "col/who-ru.html")
      val (whoRuTag, whoRuBody) = valueByHeading(whoRu, "Кто")
      assert(!whoRuTag.contains("lang="), whoRuTag)
      assert(whoRuBody.contains("Шнеур Залман"), whoRuBody)
      assert(whoRuBody.contains("alter-rebbe"), whoRuBody)
      val toRu: String = html(target, "col/to-ru.html")
      val (toRuTag, toRuBody) = valueByHeading(toRu, "Кому")
      assert(!toRuTag.contains("lang="), toRuTag)
      assert(toRuBody.contains("Лопухин"), toRuBody)
      assert(toRuBody.contains("href="), toRuBody)
      Seq(who, to, toSv, absSv, absP, mix, textNames, whoRu, toRu).foreach: page =>
        openTags(page, """class="heading"""").foreach: tag =>
          assert(!tag.contains("""lang="he""""), tag)
      refuteLang(openTag(doc, """class="store-current""""))
      assert(!doc.contains("""class="page-title""""), doc)
      val breaks: Seq[String] = openTags(doc, """class="pb"""")
      assert(breaks.exists(_.startsWith("<a")), breaks.mkString("\n"))
      assert(breaks.exists(_.startsWith("<span")), breaks.mkString("\n"))
      breaks.foreach(assertInterface)
      val runTag: String = openTag(doc, """class="pb-run"""")
      assert(runTag.startsWith("<div"), runTag)
      assertInterface(runTag)
      val runAt: Int = doc.indexOf(runTag)
      val runBody: String = doc.substring(runAt, doc.indexOf("</div>", runAt))
      assert(runBody.contains("""id="p000-1""""), runBody)
      assert(runBody.contains("""id="p000-2""""), runBody)
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
      refuteLang(openTag(note, """class="page-body""""))
      val noteBodyEnd: Int = note.indexOf(">", note.indexOf("""class="page-body""""))
      val contentStart: Int = note.indexOf("<", noteBodyEnd + 1)
      val contentRoot: String = note.substring(contentStart, note.indexOf(">", contentStart) + 1)
      assert(contentRoot.contains("""lang="he""""), contentRoot)
      val mdNotes: String = openTag(note, """class="footnotes"""")
      assert(!mdNotes.contains("""lang="ru""""), mdNotes)
      assert(!mdNotes.contains("dir="), mdNotes)

      val french: String = html(target, "letter/French.html")
      assert(openTag(french, "<html").contains("""<html lang="ru""""), french)
      val frenchSection: String = openTag(french, """class="section"""")
      assert(frenchSection.contains(""" lang="fr""""), frenchSection)
      assert(!frenchSection.contains(""" lang="he""""), frenchSection)
      val bare: String = html(target, "letter/Bare.html")
      val bareHtml: String = openTag(bare, "<html")
      assert(bareHtml.contains("""<html lang="ru""""), bareHtml)
      assert(!bareHtml.contains("""lang="he""""), bareHtml)
      val bareSection: String = openTag(bare, """class="section"""")
      assert(bareSection.contains(""" lang="he""""), bareSection)

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
      val equalRun: String = openTag(equal, """class="pb-run"""")
      assert(equalRun.startsWith("<div"), equalRun)
      refuteLang(equalRun)
      assert(!equalRun.contains("dir="), equalRun)
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
