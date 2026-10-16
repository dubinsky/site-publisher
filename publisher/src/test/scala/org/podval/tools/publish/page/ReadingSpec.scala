package org.podval.tools.publish.page

import org.podval.tools.publish.site.{CollectionAliases, Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class ReadingSpec extends AnyFunSuite:
  test("splitLang does not treat +reg as a language") {
    assert(CollectionIndex.splitLang("doc+reg") == ("doc+reg", None))
    assert(CollectionIndex.splitLang("doc-ru") == ("doc", Some("ru")))
    assert(CollectionIndex.splitLang("doc-ru+reg") == ("doc-ru+reg", None))
    assert(CollectionIndex.splitLang(CollectionIndex.withoutReading("doc+reg")) == ("doc", None))
    assert(CollectionIndex.splitLang(CollectionIndex.withoutReading("doc-ru+reg")) == ("doc", Some("ru")))
  }

  private def tei(
    body: String,
    lang: Option[String] = Some("ru"),
    bareLang: Option[String] = None,
    pb: Boolean = false
  ): String =
    val xmlLang: String = lang.fold("")(code => s""" xml:lang="$code"""")
    val langAttr: String = bareLang.fold("")(code => s""" lang="$code"""")
    val break: String = if pb then """<pb n="000-1"/>""" else ""
    s"""<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt><title>t</title></titleStmt></fileDesc>
       |  </teiHeader>
       |  <text$xmlLang$langAttr><body>$break<p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: Reading Fixture
        |description: regularized reading
        |url: http://reading.test
        |author: Test
        |email: test@reading.test
        |lang: ru
        |facsimiles-url: https://facsimiles.test/
        |search:
        |  enabled: true
        |""".stripMargin,
    "index.md" -> "Home.\n",
    "col.xml" ->
      """<collection n="1" alias="col">
        |  <title>Case</title>
        |</collection>
        |""".stripMargin,
    "col/a.xml" -> tei("before"),
    "col/doc.xml" -> tei("diplomaticword", lang = Some("he"), pb = true),
    "col/doc+reg.xml" -> tei("regularizedword", lang = Some("he"), pb = true),
    "col/doc-ru.xml" -> tei("rudiplomatic", lang = Some("ru"), pb = true),
    "col/doc-ru+reg.xml" -> tei("ruregularized", lang = Some("ru"), pb = true),
    "col/z.xml" -> tei("after"),
    "same.xml" -> tei("sameoriginal", lang = Some("ru")),
    "same+reg.xml" -> tei("samereading", lang = Some("ru")),
    "name.xml" -> tei("nameoriginal", lang = Some("Russian")),
    "name+reg.xml" -> tei("namereading", lang = Some("ru")),
    "plain.xml" -> tei("plainoriginal", lang = None),
    "plain+reg.xml" -> tei("plainreading", lang = None),
    "bad.xml" -> tei("badoriginal", lang = Some("ru")),
    "bad+reg.xml" -> tei("badreading", lang = Some("he")),
    "gone+reg.xml" -> tei("gone reading", lang = Some("ru")),
    "clash.xml" -> tei("clashoriginal", lang = Some("ru")),
    "clash+reg.xml" -> tei("clashreading", lang = Some("he"), bareLang = Some("ru")),
    "inc.xml" ->
      """<collection xmlns:xi="http://www.w3.org/2001/XInclude" n="2">
        |  <title>Included</title>
        |  <xi:include href="inc/kept.xml"/>
        |</collection>
        |""".stripMargin,
    "inc/kept.xml" -> tei("keptoriginal"),
    "inc/kept+reg.xml" -> tei("keptreading"),
    "notes/a.md" -> "before note\n",
    "notes/m.md" -> "noteoriginal\n",
    "notes/m+reg.md" -> "noteregularized\n",
    "notes/z.md" -> "after note\n",
    "archive.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="archive">
        |    <xi:include href="archive/case.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "archive/case.xml" ->
      """<collection n="3">
        |  <title>Year case</title>
        |</collection>
        |""".stripMargin,
    "archive/case/leaf.xml" -> tei("yeardiplomatic"),
    "archive/case/leaf+reg.xml" -> tei("yearregularized")
  )

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-reading")
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

  private def raw(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file)

  private def bySource(site: Site, source: String): MarkupPage =
    val found: Option[Page] = site.pages.pages.find(_.sourcePath.exists(_.toString == source))
    assert(found.isDefined, s"missing page $source")
    found.get.asInstanceOf[MarkupPage]

  private def rewriteJs: File =
    val candidates: Seq[File] = Seq(
      File("src/main/resources/org/podval/tools/publish/site/cloudflare/rewrite.js"),
      File("publisher/src/main/resources/org/podval/tools/publish/site/cloudflare/rewrite.js")
    )
    candidates.find(_.isFile).getOrElse(
      throw IllegalStateException(s"rewrite.js not found from ${new File(".").getAbsolutePath}")
    )

  private def workerRewrite(aliases: File, pathname: String): String =
    val script: String =
      """import { readFileSync } from "node:fs";
        |import { rewritePath } from "./rewrite.js";
        |const aliases = JSON.parse(readFileSync(process.env.ALIASES, "utf8"));
        |const out = rewritePath(process.env.PATHNAME, aliases);
        |process.stdout.write(out == null ? "" : String(out));
        |""".stripMargin
    val builder: ProcessBuilder = new ProcessBuilder("node", "--input-type=module", "-e", script)
      .directory(rewriteJs.getParentFile)
      .redirectErrorStream(true)
    val env: java.util.Map[String, String] = builder.environment()
    env.put("ALIASES", aliases.getAbsolutePath)
    env.put("PATHNAME", pathname)
    val process: Process = builder.start()
    val output: String = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val code: Int = process.waitFor()
    assert(code == 0, output)
    output

  test("a +reg file is a regularized reading of the sibling, not a language") {
    withSite: (site, target) =>
      val index: String = html(target, "col/index.html")
      assert(index.contains("""href="/col/doc.html""""), index)
      assert(!index.contains("+reg"), index)

      val doc: MarkupPage = bySource(site, "/col/doc.xml")
      val reading: MarkupPage = bySource(site, "/col/doc+reg.xml")
      val translation: MarkupPage = bySource(site, "/col/doc-ru.xml")
      val translationReading: MarkupPage = bySource(site, "/col/doc-ru+reg.xml")
      val before: MarkupPage = bySource(site, "/col/a.xml")
      val after: MarkupPage = bySource(site, "/col/z.xml")
      assert(doc.prev.contains(before), doc.prev)
      assert(doc.next.contains(after), doc.next)
      assert(reading.prev.contains(before), reading.prev)
      assert(reading.next.contains(after), reading.next)
      assert(translation.prev.contains(before), translation.prev)
      assert(translation.next.contains(after), translation.next)
      assert(translationReading.prev.contains(before), translationReading.prev)
      assert(translationReading.next.contains(after), translationReading.next)
      assert(before.next.contains(doc), before.next)
      assert(!before.next.contains(reading), before.next)
      assert(after.prev.contains(doc), after.prev)

      val note: MarkupPage = bySource(site, "/notes/m.md")
      val noteReading: MarkupPage = bySource(site, "/notes/m+reg.md")
      val noteBefore: MarkupPage = bySource(site, "/notes/a.md")
      val noteAfter: MarkupPage = bySource(site, "/notes/z.md")
      assert(note.prev.contains(noteBefore), note.prev)
      assert(note.next.contains(noteAfter), note.next)
      assert(noteReading.prev.contains(noteBefore), noteReading.prev)
      assert(noteReading.next.contains(noteAfter), noteReading.next)
      assert(noteBefore.next.contains(note), noteBefore.next)
      val listing: String = html(target, "notes/index.html")
      assert(listing.contains("""href="/notes/m+reg.html""""), listing)

      val originalHtml: String = html(target, "col/doc.html")
      val readingHtml: String = html(target, "col/doc+reg.html")
      val translationHtml: String = html(target, "col/doc-ru.html")
      val translationReadingHtml: String = html(target, "col/doc-ru+reg.html")
      val regAt: Int = originalHtml.indexOf("""class="nav-item reading"""")
      val langAt: Int = originalHtml.indexOf("""class="nav-item translation"""")
      assert(langAt >= 0 && regAt > langAt, originalHtml)
      val regLink: String = originalHtml.substring(regAt, regAt + 160)
      assert(regLink.contains("""href="/col/doc+reg.html""""), regLink)
      assert(regLink.contains("[reg]"), regLink)
      assert(!regLink.contains("hreflang"), regLink)
      assert(!regLink.contains("lang="), regLink)
      assert(!readingHtml.contains("""class="nav-item reading""""), readingHtml)
      assert(!readingHtml.contains("""class="nav-item translation""""), readingHtml)
      assert(!readingHtml.contains("[reg]"), readingHtml)
      val translationReg: Int = translationHtml.indexOf("""class="nav-item reading"""")
      assert(translationReg > translationHtml.indexOf("""class="nav-item translation""""), translationHtml)
      assert(translationHtml.substring(translationReg, translationReg + 180).contains("/col/doc-ru+reg.html"), translationHtml)
      assert(!translationReadingHtml.contains("""class="nav-item reading""""), translationReadingHtml)
      assert(!translationReadingHtml.contains("""class="nav-item translation""""), translationReadingHtml)

      val head: String = originalHtml.substring(0, originalHtml.indexOf("</head>"))
      assert(head.contains("hreflang"), head)
      assert(!head.contains("+reg"), head)
      assert(!readingHtml.substring(0, readingHtml.indexOf("</head>")).contains("hreflang"), readingHtml)
      assert(!translationReadingHtml.contains("hreflang"), translationReadingHtml)

      assert(File(target, "col/doc/facsimile.html").isFile)
      assert(!File(target, "col/doc+reg/facsimile.html").isFile)
      assert(!File(target, "col/doc-ru/facsimile.html").isFile)
      assert(!File(target, "col/doc-ru+reg/facsimile.html").isFile)
      assert(originalHtml.contains("""href="/col/doc/facsimile.html""""), originalHtml)
      assert(readingHtml.contains("""href="/col/doc/facsimile.html""""), readingHtml)
      assert(translationReadingHtml.contains("""href="/col/doc/facsimile.html""""), translationReadingHtml)
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(
        CollectionAliases.rewrite(Path.fromHref("/col/doc+reg/facsimile"), table).exists(_.toString == "/col/doc/facsimile.html")
      )
      assert(
        CollectionAliases.rewrite(Path.fromHref("/col/doc-ru+reg/facsimile"), table).exists(_.toString == "/col/doc/facsimile.html")
      )
      assert(
        site.pages.rewriteRequest(Path.fromHref("/col/doc+reg/facsimile")).exists(_.toString == "/col/doc/facsimile.html")
      )
      val aliases: File = File(target, CollectionAliases.fileName)
      assert(workerRewrite(aliases, "/col/doc+reg/facsimile") == "/col/doc/facsimile.html")
      assert(workerRewrite(aliases, "/col/facsimile/doc+reg") == "/col/doc/facsimile.html")
      assert(workerRewrite(aliases, "/col/doc-ru+reg/facsimile") == "/col/doc/facsimile.html")

      val errors: String = raw(target, "errors.html")
      assert(errors.contains("Wrong language in bad+reg.xml: he != ru"), errors)
      assert(errors.contains("no original for gone+reg.xml"), errors)
      assert(errors.contains("Wrong language in clash+reg:"), errors)
      assert(!errors.contains("Wrong language in clash+reg.xml"), errors)
      assert(!errors.contains("Wrong language in doc+reg"), errors)
      assert(!errors.contains("Wrong language in same+reg"), errors)
      assert(!errors.contains("Wrong language in name+reg"), errors)
      assert(!errors.contains("Wrong language in plain+reg"), errors)
      assert(!errors.contains("no original for doc+reg"), errors)
      assert(!errors.contains("no original for plain+reg"), errors)
      assert(!errors.contains("kept+reg"), errors)

      assert(File(target, "inc/kept+reg.html").isFile)
      val included: String = html(target, "inc/index.html")
      assert(included.contains("""href="/inc/kept.html""""), included)
      assert(!included.contains("+reg"), included)

      val years: String = html(target, "archive-years.html")
      assert(years.contains("""href="/archive/case/leaf.html""""), years)
      assert(!years.contains("leaf+reg"), years)

      val json: String = raw(target, "search.json")
      assert(json.contains("diplomaticword"), json)
      assert(json.contains("rudiplomatic"), json)
      assert(json.contains("noteoriginal"), json)
      assert(!json.contains("regularizedword"), json)
      assert(!json.contains("ruregularized"), json)
      assert(!json.contains("noteregularized"), json)
      assert(!json.contains("yearregularized"), json)
      assert(!json.contains("/col/doc+reg.html"), json)
  }
