package org.podval.tools.publish.page

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class AlternatesSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Alternates Fixture
      |description: Suffix and directory readings
      |url: http://language.test
      |author: Test
      |email: test@language.test
      |lang: ru
      |""".stripMargin

  private def tei(textAttrs: String): String =
    s"""<TEI>
       |  <teiHeader>
       |    <fileDesc><titleStmt><title>Leaf</title></titleStmt></fileDesc>
       |  </teiHeader>
       |  <text$textAttrs><body><p>leaf</p></body></text>
       |</TEI>
       |""".stripMargin

  private def withFiles(sources: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-alternates")
    try
      val dir: File = path.toFile
      (Map("_site_config.yml" -> siteConfig, "index.md" -> "Home.\n") ++ sources).foreach: (relative, content) =>
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
    val at: Int = page.indexOf(marker)
    assert(at >= 0, s"missing $marker")
    val start: Int = page.lastIndexOf("<", at)
    val end: Int = page.indexOf(">", at)
    page.substring(start, end + 1)

  private def openTags(page: String, marker: String): Seq[String] =
    Iterator.unfold(0): from =>
      val at: Int = page.indexOf(marker, from)
      if at < 0 then None
      else
        val start: Int = page.lastIndexOf("<", at)
        val end: Int = page.indexOf(">", at)
        Some((page.substring(start, end + 1), end + 1))
    .toSeq

  test("suffix outside a collection links the autonym both ways and emits hreflang") {
    withFiles(Map(
      "about.md" -> "A note.\n",
      "about-en.md" -> "A note in English.\n",
      "plain.md" -> "No other reading.\n"
    )): (_, target) =>
      val original: String = html(target, "about.html")
      val link: String = openTag(original, """class="nav-item translation"""")
      assert(link.contains("""href="/about-en.html""""), link)
      assert(link.contains("""lang="en""""), link)
      assert(link.contains("""hreflang="en""""), link)
      assert(original.contains(">English<"), original)
      assert(!original.contains("[en]"), original)
      assert(openTag(original, "<html").contains("""lang="ru""""), original)
      assert(original.contains("""rel="canonical" href="http://language.test/about.html""""), original)
      assert(original.contains("""hreflang="en" href="http://language.test/about-en.html""""), original)
      assert(original.contains("""hreflang="ru" href="http://language.test/about.html""""), original)
      assert(original.contains("""hreflang="x-default" href="http://language.test/about.html""""), original)
      assert(!original.contains("Accept-Language"), original)

      val translation: String = html(target, "about-en.html")
      val back: String = openTag(translation, """class="nav-item translation"""")
      assert(back.contains("""href="/about.html""""), back)
      assert(back.contains("""lang="ru""""), back)
      assert(back.contains("""hreflang="ru""""), back)
      assert(translation.contains(">русский<"), translation)
      assert(openTag(translation, "<html").contains("""lang="en""""), translation)
      assert(translation.contains("""rel="canonical" href="http://language.test/about-en.html""""), translation)
      assert(translation.contains("""hreflang="x-default" href="http://language.test/about.html""""), translation)
      assert(translation.contains("""hreflang="en" href="http://language.test/about-en.html""""), translation)

      val alone: String = html(target, "plain.html")
      assert(!alone.contains("hreflang"), alone)
      assert(alone.contains("""rel="canonical""""), alone)
      val errors: String = html(target, "errors.html")
      assert(!errors.contains("Wrong language in about"), errors)
  }

  test("directory and suffix signals, including a lone directory and a missing TEI xml:lang") {
    withFiles(Map(
      "en/about.md" -> "Directory only.\n",
      "en/noted.md" ->
        """---
          |lang: he
          |---
          |Disagrees.
          |""".stripMargin,
      "en/leaf.xml" -> tei(""),
      "ru/about-en.md" -> "Suffix and directory disagree.\n",
      "ru/about-ru.md" -> "Suffix and directory agree.\n",
      "about-ru.md" ->
        """---
          |lang: ru
          |---
          |Front matter agrees with the suffix.
          |""".stripMargin,
      "doc-ru.xml" -> tei("")
    )): (_, target) =>
      val directory: String = html(target, "en/about.html")
      assert(openTag(directory, "<html").contains("""lang="en""""), directory)
      assert(!directory.contains("""class="nav-item translation""""), directory)
      assert(!directory.contains("hreflang"), directory)

      val noted: String = html(target, "en/noted.html")
      assert(openTag(noted, "<html").contains("""lang="en""""), noted)

      val crossed: String = html(target, "ru/about-en.html")
      assert(openTag(crossed, "<html").contains("""lang="en""""), crossed)

      val agreedDir: String = html(target, "ru/about-ru.html")
      assert(openTag(agreedDir, "<html").contains("""lang="ru""""), agreedDir)

      val errors: String = html(target, "errors.html")
      assert(!errors.contains("/en/about.md"), errors)
      assert(errors.contains("Wrong language in noted: he !=en"), errors)
      assert(errors.contains("Wrong language in leaf: missing !=en"), errors)
      assert(errors.contains("Wrong language in about-en: en !=ru"), errors)
      assert(!errors.contains("Wrong language in about-ru"), errors)
      assert(!errors.contains("/ru/about-ru.md"), errors)
      assert(errors.contains("Wrong language in doc-ru: missing !=ru"), errors)
  }

  test("duplicate en is an error and one hreflang") {
    withFiles(Map(
      "about.md" -> "Original.\n",
      "about-en.md" -> "Suffix.\n",
      "en/about.md" -> "Directory.\n"
    )): (_, target) =>
      val original: String = html(target, "about.html")
      val nav: Seq[String] = openTags(original, """class="nav-item translation"""")
      assert(nav.length == 1, nav.mkString("\n"))
      assert(nav.head.contains("""href="/about-en.html""""), nav.head)
      assert(!original.contains("""href="/en/about.html""""), original)
      val alternateEn: Seq[String] = openTags(original, """hreflang="en"""").filter(_.contains("""rel="alternate""""))
      assert(alternateEn.length == 1, alternateEn.mkString("\n"))
      assert(alternateEn.head.contains("""href="http://language.test/about-en.html""""), alternateEn.head)
      assert(original.contains("""hreflang="x-default" href="http://language.test/about.html""""), original)

      val dropped: String = html(target, "en/about.html")
      assert(!dropped.contains("hreflang"), dropped)
      assert(!dropped.contains("""class="nav-item translation""""), dropped)

      val errors: String = html(target, "errors.html")
      assert(errors.contains("Duplicate language en"), errors)
      assert(errors.contains("/about-en.md"), errors)
      assert(errors.contains("/en/about.md"), errors)
  }

  test("nested directory readings cluster only from the site root") {
    withFiles(Map(
      "notes/about.md" -> "A note.\n",
      "en/notes/about.md" -> "The English note.\n",
      "col.xml" ->
        """<collection n="1">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "col/001.xml" -> tei(""" xml:lang="he""""),
      "col/001-ru.xml" -> tei(""" xml:lang="ru""""),
      "en/col/001.xml" -> tei(""" xml:lang="en""""),
      "col/en/001.xml" -> tei(""" xml:lang="en""""),
      "only-ru.md" -> "No original.\n",
      "about.md" -> "Root note.\n",
      "English/about.md" -> "Not a catalog code.\n",
      "zz/about.md" -> "Not a catalog code.\n"
    )): (_, target) =>
      val note: String = html(target, "notes/about.html")
      val noteLink: String = openTag(note, """class="nav-item translation"""")
      assert(noteLink.contains("""href="/en/notes/about.html""""), noteLink)
      assert(note.contains("""hreflang="en""""), note)
      val noteEn: String = html(target, "en/notes/about.html")
      val noteBack: String = openTag(noteEn, """class="nav-item translation"""")
      assert(noteBack.contains("""href="/notes/about.html""""), noteBack)
      assert(noteEn.contains("""hreflang="ru""""), noteEn)

      val index: String = html(target, "col/index.html")
      assert(index.contains("""class="language">he <a href="/en/col/001.html">en</a> <a href="/col/001-ru.html">ru</a>"""), index)
      assert(!index.contains("""href="/col/en/001.html""""), index)
      val document: String = html(target, "col/001.html")
      assert(document.contains("""class="nav-item translation""""), document)
      assert(document.contains("""href="/en/col/001.html""""), document)
      assert(document.contains("""href="/col/001-ru.html""""), document)
      assert(!document.contains("""href="/col/en/001.html""""), document)
      val buried: String = html(target, "col/en/001.html")
      assert(!buried.contains("""class="nav-item translation""""), buried)
      assert(!buried.contains("hreflang"), buried)
      assert(!buried.contains("""href="/col/001.html""""), buried)

      val lone: String = html(target, "only-ru.html")
      assert(!lone.contains("""class="nav-item translation""""), lone)
      assert(!lone.contains("hreflang"), lone)

      val root: String = html(target, "about.html")
      assert(!root.contains("""class="nav-item translation""""), root)
      assert(!root.contains("hreflang"), root)
      assert(!root.contains("""href="/English/about.html""""), root)
      assert(!root.contains("""href="/zz/about.html""""), root)
      val english: String = html(target, "English/about.html")
      assert(!english.contains("""class="nav-item translation""""), english)
      assert(!english.contains("hreflang"), english)
      val unknown: String = html(target, "zz/about.html")
      assert(!unknown.contains("""class="nav-item translation""""), unknown)
      assert(!unknown.contains("hreflang"), unknown)
  }
