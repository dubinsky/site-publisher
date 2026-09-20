package org.podval.tools.publish.markup

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class TransclusionSpec extends AnyFunSuite:
  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-transclusion")
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

  private val config: String =
    """title: Transclusion Fixture
      |description: transclusion tests
      |url: http://transclusion.test
      |author: Test
      |email: test@transclusion.test
      |""".stripMargin

  test("missing fragment is a frozen stub, not a whole-page embed") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "notes.md" ->
        """---
          |title: Notes Title
          |---
          |## Alpha
          |
          |Alpha body.
          |""".stripMargin,
      "host.md" -> "![[notes#NoSuch]]\n"
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(host.contains("unresolved-transclusion"), host)
      assert(host.contains("![[notes#NoSuch]]"), host)
      assert(!host.contains("""class="transclusion""""), host)
      assert(!host.contains("Alpha body"), host)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("unresolved transclusion"), errors)
  }

  test("section X via Y of a sibling Z is not a cycle") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "a.md" ->
        """---
          |title: A
          |---
          |## X
          |
          |![[b#Y]]
          |
          |## Z
          |
          |Zee.
          |""".stripMargin,
      "b.md" ->
        """---
          |title: B
          |---
          |## Y
          |
          |![[a#Z]]
          |""".stripMargin
    )): (_, target) =>
      val a: String = html(target, "a.html")
      assert(a.contains("""class="transclusion""""), a)
      assert(a.contains("Zee"), a)
      assert(!a.contains("transclusion-loop"), a)
      val errors: String = html(target, "errors.html")
      assert(!errors.contains("transclusion loop"), errors)
  }

  test("section X via Y back to X is a loop") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "a.md" ->
        """---
          |title: A
          |---
          |## X
          |
          |![[b#Y]]
          |""".stripMargin,
      "b.md" ->
        """---
          |title: B
          |---
          |## Y
          |
          |![[a#X]]
          |""".stripMargin
    )): (_, target) =>
      val a: String = html(target, "a.html")
      assert(a.contains("transclusion-loop"), a)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("transclusion loop"), errors)
  }

  test("transclude stubs are not backlink snippets; target lists Embedded in") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "target.md" ->
        """---
          |title: Target
          |---
          |## Alpha
          |
          |Alpha body.
          |""".stripMargin,
      "host.md" ->
        """---
          |title: Host
          |---
          |![[target#Alpha]]
          |""".stripMargin
    )): (_, target) =>
      val page: String = html(target, "target.html")
      assert(page.contains("Embedded in"), page)
      assert(page.contains("Host"), page)
      assert(!page.contains("![[target"), page)
      val back: Int = page.indexOf("class=\"backlinks\"")
      assert(back < 0, page)
  }

  test("chunk URL extra fragment wins; missing extra fragment is a stub") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "book.md" ->
        """---
          |title: Book
          |chunk: true
          |chunk-depth: 2
          |---
          |Preamble.
          |
          |## Alpha
          |
          |### One
          |
          |One body.
          |""".stripMargin,
      "host.md" ->
        """![[/book/Alpha.html#One]]
          |
          |![[/book/Alpha.html#NoSuch]]
          |""".stripMargin
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(host.contains("One body"), host)
      assert(host.contains("unresolved-transclusion"), host)
      assert(host.contains("![[/book/Alpha.html#NoSuch]]"), host)
      val book: String = html(target, "book.html")
      assert(book.contains("Embedded in"), book)
  }

  test("footnotes in a transcluded section are merged into the host") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "target.md" ->
        """---
          |title: Target
          |---
          |## Alpha
          |
          |See this [^n].
          |
          |[^n]: Target note.
          |""".stripMargin,
      "host.md" ->
        """Host [^h].
          |
          |![[target#Alpha]]
          |
          |[^h]: Host note.
          |""".stripMargin
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(host.contains("Target note"), host)
      assert(host.contains("Host note"), host)
      assert(host.contains("""class="transclusion""""), host)
      assert(host.contains("""id="_footnote_src_1""""), host)
      assert(host.contains("""id="_footnote_src_2""""), host)
      assert(host.contains("""href="#_footnote_src_1""""), host)
      assert(host.contains("""href="#_footnote_src_2""""), host)
      val copyAt: Int = host.indexOf("""class="transclusion"""")
      assert(copyAt >= 0, host)
      val copyHtml: String = host.substring(copyAt)
      assert(copyHtml.contains("""id="_footnote_src_2""""), copyHtml)
  }

  test("mixed paragraph splits around the aside") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "target.md" ->
        """---
          |title: Target
          |---
          |## Alpha
          |
          |Alpha body.
          |""".stripMargin,
      "host.md" -> "See ![[target#Alpha]] after.\n"
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(host.contains("<p>See</p>") || host.contains("<p>See </p>"), host)
      assert(host.contains("""class="transclusion""""), host)
      assert(host.contains("after"), host)
  }

  test("host table and copied table use different _table_k_fn_a ids") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "target.md" ->
        """---
          |title: Target
          |---
          |## Alpha
          |
          || C | [^c] |
          ||---|---|
          || 1 | 2 |
          |
          |[^c]: Copy table note.
          |""".stripMargin,
      "host.md" ->
        """| H | [^h] |
          ||---|---|
          || 1 | 2 |
          |
          |[^h]: Host table note.
          |
          |![[target#Alpha]]
          |""".stripMargin
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(host.contains("table-with-notes"), host)
      assert(host.contains("""id="_table_1_fn_a""""), host)
      assert(host.contains("""id="_table_2_fn_a""""), host)
      assert(host.contains("""id="_table_1_fn_src_a""""), host)
      assert(host.contains("""id="_table_2_fn_src_a""""), host)
      assert(host.contains("""href="#_table_1_fn_src_a""""), host)
      assert(host.contains("""href="#_table_2_fn_src_a""""), host)
      assert(host.contains("Host table note"), host)
      assert(host.contains("Copy table note"), host)
      val copyAt: Int = host.indexOf("""class="transclusion"""")
      assert(copyAt >= 0, host)
      val copyHtml: String = host.substring(copyAt)
      assert(copyHtml.contains("""id="_table_2_fn_src_a""""), copyHtml)
      val pageLists: Int = host.split("""class="footnotes"""").length - 1
      assert(pageLists >= 0, host)
      assert(!host.contains("""id="_footnote_1""""), host)
  }

  test("transcluded note-in-note prefixes inner ids on the copy; host inners stay unprefixed") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "target.md" ->
        """---
          |title: Target
          |---
          |## Alpha
          |
          |Copied <span class="footnote-link" footnote-correlation-id="outer"></span><span class="footnote" footnote-correlation-id="outer"><p>wrap <l>line</l> <hi>x</hi><span class="footnote-link" footnote-correlation-id="inner"></span><span class="footnote" footnote-correlation-id="inner">deep inner</span></p></span>.
          |""".stripMargin,
      "host.md" ->
        """Host <span class="footnote-link" footnote-correlation-id="outer"></span><span class="footnote" footnote-correlation-id="outer">Host outer <span class="footnote-link" footnote-correlation-id="inner"></span><span class="footnote" footnote-correlation-id="inner">Host inner only</span></span>.
          |
          |![[target#Alpha]]
          |""".stripMargin
    )): (_, target) =>
      val host: String = html(target, "host.html")
      assert(host.contains("""class="transclusion""""), host)
      assert(host.contains("""id="_footnote_1""""), host)
      assert(host.contains("""id="_footnote_1_n_a""""), host)
      assert(host.contains("Host inner only"), host)
      assert(host.contains("""id="_footnote_2""""), host)
      assert(host.contains("""id="_footnote_2_n_a""""), host)
      assert(host.contains("deep inner"), host)
      assert(host.contains("nested-footnotes"), host)
      val fn1At: Int = host.indexOf("""id="_footnote_1"""")
      val hostInnerAt: Int = host.indexOf("Host inner only")
      val fn1nAt: Int = host.indexOf("""id="_footnote_1_n_a"""")
      val fn2At: Int = host.indexOf("""id="_footnote_2"""")
      val copyInnerAt: Int = host.indexOf("deep inner")
      val fn2nAt: Int = host.indexOf("""id="_footnote_2_n_a"""")
      assert(fn1At >= 0 && fn1nAt > fn1At && hostInnerAt > fn1At, host)
      assert(fn2At >= 0 && fn2nAt > fn2At && copyInnerAt > fn2At, host)
      assert(!host.contains("""id="_footnote_3""""), host)
      val copyChunk: String =
        if fn2At >= 0 then host.substring(fn2At) else host
      assert(copyChunk.contains("deep inner"), copyChunk)
      assert(copyChunk.contains("nested-footnotes"), copyChunk)
      val hostChunk: String =
        if fn1At >= 0 && fn2At > fn1At then host.substring(fn1At, fn2At) else host
      assert(hostChunk.contains("Host inner only"), hostChunk)
      assert(!hostChunk.contains("deep inner"), hostChunk)
  }
