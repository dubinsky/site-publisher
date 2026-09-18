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
