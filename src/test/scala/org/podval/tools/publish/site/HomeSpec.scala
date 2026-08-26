package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class HomeSpec extends AnyFunSuite:
  private def config(home: String): String =
    s"""title: Home Fixture
       |description: home config tests
       |url: http://home.test
       |author: Test
       |email: test@home.test
       |home: $home
       |""".stripMargin

  private def withSite(
    home: String,
    files: Map[String, String]
  )(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-home")
    try
      val dir: File = path.toFile
      Files.write(File(dir, "_site_config.yml"), config(home))
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
    Files.read(File(target, relative)).replaceAll("\\s+", " ").replace("= ", "=")

  test("home to a chunked TOC: /index.html refreshes there, no directory listing") {
    withSite(
      home = "/doc/index.html",
      files = Map(
        "doc.md" ->
          """---
            |title: Doc
            |chunk: true
            |chunk-depth: 2
            |---
            |Preamble.
            |
            |## Alpha
            |
            |Leaf.
            |""".stripMargin
      )
    ): (_, target) =>
      val index: String = html(target, "index.html")
      assert(index.toLowerCase.contains("refresh"), index)
      assert(index.contains("/doc/index.html"), index)
      assert(!index.contains("""class="directory""""), index)
      assert(File(target, "doc.html").isFile)
      val toc: String = html(target, "doc/index.html")
      assert(toc.contains("Preamble"), toc)
      assert(toc.contains("Table of Contents"), toc)
  }

  test("home to an unchunked page refreshes to that page") {
    withSite(
      home = "/about.html",
      files = Map(
        "about.md" ->
          """---
            |title: About
            |---
            |Hello.
            |""".stripMargin
      )
    ): (_, target) =>
      val index: String = html(target, "index.html")
      assert(index.toLowerCase.contains("refresh"), index)
      assert(index.contains("/about.html"), index)
      assert(!index.contains("""class="directory""""), index)
  }

  test("home cannot coexist with an authored index") {
    withSite(
      home = "/about.html",
      files = Map(
        "index.md" ->
          """---
            |title: Home
            |---
            |Listing.
            |""".stripMargin,
        "about.md" ->
          """---
            |title: About
            |---
            |Hello.
            |""".stripMargin
      )
    ): (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("cannot coexist with an authored index"), errors)
      val index: String = html(target, "index.html")
      assert(index.contains("Listing"), index)
      assert(!index.toLowerCase.contains("refresh"), index)
  }

  test("missing home target is recorded") {
    withSite(
      home = "/nope.html",
      files = Map(
        "about.md" ->
          """---
            |title: About
            |---
            |Hello.
            |""".stripMargin
      )
    ): (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.contains("home page not found"), errors)
      assert(errors.contains("/nope.html") || errors.contains("nope"), errors)
  }
