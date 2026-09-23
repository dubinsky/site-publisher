package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}
import java.time.LocalDate

final class PermalinkSpec extends AnyFunSuite:
  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-permalink")
    try
      val dir: File = path.toFile
      Files.write(
        File(dir, "_site_config.yml"),
        """title: Permalink Fixture
          |description: permalink and post tests
          |url: http://permalink.test
          |author: Test
          |email: test@permalink.test
          |""".stripMargin
      )
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

  private val files: Map[String, String] = Map(
    "index.md" -> "Home.\n",
    "about.md" ->
      """---
        |permalink: /about-page
        |---
        |About.
        |""".stripMargin,
    "notes/custom.md" ->
      """---
        |title: Custom Post
        |description: From a permalink
        |permalink: /2026/09/01/custom-name
        |---
        |Hello custom.
        |""".stripMargin,
    "notes/same-name.md" ->
      """---
        |title: Same Name
        |description: Auto post
        |post: true
        |date: 2026-05-02
        |---
        |Hello auto.
        |""".stripMargin,
    "notes/relative.md" ->
      """---
        |permalink: not-absolute
        |---
        |Relative.
        |""".stripMargin,
    "_posts/2026-08-01-hello.md" -> "Hello from a directory post.\n"
  )

  test("absolute permalink, auto-post, and _posts are posts; a relative permalink is an error") {
    withSite(files): (site, target) =>
      val custom = site.pages.pages.find(_.path == Path("notes", "custom").html).get
      assert(custom.isPost)
      assert(custom.date.map(_.localDate).contains(LocalDate.parse("2026-09-01")))
      val auto = site.pages.pages.find(_.path == Path("notes", "same-name").html).get
      assert(auto.isPost)
      assert(auto.date.map(_.localDate).contains(LocalDate.parse("2026-05-02")))
      val directory = site.pages.pages.find(_.path == Path("2026", "08", "01", "hello").html).get
      assert(directory.isPost)
      val about = site.pages.pages.find(_.path == Path("about").html).get
      assert(!about.isPost)
      val relative = site.pages.pages.find(_.path == Path("notes", "relative").html).get
      assert(!relative.isPost)
      assert(!site.pages.pages.exists(_.path == Path("notes", "not-absolute").html))
      assert(!File(target, "notes/not-absolute.html").isFile)

      val refresh: String = html(target, "2026/09/01/custom-name.html")
      assert(refresh.toLowerCase.contains("refresh"), refresh)
      assert(refresh.contains("/notes/custom.html"), refresh)
      val autoRefresh: String = html(target, "2026/05/02/same-name.html")
      assert(autoRefresh.contains("/notes/same-name.html"), autoRefresh)
      val aboutRefresh: String = html(target, "about-page.html")
      assert(aboutRefresh.contains("/about.html"), aboutRefresh)

      val posts: String = html(target, "posts.html")
      assert(posts.contains("/notes/custom.html"), posts)
      assert(posts.contains("Custom Post"), posts)
      assert(posts.contains("From a permalink"), posts)
      assert(posts.contains("/notes/same-name.html"), posts)
      assert(posts.contains("Same Name"), posts)
      assert(posts.contains("Auto post"), posts)
      assert(posts.contains("/2026/08/01/hello.html"), posts)
      assert(!posts.contains("/2026/09/01/custom-name.html"), posts)
      assert(!posts.contains("/2026/05/02/same-name.html"), posts)
      assert(!posts.contains("/about.html"), posts)
      assert(posts.split("/notes/same-name.html").length - 1 == 1, posts)

      val customHtml: String = html(target, "notes/custom.html")
      assert(customHtml.contains("BlogPosting"), customHtml)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("id=\"permalink\""), errors)
      assert(errors.contains("permalink must be absolute: not-absolute"), errors)
  }
