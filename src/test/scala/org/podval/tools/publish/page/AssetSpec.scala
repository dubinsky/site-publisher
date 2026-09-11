package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class AssetSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Asset Fixture
      |description: markup-as-asset tests
      |url: http://asset-page.test
      |author: Test
      |email: test@asset-page.test
      |""".stripMargin

  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-markup-asset")
    try
      val dir: File = path.toFile
      Files.write(File(dir, "_site_config.yml"), siteConfig)
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

  test("standalone asset: true copies HTML without site chrome") {
    val source: String =
      """<!DOCTYPE html>
        |<html><head><title>Raw</title></head>
        |<body><p>no chrome</p></body></html>
        |""".stripMargin
    withSite(Map(
      "index.md" -> "Home.\n",
      "raw.html" -> source,
      "raw.yml" -> "asset: true\n"
    )): (_, target) =>
      assert(Files.read(File(target, "raw.html")) == source)
      assert(!html(target, "raw.html").contains("post h-entry"))
      assert(!File(target, "raw.yml").exists)
      assert(!File(target, "raw.yaml").exists)
  }

  test("standalone asset: true copies Markdown with its source extension") {
    val source: String = "# Hello from markdown\n"
    withSite(Map(
      "index.md" -> "Home.\n",
      "note.md" -> source,
      "note.yml" -> "asset: true\n"
    )): (_, target) =>
      assert(File(target, "note.md").isFile)
      assert(Files.read(File(target, "note.md")) == source)
      assert(!File(target, "note.html").exists)
      assert(!File(target, "note.yml").exists)
  }

  test("standalone asset: true copies XML without dialect disambiguation") {
    val source: String = "<foo>not a known dialect</foo>\n"
    withSite(Map(
      "index.md" -> "Home.\n",
      "data.xml" -> source,
      "data.yml" -> "asset: true\n"
    )): (_, target) =>
      assert(Files.read(File(target, "data.xml")) == source)
      assert(!File(target, "data.html").exists)
  }

  test("internal asset: true is an error and the file stays markup") {
    withSite(Map(
      "index.md" -> "Home.\n",
      "page.md" ->
        """---
          |asset: true
          |---
          |Kept as markup.
          |""".stripMargin
    )): (_, target) =>
      val page: String = html(target, "page.html")
      assert(page.contains("post h-entry"), page)
      assert(page.contains("Kept as markup."), page)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("invalid asset"), errors)
      assert(errors.contains("standalone front matter"), errors)
  }

  test("directory index sidecar asset: true copies the file and keeps children") {
    val source: String =
      """<!DOCTYPE html>
        |<html><head><title>Home</title></head>
        |<body><p>landing</p></body></html>
        |""".stripMargin
    withSite(Map(
      "index.html" -> source,
      "index.yml" -> "asset: true\n",
      "child.md" -> "Child page.\n"
    )): (site, target) =>
      assert(Files.read(File(target, "index.html")) == source)
      assert(!html(target, "index.html").contains("post h-entry"))
      assert(!html(target, "index.html").contains("directory"))
      assert(!File(target, "index.yml").exists)
      val child: String = html(target, "child.html")
      assert(child.contains("Child page."), child)
      assert(child.contains("post h-entry"), child)
      val home: Page = site.pages.pages.find(_.path == Path("index").html).get
      assert(home.isDirectory)
      val errors: String = html(target, "errors.html")
      assert(!errors.contains("invalid asset"), errors)
  }

  test("nested directory index sidecar asset: true copies that index") {
    val source: String = "<!DOCTYPE html><html><body>nested</body></html>\n"
    withSite(Map(
      "index.md" -> "Home.\n",
      "dir/index.html" -> source,
      "dir/index.yml" -> "asset: true\n",
      "dir/leaf.md" -> "Leaf.\n"
    )): (site, target) =>
      assert(Files.read(File(target, "dir/index.html")) == source)
      assert(!html(target, "dir/index.html").contains("post h-entry"))
      val leaf: String = html(target, "dir/leaf.html")
      assert(leaf.contains("Leaf."), leaf)
      val directory: Page = site.pages.pages.find(_.path == Path("dir", "index").html).get
      assert(directory.isDirectory)
      val kid: Page = site.pages.pages.find(_.path.fileName == "leaf").get
      assert(kid.parent.contains(directory))
  }

  test("sidecar asset: true with internal front matter stays markup") {
    withSite(Map(
      "index.md" -> "Home.\n",
      "both.html" ->
        """---
          |title: Internal
          |---
          |<p>body</p>
          |""".stripMargin,
      "both.yml" -> "asset: true\n"
    )): (_, target) =>
      val page: String = html(target, "both.html")
      assert(page.contains("post h-entry"), page)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("ambiguous frontmatter"), errors)
  }

  test("sidecar without asset: true is still front matter") {
    withSite(Map(
      "index.md" -> "Home.\n",
      "titled.md" -> "Body.\n",
      "titled.yml" -> "title: Sidecar Title\n"
    )): (_, target) =>
      val page: String = html(target, "titled.html")
      assert(page.contains("Sidecar Title"), page)
      assert(page.contains("post h-entry"), page)
      assert(!File(target, "titled.yml").exists)
      assert(!File(target, "titled.md").exists)
  }
