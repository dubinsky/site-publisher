package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class GraphSpec extends AnyFunSuite:
  private def tei(body: String): String =
    s"""<TEI>
       |  <teiHeader><fileDesc><titleStmt><title>000</title></titleStmt></fileDesc></teiHeader>
       |  <text><body><p>$body</p></body></text>
       |</TEI>
       |""".stripMargin

  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-graph")
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

  private def config(
    extra: String = "graph:\n  enabled: true\n"
  ): String =
    s"""title: Graph Fixture
       |description: graph tests
       |url: http://graph.test
       |author: Test
       |email: test@graph.test
       |$extra""".stripMargin

  private def notes: Map[String, String] = Map(
    "index.md" -> "See [[a]].\n",
    "a.md" ->
      """---
        |title: A
        |---
        |![[b]]
        |""".stripMargin,
    "b.md" ->
      """---
        |title: B
        |---
        |Isolated.
        |""".stripMargin
  )

  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file)

  private def json(target: File): String =
    val file: File = File(target, "graph.json")
    assert(file.isFile, s"missing graph.json under $target")
    Files.read(file)

  test("emits graph.json and graph.html when enabled") {
    withSite(notes + ("_site_config.yml" -> config())): (_, target) =>
      val graph: String = json(target)
      assert(graph.contains(""""id":"/index.html""""), graph)
      assert(graph.contains(""""id":"/a.html""""), graph)
      assert(graph.contains(""""id":"/b.html""""), graph)
      assert(graph.contains(""""source":"/index.html","target":"/a.html","kind":"link""""), graph)
      assert(graph.contains(""""source":"/a.html","target":"/b.html","kind":"transclude""""), graph)
      assert(!graph.contains("/errors.html"), graph)
      val page: String = html(target, "graph.html")
      assert(page.contains("""id="site-graph""""), page)
      assert(page.contains("cdnjs.cloudflare.com/ajax/libs/cytoscape/3.34.2/cytoscape.esm.min.mjs"), page)
      assert(page.contains("""href="/assets/css/graph.css""""), page)
      assert(page.contains("/assets/js/graph.js"), page)
      assert(page.contains("\nimport { run } from \"/assets/js/graph.js\";"), page)
      assert(page.contains("\nrun(cytoscape);"), page)
      assert(!page.contains("&lt;"), page)
      assert(!page.contains("&amp;&amp;"), page)
      assert(!page.contains("cdnjs.cloudflare.com/ajax/libs/cytoscape/3.34.2/assets/css/graph.css"), page)
      assert(File(target, "assets/css/graph.css").isFile)
      assert(File(target, "assets/js/graph.js").isFile)
  }

  test("include-transclusions false omits transclude edges") {
    withSite(notes + ("_site_config.yml" -> config(
      """graph:
        |  enabled: true
        |  include-transclusions: false
        |""".stripMargin
    ))): (_, target) =>
      val graph: String = json(target)
      assert(graph.contains(""""source":"/index.html","target":"/a.html","kind":"link""""), graph)
      assert(!graph.contains(""""kind":"transclude""""), graph)
  }

  test("omitted graph writes neither file") {
    withSite(notes + ("_site_config.yml" -> config(extra = ""))): (_, target) =>
      assert(!File(target, "graph.json").exists)
      assert(!File(target, "graph.html").exists)
  }

  test("exclude-path-prefixes days matches source path after Posts.path remap") {
    val dailyFiles: Map[String, String] = Map(
      ".obsidian/daily-notes.json" -> """{"folder":"days"}""",
      "index.md" -> "Home.\n",
      "days/2020-10-23.md" -> "Daily.\n"
    )
    withSite(dailyFiles + ("_site_config.yml" -> config(
      """graph:
        |  enabled: true
        |  exclude-path-prefixes:
        |    - days
        |""".stripMargin
    ))): (_, target) =>
      val graph: String = json(target)
      assert(!graph.contains("/2020/10/23/index.html"), graph)
      assert(graph.contains(""""id":"/index.html""""), graph)
    withSite(dailyFiles + ("_site_config.yml" -> config())): (_, target) =>
      val graph: String = json(target)
      assert(graph.contains(""""id":"/2020/10/23/index.html""""), graph)
      assert(graph.contains(""""group":"days""""), graph)
  }

  test("exclude-path-prefixes archive matches source path not collection alias") {
    val archiveFiles: Map[String, String] = Map(
      "index.md" -> "Home.\n",
      "archive/col.xml" ->
        """<collection n="1" alias="col">
          |  <title>Case</title>
          |</collection>
          |""".stripMargin,
      "archive/col/000.xml" -> tei("Body.")
    )
    withSite(archiveFiles + ("_site_config.yml" -> config(
      """graph:
        |  enabled: true
        |  exclude-path-prefixes:
        |    - archive
        |""".stripMargin
    ))): (_, target) =>
      val graph: String = json(target)
      assert(!graph.contains("/col/000.html"), graph)
      assert(graph.contains(""""id":"/index.html""""), graph)
    withSite(archiveFiles + ("_site_config.yml" -> config())): (_, target) =>
      val graph: String = json(target)
      assert(graph.contains(""""id":"/col/000.html""""), graph)
      assert(graph.contains(""""kind":"tei""""), graph)
  }

  test("named-windows emits node target") {
    withSite(notes + ("_site_config.yml" -> config(
      """named-windows: true
        |graph:
        |  enabled: true
        |""".stripMargin
    ))): (_, target) =>
      val graph: String = json(target)
      assert(graph.contains(""""target":"hierarchyViewer""""), graph)
  }
