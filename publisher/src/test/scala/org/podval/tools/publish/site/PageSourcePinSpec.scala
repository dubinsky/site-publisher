package org.podval.tools.publish.site

import org.podval.tools.publish.page.{MarkupPage, PageContent, PageSource}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class PageSourcePinSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Pin Fixture
      |description: pinned page content
      |url: http://pin.test
      |author: Test
      |email: test@pin.test
      |lang: en
      |""".stripMargin

  private val indexFiles: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "index.md" -> "Home.\n"
  )

  private val inlineFiles: Map[String, String] = indexFiles ++ Map(
    "lists.xml" ->
      """<entityLists>
        |  <title>Catalog</title>
        |  <listPlace n="cities-he"><title>Cities</title></listPlace>
        |</entityLists>
        |""".stripMargin,
    "lists/rome.xml" ->
      """<place>
        |  <placeName>Rome</placeName>
        |</place>
        |""".stripMargin,
    "lists/cities-he.md" ->
      """---
        |title: Front Title
        |lang: en
        |---
        |# Content Title
        |
        |Beside the list.
        |""".stripMargin
  )

  private def withSite(files: Map[String, String], generate: Boolean)(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-pin")
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
      if generate then site.generate() else site.pages.load()
      body(site, target)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  test("pin keeps an already cached content") {
    withSite(indexFiles, generate = false): (site, _) =>
      val source: PageSource = site.pages.pages.collectFirst:
        case page: MarkupPage if page.sourcePath.exists(_.fileName == "index") => page.source
      .flatten.get
      val cached: PageContent = source.content
      source.pin()
      assert(cached eq source.content)
  }

  test("a second quiet parse of inline-list prose does not add another ambiguous title or file name") {
    withSite(inlineFiles, generate = true): (site, target) =>
      val catalog: MarkupPage = site.pages.pages.collectFirst:
        case page: MarkupPage if page.inlineListProse.isDefined => page
      .get
      assert(!catalog.inlineListProse.get.recordParseErrors)
      assert(!File(target, "lists/cities-he.html").exists)
      val page: String = html(target, "lists/index.html")
      assert(page.contains("Beside the list."), page)
      assert(page.contains("Rome"), page)
      val errors: String = html(target, "errors.html")
      def kindItems(id: String): Seq[String] =
        val start: Int = errors.indexOf(s"""id="$id"""")
        assert(start >= 0, errors)
        val ul: Int = errors.indexOf("<ul", start)
        val end: Int = errors.indexOf("</ul>", ul)
        errors.substring(ul, end).split("<li>").drop(1).toSeq
      val titles: Seq[String] = kindItems("ambiguous-title")
      val names: Seq[String] = kindItems("file-name")
      assert(titles.length == 1, errors)
      assert(names.length == 1, errors)
      assert(titles.head.contains("lists/cities-he.md"), titles.head)
      assert(titles.head.contains("Front Title"), titles.head)
      assert(titles.head.contains("Content Title"), titles.head)
      assert(names.head.contains("lists/cities-he.md"), names.head)
      assert(names.head.contains("Wrong language"), names.head)
  }
