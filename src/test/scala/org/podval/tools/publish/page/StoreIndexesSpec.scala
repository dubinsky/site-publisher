package org.podval.tools.publish.page

import org.podval.tools.publish.site.{CollectionAliases, Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class StoreIndexesSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Archive Fixture
      |description: root store indexes
      |url: http://archive.test
      |author: Test
      |email: test@archive.test
      |header-pages:
      |  - names.md
      |  - archive-collections
      |  - about.md
      |home: /archive-index.html
      |""".stripMargin

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-store-indexes")
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
    Files.read(File(target, relative)).replaceAll("\\s+", " ").replace("= ", "=")

  private val files: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "names.md" ->
      """---
        |title: Имена
        |---
        |Names.
        |""".stripMargin,
    "about.md" ->
      """---
        |title: About
        |---
        |See [дела](/collections).
        |""".stripMargin,
    "archive.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="archive">
        |    <xi:include href="archive/books.xml"/>
        |    <xi:include href="archive/rgada.xml"/>
        |    <xi:include href="archive/bare.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "archive/books.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <name lang="ru" n="книги"/>
        |  <name lang="en" n="books"/>
        |  <title/>
        |  <by selector="book">
        |    <xi:include href="books/book/derzhavin.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "archive/books/book/derzhavin.xml" ->
      """<collection n="Державин">
        |  <title>Переписка</title>
        |  <abstract><p>From the notes.</p></abstract>
        |</collection>
        |""".stripMargin,
    "archive/rgada.xml" ->
      """<collection n="3140" alias="rgada">
        |  <name lang="ru" n="РГАДА"/>
        |  <title>О секте</title>
        |  <abstract><p>Донос</p></abstract>
        |</collection>
        |""".stripMargin,
    "archive/bare.xml" ->
      """<collection n="bare">
        |  <title>Bare</title>
        |</collection>
        |""".stripMargin
  )

  test("writes archive-collections tree and archive-index flat list") {
    withSite: (_, target) =>
      assert(File(target, "archive-collections.html").isFile)
      assert(File(target, "archive-index.html").isFile)
      val tree: String = html(target, "archive-collections.html")
      assert(tree.contains("<title>Архивы | Archive Fixture</title>"), tree)
      assert(tree.contains("""class="tree-index""""), tree)
      assert(tree.contains("<em>архив</em>"), tree)
      assert(tree.contains("книги:"), tree)
      assert(tree.contains("<em>книга</em>"), tree)
      assert(tree.contains("Державин: Переписка"), tree)
      assert(tree.contains("РГАДА: О секте"), tree)
      val booksHref: Int = tree.indexOf("""href="/archive/books/index.html"""")
      val derzhavinHref: Int = tree.indexOf("Державин: Переписка")
      val rgadaHref: Int = tree.indexOf("РГАДА: О секте")
      assert(booksHref >= 0, tree)
      assert(derzhavinHref > booksHref, tree)
      assert(rgadaHref > derzhavinHref, tree)
      val flat: String = html(target, "archive-index.html")
      assert(flat.contains("<title>Дела | Archive Fixture</title>"), flat)
      assert(flat.contains("архив книги, книга Державин: Переписка"), flat)
      assert(flat.contains("архив РГАДА: О секте"), flat)
      assert(flat.contains("From the notes."), flat)
      assert(flat.contains("Донос"), flat)
      val derzhavinItem: Int = flat.indexOf("книга Державин")
      val rgadaItem: Int = flat.indexOf("архив РГАДА")
      assert(derzhavinItem >= 0 && rgadaItem > derzhavinItem, flat)
      val body: String = flat.substring(flat.indexOf("post-content"), flat.indexOf("</article>"))
      val items: Int = "<li>".r.findAllMatchIn(body).size
      val abstracts: Int = "<abstract".r.findAllMatchIn(body).size
      assert(abstracts == items, s"abstracts=$abstracts items=$items $body")
      assert(body.contains("<abstract></abstract>"), body)
  }

  test("home refreshes to archive-index; header lists Архивы after Имена") {
    withSite: (_, target) =>
      val index: String = html(target, "index.html")
      assert(index.toLowerCase.contains("refresh"), index)
      assert(index.contains("/archive-index.html"), index)
      val tree: String = html(target, "archive-collections.html")
      val nav: String = tree.substring(tree.indexOf("nav-items"))
      val namesHref: Int = nav.indexOf("""href="/names.html"""")
      val collectionsHref: Int = nav.indexOf("""href="/archive-collections.html"""")
      val aboutHref: Int = nav.indexOf("""href="/about.html"""")
      assert(namesHref >= 0 && collectionsHref > namesHref && aboutHref > collectionsHref, nav)
      assert(tree.contains("""class="post-title"""), tree)
      assert(tree.contains("Архивы"), tree)
  }

  test("rewrites /collections to the tree page; no Refresh file") {
    withSite: (site, target) =>
      assert(!File(target, "collections.html").isFile)
      val rewritten: Option[String] =
        site.pages.rewriteRequest(Path.fromHref("/collections")).map(_.toString)
      assert(rewritten.contains("/archive-collections.html"), rewritten)
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(table.exists(_.from == Seq("collections")), table.map(_.from).toString)
      val viaTable: Option[String] =
        CollectionAliases.rewrite(Path.fromHref("/collections"), table).map(_.toString)
      assert(viaTable.contains("/archive-collections.html"), viaTable)
      val about: String = html(target, "about.html")
      assert(about.contains("""href="/archive-collections.html""""), about)
  }
