package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.store.selector
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
      |lang: ru
      |header-pages:
      |  - names.md
      |  - archive-collections
      |  - about.md
      |home: /archive-index.html
      |""".stripMargin

  private def withSite(extra: Map[String, String] = Map.empty)(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-store-indexes")
    try
      val dir: File = path.toFile
      (files ++ extra).foreach: (relative, content) =>
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

  private val refConfig: String =
    """title: Ref Fixture
      |description: ref
      |url: http://ref.test
      |author: Test
      |email: test@ref.test
      |lang: ru
      |""".stripMargin

  private def openSite(written: Map[String, String])(body: Site => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-store-ref")
    try
      val dir: File = path.toFile
      written.foreach: (relative, content) =>
        Files.write(File(dir, relative), content)
      val site: Site = Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(File(dir, "_site").getAbsolutePath),
        treatErrorsAsWarnings = true,
        logLevelOpt = Some("WARN")
      ))
      body(site)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def html(target: File, relative: String): String =
    Files.read(File(target, relative)).replaceAll("\\s+", " ").replace("= ", "=")

  private def badLabelItems(errors: String): Seq[String] =
    val start: Int = errors.indexOf("""id="bad-label"""")
    assert(start >= 0, errors)
    val ul: Int = errors.indexOf("<ul", start)
    val end: Int = errors.indexOf("</ul>", ul)
    errors.substring(ul, end).split("<li>").toSeq.drop(1)

  private val archiveTerms: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<Terms>
      |  <term id="archive" selector="yes">
      |    <name lang="en" n="archive" plural="archives"/>
      |    <name lang="ru" n="архив" plural="архивы"/>
      |  </term>
      |  <term id="book" selector="yes">
      |    <name lang="en" n="book" plural="books"/>
      |    <name lang="ru" n="книга" plural="книги"/>
      |  </term>
      |</Terms>
      |""".stripMargin

  private val files: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "_terms.xml" -> archiveTerms,
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
        |See [дела](/archive-collections).
        |""".stripMargin,
    "archive.xml" ->
      """<store ref="archive" plural="yes" xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="archive">
        |    <xi:include href="archive/books.xml"/>
        |    <xi:include href="archive/rgada.xml"/>
        |    <xi:include href="archive/bare.xml"/>
        |    <xi:include href="archive/rgia.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "archive/books.xml" ->
      """<store ref="book" plural="yes" xmlns:xi="http://www.w3.org/2001/XInclude">
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
        |""".stripMargin,
    "archive/rgia.xml" ->
      """<collection>
        |  <name n="rgia"/>
        |</collection>
        |""".stripMargin
  )

  test("wraps StoreIndex as org.podval.store") {
    withSite(): (site, _) =>
      val root: Page = site.pages.pages.find(StoreIndexes.isRootStore).get
      val tree = StoreTree.node(root)
      assert(StoreTree.node(root) eq tree)
      val atRoot = StoreTree.resolvePage(tree, "/").get
      assert(atRoot.page.path == root.path)
      assert(atRoot.hop.isEmpty)
      assert(StoreTree.by(atRoot.page).map(_.selector) == Some(site.terms.selector("archive")))
      assert(root.names.doFind(Language.English.toSpec).name == "archives")
      assert(root.names.doFind(Language.Russian.toSpec).name == "архивы")
      assert(site.pages.siteStore.resolve("/archives").structureNames == Seq("archives"))
      assert(site.pages.rewriteRequest(Path.fromHref("/archives")).contains(root.path))
      assert(site.pages.rewriteRequest(Path.fromHref("/Archives")).isEmpty)
      assert(site.pages.rewriteRequest(Path.fromHref("/Архивы")).isEmpty)
      assert(StoreTree.pageOf(tree.resolve("/archive").last).isEmpty)
      assert(StoreTree.pageAt(tree, "/archive").isEmpty)
      val books = tree.resolve("/books")
      assert(books.last.names.hasName("книги"))
      assert(books.last.names.hasName("books"))
      // First segment is the archive By hop. The root's stored name is the lexicon plural.
      assert(books.structureNames == Seq("archive", "books"))
      val booksResolved = StoreTree.resolvePage(tree, "/books").get
      assert(booksResolved.path.structureNames == books.structureNames)
      assert(booksResolved.hop.map(_.selector) == Some(site.terms.selector("archive")))
      assert(StoreTree.by(booksResolved.page).map(_.selector) == Some(site.terms.selector("book")))
      assert(booksResolved.hop.map(_.selector) != StoreTree.by(booksResolved.page).map(_.selector))
      val derzhavin = tree.resolve("/books/Державин")
      assert(derzhavin.last.names.hasName("Державин"))
      assert(derzhavin.structureNames == Seq("archive", "books", "book", "Державин"))
      val derzhavinResolved = StoreTree.resolvePage(tree, "/books/Державин").get
      assert(derzhavinResolved.path.structureNames == derzhavin.structureNames)
      assert(derzhavinResolved.hop.map(_.selector) == Some(site.terms.selector("book")))
      assert(StoreTree.by(derzhavinResolved.page).map(_.selector) == Some(site.terms.selector("document")))
      assert(derzhavinResolved.hop.map(_.selector) != StoreTree.by(derzhavinResolved.page).map(_.selector))
      val rgada = root.store.flatMap(_.boundChildren.find(_.store.flatMap(_.alias).contains("rgada")))
      assert(rgada.isDefined)
      assert(tree.resolve("/rgada").last.names.hasName("РГАДА"))
      val booksPage = StoreTree.pageOf(tree.resolve("/books").last).get
      assert(site.pages.rewriteRequest(Path.fromHref("/books")).contains(booksPage.path))
      val from: Page = site.pages.pages.head
      val hop: Path = Path("archive", "books", "book", "index").html
      assert(!site.pages.pages.exists(_.path == hop))
      site.pages.resolve(hop.toString, None, from).foreach: link =>
        assert(site.pages.pages.exists(_ eq link.page))
      assert(site.pages.resolve("/books", None, from).map(_.page.real).contains(booksPage))
      val derzhavinPage = StoreTree.pageOf(derzhavin.last).get
      assert(site.pages.rewriteRequest(Path.fromHref("/books/Державин")).contains(derzhavinPage.path))
      assert(site.pages.resolve("/books/Державин", None, from).map(_.page.real).contains(derzhavinPage))
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(table.exists(_.from == Seq("rgada")), table.map(_.from).toString)
      assert(!table.exists(_.from == Seq("collections")), table.map(_.from).toString)
      val siteStore = site.pages.siteStore
      assert(siteStore.names.hasName("Archive Fixture"))
      assert(siteStore.stores.exists(store => StoreTree.pageOf(store).exists(StoreIndexes.isRootStore)))
      assert(StoreTree.pageAt(siteStore, "/rgada").isEmpty)
      val rgia = site.pages.pages.find(_.names.hasName("rgia")).get
      assert(PageHeader.pageDisplayName(rgia) == "rgia")
      assert(rgia.listTitle == "rgia")
  }

  test("writes archive-collections tree and archive-index flat list") {
    withSite(): (_, target) =>
      assert(File(target, "archive-collections.html").isFile)
      assert(File(target, "archive-index.html").isFile)
      val tree: String = html(target, "archive-collections.html")
      assert(tree.contains("<title>Дерево | Archive Fixture</title>"), tree)
      assert(tree.contains("""class="tree-index""""), tree)
      assert(tree.contains("<em>архив</em>"), tree)
      assert(tree.contains("Книги:"), tree)
      assert(tree.contains("rgia:"), tree)
      assert(!tree.contains("Rgia"), tree)
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
      assert(flat.contains("<title>Коллекции | Archive Fixture</title>"), flat)
      assert(flat.contains("архив Книги / книга Державин: Переписка"), flat)
      assert(flat.contains("архив РГАДА: О секте"), flat)
      assert(flat.contains("From the notes."), flat)
      assert(flat.contains("Донос"), flat)
      val derzhavinItem: Int = flat.indexOf("книга Державин")
      val rgadaItem: Int = flat.indexOf("архив РГАДА")
      assert(derzhavinItem >= 0 && rgadaItem > derzhavinItem, flat)
      val body: String = flat.substring(flat.indexOf("page-body"), flat.indexOf("</article>"))
      val items: Int = "<li>".r.findAllMatchIn(body).size
      val abstracts: Int = "<abstract".r.findAllMatchIn(body).size
      assert(abstracts == items, s"abstracts=$abstracts items=$items $body")
      assert(body.contains("<abstract></abstract>"), body)
  }

  test("one root store is left off the descendant path") {
    withSite(): (site, target) =>
      val page: Page = site.pages.pages.find(_.title.contains("Переписка")).get
      val doc: String = html(target, page.path.toString.dropWhile(_ == '/'))
      val nav: String = storePath(doc)
      assert(!nav.contains("Архивы"), nav)
      assert(nav.contains("Книги"), nav)
      assert(site.pages.rewriteRequest(Path.fromHref("/collections")).isEmpty)
      assert(site.pages.rewriteRequest(Path.fromHref("/years")).isEmpty)
  }

  test("a second root store stays on the descendant path") {
    withSite(Map(
      "other.xml" ->
        """<store>
          |  <by selector="archive"/>
          |</store>
          |""".stripMargin
    )): (site, target) =>
      val page: Page = site.pages.pages.find(_.title.contains("Переписка")).get
      val doc: String = html(target, page.path.toString.dropWhile(_ == '/'))
      val nav: String = storePath(doc)
      assert(nav.contains("Архивы"), nav)
  }

  private def storePath(doc: String): String =
    val start: Int = doc.indexOf("""class="store-path"""")
    assert(start >= 0, doc)
    doc.substring(start, doc.indexOf("</nav>", start))

  test("home refreshes to archive-index; header lists Дерево after Имена") {
    withSite(): (_, target) =>
      val index: String = html(target, "index.html")
      assert(index.toLowerCase.contains("refresh"), index)
      assert(index.contains("/archive-index.html"), index)
      val tree: String = html(target, "archive-collections.html")
      val nav: String = tree.substring(tree.indexOf("site-nav"))
      val namesHref: Int = nav.indexOf("""href="/names.html"""")
      val collectionsHref: Int = nav.indexOf("""href="/archive-collections.html"""")
      val aboutHref: Int = nav.indexOf("""href="/about.html"""")
      assert(namesHref >= 0 && collectionsHref > namesHref && aboutHref > collectionsHref, nav)
      assert(tree.contains("""class="page-title"""), tree)
      assert(tree.contains("Дерево"), tree)
  }

  test("store and selector names follow site lang") {
    val english: String = siteConfig.replace("lang: ru", "lang: en")
    withSite(Map("_site_config.yml" -> english)): (_, target) =>
      val tree: String = html(target, "archive-collections.html")
      assert(tree.contains("<title>Tree | Archive Fixture</title>"), tree)
      assert(tree.contains("<em>archive</em>"), tree)
      assert(tree.contains("Books:"), tree)
      assert(tree.contains("rgia:"), tree)
      assert(!tree.contains("Rgia"), tree)
      assert(tree.contains("<em>book</em>"), tree)
      assert(!tree.contains("<em>архив</em>"), tree)
      val flat: String = html(target, "archive-index.html")
      assert(flat.contains("<title>Collections | Archive Fixture</title>"), flat)
      assert(flat.contains("archive Books / book Державин"), flat)
  }

  test("does not rewrite /collections; one root stays off the path without that alias") {
    withSite(): (site, target) =>
      assert(!File(target, "collections.html").isFile)
      assert(site.pages.rewriteRequest(Path.fromHref("/collections")).isEmpty)
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      assert(!table.exists(_.from == Seq("collections")), table.map(_.from).toString)
      assert(CollectionAliases.rewrite(Path.fromHref("/collections"), table).isEmpty)
      val about: String = html(target, "about.html")
      assert(about.contains("""href="/archive-collections.html""""), about)
  }

  test("renders a file at the collections and index paths above the lists") {
    withSite(Map(
      "archive-collections.md" ->
        """---
          |title: Дела по архивам
          |---
          |Дерево.
          |""".stripMargin,
      "archive-index.md" ->
        """---
          |title: Дела
          |---
          |Список.
          |""".stripMargin
    )): (site, target) =>
      val tree: String = html(target, "archive-collections.html")
      assert(tree.contains("""<h1 class="page-title">Дела по архивам</h1>"""), tree)
      assert(tree.contains("Дерево."), tree)
      val proseAt: Int = tree.indexOf("Дерево.")
      val listAt: Int = tree.indexOf("tree-index")
      assert(proseAt >= 0 && listAt > proseAt, tree)
      val flat: String = html(target, "archive-index.html")
      assert(flat.contains("""<h1 class="page-title">Дела</h1>"""), flat)
      assert(flat.contains("Список."), flat)
      val flatProse: Int = flat.indexOf("Список.")
      val flatList: Int = flat.indexOf("Державин")
      assert(flatProse >= 0 && flatList > flatProse, flat)
      assert(site.pages.pages.count(_.path.toString == "/archive-collections.html") == 1)
      assert(site.pages.pages.count(_.path.toString == "/archive-index.html") == 1)
      val errors: String = html(target, "errors.html")
      assert(!errors.contains("Duplicates for the path"), errors)
  }

  test("a site file that repeats document fails") {
    val caught: IllegalArgumentException = intercept[IllegalArgumentException] {
      openSite(Map(
        "_site_config.yml" -> refConfig,
        "_terms.xml" ->
          """<Terms>
            |  <term id="document" selector="yes">
            |    <name lang="en" n="document"/>
            |  </term>
            |</Terms>
            |""".stripMargin
      ))(_ => ())
    }
    assert(caught.getMessage.contains("Duplicate term ids: document"), caught.getMessage)
  }

  test("ref and plural are a pair and the message names the source") {
    def fails(file: String, xml: String, phrase: String): Unit =
      openSite(Map(
        "_site_config.yml" -> refConfig,
        file -> xml
      )): site =>
        site.generate()
        val items: Seq[String] = badLabelItems(html(site.targetDirectory, "errors.html"))
        val hits: Seq[String] = items.filter(item => item.contains(phrase) && item.contains(s"/$file"))
        assert(hits.length == 1, items.mkString("\n"))
        assert(hits.head.contains(s"bad label: $phrase"), hits.head)

    fails("plural.xml", """<store plural="yes"/>""", "plural without ref")
    fails("ref-only.xml", """<store ref="book"/>""", "ref without plural")
    fails(
      "named.xml",
      """<store ref="book" plural="yes"><name lang="en" n="books"/></store>""",
      "ref with names"
    )
    fails("missing.xml", """<store ref="nope" plural="yes"/>""", "Unknown term: nope")
    fails("rooted.xml", """<store ref="book" plural="yes" n="books"/>""", "ref with names")
  }

  test("a year, tree, and flat re-home does not add a second bad label") {
    withSite(Map(
      "archive/rgada/900.xml" ->
        """<TEI>
          |  <teiHeader>
          |    <fileDesc><titleStmt><title>t</title></titleStmt></fileDesc>
          |    <profileDesc>
          |      <creation><date when="1798-08-11">11 августа 1798</date></creation>
          |    </profileDesc>
          |  </teiHeader>
          |  <text xml:lang="ru"><body><p>hello</p></body></text>
          |</TEI>
          |""".stripMargin,
      "archive-collections.xml" -> """<store plural="yes"/>""",
      "archive-index.xml" -> """<store ref="book"/>""",
      "archive-years.xml" -> """<store ref="nope" plural="yes"/>"""
    )): (_, target) =>
      assert(html(target, "archive-collections.html").contains("""class="tree-index""""))
      assert(html(target, "archive-index.html").contains("Державин"))
      assert(html(target, "archive-years.html").contains("""class="year-index""""))
      val items: Seq[String] = badLabelItems(html(target, "errors.html"))
      assert(items.count(_.contains("/archive-collections.xml")) == 1, items.mkString("\n"))
      assert(items.count(_.contains("plural without ref")) == 1, items.mkString("\n"))
      assert(items.count(_.contains("/archive-index.xml")) == 1, items.mkString("\n"))
      assert(items.count(_.contains("ref without plural")) == 1, items.mkString("\n"))
      assert(items.count(_.contains("/archive-years.xml")) == 1, items.mkString("\n"))
      assert(items.count(_.contains("Unknown term: nope")) == 1, items.mkString("\n"))
  }

  test("a ref with no plural in the site language fails in namesOf") {
    val caught: IllegalArgumentException = intercept[IllegalArgumentException] {
      openSite(Map(
        "_site_config.yml" -> refConfig,
        "_terms.xml" ->
          """<Terms>
            |  <term id="shelf" selector="yes">
            |    <name lang="en" n="shelf" plural="shelves"/>
            |    <name lang="ru" n="полка"/>
            |  </term>
            |</Terms>
            |""".stripMargin,
        "shelf.xml" -> """<store ref="shelf" plural="yes"/>"""
      ))(_.generate())
    }
    assert(caught.getMessage.contains("No plural for term shelf"), caught.getMessage)
    assert(caught.getMessage.contains("/shelf.xml"), caught.getMessage)
    assert(!caught.getMessage.contains("Archives"), caught.getMessage)
  }
