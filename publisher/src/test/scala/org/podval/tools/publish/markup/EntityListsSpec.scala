package org.podval.tools.publish.markup

import org.podval.store.selector
import org.podval.tools.publish.page.{DirectoryPage, EntityListPage, Page, StoreTree}
import org.podval.tools.publish.site.{CollectionAliases, Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class EntityListsSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Names Fixture
      |description: Entity list tests
      |url: http://names.test
      |author: Test
      |email: test@names.test
      |lang: ru
      |""".stripMargin

  private val defaultFiles: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "index.md" ->
      """---
        |title: Home
        |---
        |Home.
        |""".stripMargin,
    "name.xml" ->
      """<entityLists>
        |  <title>Имена</title>
        |  <listPerson n="jews" role="jew"><title>Жиды</title></listPerson>
        |  <listPerson n="officials" role="official"><title>Начальство</title></listPerson>
        |  <listPerson n="unknown"><title>Неизвестно кто</title></listPerson>
        |  <listPlace n="places"><title>Места</title></listPlace>
        |  <listOrg n="organizations"><title>Организации</title></listOrg>
        |</entityLists>
        |""".stripMargin,
    "name/ab.xml" ->
      """<person role="jew">
        |  <persName>Earlier</persName>
        |</person>
        |""".stripMargin,
    "name/alter-rebbe.xml" ->
      """<person role="jew">
        |  <persName>Залман Борухович</persName>
        |  <p>founder</p>
        |</person>
        |""".stripMargin,
    "name/plain.xml" ->
      """<person>
        |  <persName>Nobody</persName>
        |</person>
        |""".stripMargin,
    "name/Вильна.xml" ->
      """<place>
        |  <placeName>Вильна</placeName>
        |</place>
        |""".stripMargin,
    "name/яков.xml" ->
      """<person role="jew">
        |  <persName>Аарон</persName>
        |</person>
        |""".stripMargin,
    "doc.xml" ->
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><title>A document</title></titleStmt></fileDesc></teiHeader>
        |  <text><body>
        |    <p>See <persName ref="alter-rebbe">the Rebbe</persName>.</p>
        |    <listPerson n="inline"><title>Not a directory list</title></listPerson>
        |  </body></text>
        |</TEI>
        |""".stripMargin
  )

  private def withSite(
    extra: Map[String, String] = Map.empty
  )(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-entity-lists")
    try
      val dir: File = path.toFile
      (defaultFiles ++ extra).foreach: (relative, content) =>
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

  test("overall catalog is TOC of non-empty lists only") {
    withSite(): (_, target) =>
      val page: String = html(target, "name/index.html")
      assert(page.contains("Имена"), page)
      assert(page.contains("Жиды"), page)
      assert(page.contains("""class="entity-lists-toc""""), page)
      assert(page.contains("""href="/name/jews.html""""), page)
      assert(page.contains("""href="/name/places.html""""), page)
      assert(page.contains("""href="/name/unknown.html""""), page)
      assert(!page.contains("Начальство"), page)
      assert(!page.contains("Организации"), page)
      assert(!page.contains("Залман Борухович"), page)
      assert(!page.contains("Вильна"), page)
      assert(!page.contains("Nobody"), page)
      assert(!page.contains("""href="#jews""""), page)
      assert(!page.contains("""class="page-list""""), page)
      assert(!page.contains("Not a directory list"), page)
  }

  test("empty lists have no subpage") {
    withSite(): (_, target) =>
      assert(File(target, "name/jews.html").isFile)
      assert(File(target, "name/places.html").isFile)
      assert(File(target, "name/unknown.html").isFile)
      assert(!File(target, "name/officials.html").exists)
      assert(!File(target, "name/organizations.html").exists)
  }

  test("list label is the first name, not the file name") {
    withSite(): (_, target) =>
      val page: String = html(target, "name/jews.html")
      assert(page.contains("Залман Борухович"), page)
      assert(!page.contains(">alter-rebbe<"), page)
  }

  test("members are sorted by display name in the site language") {
    withSite(): (_, target) =>
      val page: String = html(target, "name/jews.html")
      val aaron: Int = page.indexOf("Аарон")
      val zalman: Int = page.indexOf("Залман Борухович")
      // File яков.xml would follow alter-rebbe.xml by file id. The heading Аарон leads the list.
      assert(aaron >= 0 && zalman >= 0 && aaron < zalman, page)
      assert(page.indexOf("""href="/name/яков.html"""") < page.indexOf("""href="/name/alter-rebbe.html""""), page)
  }

  test("list subpage has members of that list only") {
    withSite(): (_, target) =>
      val jews: String = html(target, "name/jews.html")
      assert(jews.contains("store-header"), jews)
      assert(jews.contains("""class="store-current">Жиды<"""), jews)
      assert(!jews.contains("store-by"), jews)
      assert(!jews.contains("имя"), jews)
      assert(jews.contains("Жиды"), jews)
      assert(jews.contains("Залман Борухович"), jews)
      assert(jews.contains("""href="/name/alter-rebbe.html""""), jews)
      assert(!jews.contains("Вильна"), jews)
      assert(!jews.contains("Nobody"), jews)
      val places: String = html(target, "name/places.html")
      assert(places.contains("""class="store-current">Места<"""), places)
      assert(!places.contains("имя"), places)
      assert(!places.contains("store-by"), places)
      assert(places.contains("Места"), places)
      assert(places.contains("Вильна"), places)
      assert(!places.contains("Залман"), places)
      assert(!places.contains("<svg"), places)
  }

  test("listPerson inside a TEI document is not filled from the directory") {
    withSite(): (_, target) =>
      val page: String = html(target, "doc.html")
      assert(page.contains("Not a directory list"), page)
      assert(!page.contains("Залман Борухович"), page)
  }

  test("list pages are direct store children; members are By name") {
    withSite(): (site, _) =>
      val directory: DirectoryPage = site.pages.pages.collect:
        case page: DirectoryPage if page.doc.exists(_.asEntityLists.isDefined) => page
      .head
      val tree = StoreTree.node(directory)
      assert(StoreTree.by(directory).isEmpty)
      val jews = tree.resolve("/jews")
      assert(jews.last.names.hasName("jews"))
      assert(jews.last.names.hasName("Жиды"))
      assert(jews.structureNames == Seq("jews"), jews.structureNames)
      intercept[IllegalArgumentException] { tree.resolve("/names/jews") }
      val jewsPage = StoreTree.pageOf(jews.last).get
      val jewsResolved = StoreTree.resolvePage(tree, "/jews").get
      assert(jewsResolved.page.path == jewsPage.path)
      assert(jewsResolved.hop.isEmpty)
      assert(StoreTree.by(jewsResolved.page).map(_.selector) == Some(site.terms.selector("name")))
      assert(jewsPage.isInstanceOf[EntityListPage])
      assert(site.pages.rewriteRequest(Path.fromHref("/jews")).contains(jewsPage.path))
      assert(site.pages.rewriteRequest(Path.fromHref("/names/jews")).isEmpty)
      val from: Page = site.pages.pages.head
      assert(site.pages.resolve("/jews", None, from).map(_.page.real).contains(jewsPage))
      assert(site.pages.resolve("/names/jews", None, from).isEmpty)
      val zalman = tree.resolve("/jews/alter-rebbe")
      assert(zalman.last.names.hasName("alter-rebbe"))
      assert(zalman.last.names.hasName("Залман Борухович"))
      assert(zalman.structureNames == Seq("jews", "name", "alter-rebbe"), zalman.structureNames)
      val zalmanPage = StoreTree.pageOf(zalman.last).get
      val zalmanResolved = StoreTree.resolvePage(tree, "/jews/alter-rebbe").get
      assert(zalmanResolved.hop.map(_.selector) == Some(site.terms.selector("name")))
      assert(StoreTree.by(zalmanResolved.page).isEmpty)
      assert(site.pages.rewriteRequest(Path.fromHref("/jews/alter-rebbe")).contains(zalmanPage.path))
      val vilna = tree.resolve("/places/Вильна")
      assert(vilna.last.names.hasName("Вильна"))
      intercept[IllegalArgumentException] { tree.resolve("/officials") }
      intercept[IllegalArgumentException] { tree.resolve("/organizations") }
      val siteStore = site.pages.siteStore
      assert(siteStore.names.hasName("Names Fixture"))
      assert(siteStore.stores.exists(store => StoreTree.pageOf(store).contains(directory)))
  }

  test("identity prefix rewrites /name and /name/id to the catalog tree") {
    withSite(): (site, target) =>
      assert(!File(target, "name.html").isFile)
      assert(site.pages.rewriteRequest(Path.fromHref("/name")).contains(Path("name", "index").html))
      assert(
        site.pages.rewriteRequest(Path.fromHref("/name/alter-rebbe"))
          .contains(Path("name", "alter-rebbe").html)
      )
      val table: Seq[CollectionAliases.Entry] = CollectionAliases.entries(site.pages)
      val name: CollectionAliases.Entry = table.find(_.from == Seq("name")).get
      assert(name.to == Seq("name"), name.to)
      assert(name.index == Path("name", "index").html, name.index.toString)
      assert(
        CollectionAliases.rewrite(Path.fromHref("/name/alter-rebbe"), table)
          .contains(Path("name", "alter-rebbe").html)
      )
  }

  test("members are site-wide by kind and role") {
    withSite(Map(
      "notes/extra.xml" ->
        """<person role="jew">
          |  <persName>Scattered</persName>
          |</person>
          |""".stripMargin
    )): (_, target) =>
      val jews: String = html(target, "name/jews.html")
      assert(jews.contains("Scattered"), jews)
      assert(jews.contains("""href="/notes/extra.html""""), jews)
  }

  test("a catalog with one non-empty list is that list") {
    withSite(Map(
      "other.xml" ->
        """<entityLists>
          |  <title>Other</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |  <listOrg n="empty"><title>Empty</title></listOrg>
          |</entityLists>
          |""".stripMargin,
      "other/rome.xml" ->
        """<place>
          |  <placeName>Rome</placeName>
          |</place>
          |""".stripMargin
    )): (_, target) =>
      val page: String = html(target, "other/index.html")
      assert(page.contains("Rome"), page)
      assert(page.contains("""href="/other/rome.html""""), page)
      assert(!page.contains("entity-lists-toc"), page)
      assert(!File(target, "other/cities.html").exists)
      assert(!File(target, "other/empty.html").exists)
  }

  test("two catalogs stay independent") {
    withSite(Map(
      "other.xml" ->
        """<entityLists>
          |  <title>Other</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |</entityLists>
          |""".stripMargin,
      "other/rome.xml" ->
        """<place>
          |  <placeName>Rome</placeName>
          |</place>
          |""".stripMargin
    )): (_, target) =>
      val names: String = html(target, "name/index.html")
      val other: String = html(target, "other/index.html")
      assert(names.contains("Жиды"), names)
      assert(!names.contains("Rome"), names)
      assert(other.contains("Rome"), other)
      assert(!other.contains("Жиды"), other)
  }

  test("list prose sits above the list and keeps the head when the file has no title") {
    withSite(Map(
      "name/places.md" -> "Рядом со списком.\n",
      "note.md" -> "Смотри [места](/name/places#places).\n"
    )): (_, target) =>
      val places: String = html(target, "name/places.html")
      val proseAt: Int = places.indexOf("Рядом со списком.")
      val memberAt: Int = places.indexOf("Вильна")
      assert(proseAt >= 0 && memberAt > proseAt, places)
      assert(places.contains("""class="store-current">Места<"""), places)
      assert(places.contains("""<title>Места | Names Fixture</title>"""), places)
      val note: String = html(target, "note.html")
      assert(note.contains("""href="/name/places.html#places""""), note)
  }

  test("an h1 replaces the list heading and the catalog TOC stays the head") {
    withSite(Map(
      "name/places.md" ->
        """# Города
          |
          |Смотри список.
          |""".stripMargin
    )): (_, target) =>
      val places: String = html(target, "name/places.html")
      assert(places.contains("""class="store-current">Города<"""), places)
      assert(places.contains("""<title>Города | Names Fixture</title>"""), places)
      assert(!places.contains("""class="store-current">Места<"""), places)
      val catalog: String = html(target, "name/index.html")
      assert(catalog.contains("""href="/name/places.html""""), catalog)
      assert(catalog.contains(">Места<"), catalog)
      assert(!catalog.contains("Города"), catalog)
  }

  test("a place file at the list path is not absorbed") {
    withSite(Map(
      "name/places.xml" ->
        """<place>
          |  <placeName>Не список</placeName>
          |</place>
          |""".stripMargin
    )): (_, target) =>
      val errors: String = html(target, "errors.html")
      assert(errors.split("entity list 'places' collides").length - 1 == 1, errors)
      val page: String = html(target, "name/places.html")
      assert(page.contains("Не список"), page)
      assert(!page.contains("Вильна"), page)
      assert(!page.contains("""id="places""""), page)
  }

  test("one non-empty list takes prose on the catalog and keeps the fragment") {
    withSite(Map(
      "other.xml" ->
        """<entityLists>
          |  <title>Other</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |  <listOrg n="empty"><title>Empty</title></listOrg>
          |</entityLists>
          |""".stripMargin,
      "other/rome.xml" ->
        """<place>
          |  <placeName>Rome</placeName>
          |</place>
          |""".stripMargin,
      "other/cities.md" ->
        """Города рядом.
          |
          |[каталог](index.html)
          |""".stripMargin,
      "note.md" -> "Смотри [города](/other#cities).\n"
    )): (_, target) =>
      val page: String = html(target, "other/index.html")
      val proseAt: Int = page.indexOf("Города рядом.")
      val romeAt: Int = page.indexOf("Rome")
      assert(proseAt >= 0 && romeAt > proseAt, page)
      assert(page.contains("""href="/other.html""""), page)
      assert(page.contains("""<title>Other | Names Fixture</title>"""), page)
      assert(!File(target, "other/cities.html").exists)
      val note: String = html(target, "note.html")
      assert(note.contains("""href="/other.html#cities""""), note)
  }

  test("a catalog file beside its directory absorbs list prose and resolves from the published page") {
    withSite(Map(
      "lists.xml" ->
        """<entityLists>
          |  <title>Catalog</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |</entityLists>
          |""".stripMargin,
      "lists/rome.xml" ->
        """<place>
          |  <placeName>Rome</placeName>
          |</place>
          |""".stripMargin,
      "lists/cities.md" ->
        """---
          |title: Cities
          |---
          |# Towns
          |
          |[Рим](rome.html)
          |
          |[список](cities.html)
          |""".stripMargin
    )): (_, target) =>
      assert(!File(target, "lists/cities.html").exists)
      val page: String = html(target, "lists/index.html")
      assert(page.contains("""<title>Catalog | Names Fixture</title>"""), page)
      assert(page.contains("Towns"), page)
      assert(!page.contains("""<title>Towns"""), page)
      assert(!page.contains("""<title>Cities"""), page)
      assert(page.contains("Rome"), page)
      assert(page.contains("""href="/lists/rome.html""""), page)
      assert(page.contains("""href="cities.html""""), page)
      assert(!page.contains("lists/cities.html"), page)
      val romeLink: Int = page.indexOf(">Рим<")
      assert(romeLink >= 0, page)
      val around: String = page.substring((romeLink - 250).max(0), romeLink)
      assert(around.contains("""href="/lists/rome.html""""), around)
      val errors: String = html(target, "errors.html")
      val items: Seq[String] = kindItems(errors, "ambiguous-title")
      assert(items.length == 1, errors)
      val item: String = items.mkString
      assert(item.contains("ambiguous title"), item)
      assert(item.contains("lists/cities.md"), item)
  }

  private def kindItems(errors: String, id: String): Seq[String] =
    val start: Int = errors.indexOf(s"""id="$id"""")
    assert(start >= 0, errors)
    val ul: Int = errors.indexOf("<ul", start)
    val end: Int = errors.indexOf("</ul>", ul)
    errors.substring(ul, end).split("<li>").drop(1).toSeq

  test("entity page keeps document backlinks and does not list the names index") {
    withSite(): (_, target) =>
      val page: String = html(target, "name/alter-rebbe.html")
      assert(page.contains("Обратные ссылки"), page)
      val back: Int = page.indexOf("class=\"backlinks\"")
      assert(back >= 0, page)
      val section: String = page.substring(back)
      assert(section.contains("A document") || section.contains("/doc.html"), section)
      assert(!section.contains("/name/index.html"), section)
  }

  test("prose in name/places.md is a backlink and a member href is not") {
    withSite(Map(
      "name/places.md" -> "Смотри [заметку](/note.html).\n",
      "note.md" ->
        """---
          |title: Note
          |---
          |Заметка.
          |""".stripMargin
    )): (site, target) =>
      val note: String = html(target, "note.html")
      val back: Int = note.indexOf("class=\"backlinks\"")
      assert(back >= 0, note)
      assert(note.substring(back).contains("/name/places.html"), note)
      val vilna: String = html(target, "name/Вильна.html")
      val vilnaBack: Int = vilna.indexOf("class=\"backlinks\"")
      val section: String = if vilnaBack < 0 then "" else vilna.substring(vilnaBack)
      assert(!section.contains("/name/places.html"), section)
      assert(!site.backLinks.all.exists(link =>
        link.from.publishedPath.toString == "/name/places.html" &&
          link.to.page.publishedPath.toString == "/name/Вильна.html"
      ))
  }

  test("one-list prose backlink is the catalog, and inline front matter is membership and tags only") {
    val graphConfig: String =
      """title: Names Fixture
        |description: Entity list tests
        |url: http://names.test
        |author: Test
        |email: test@names.test
        |lang: ru
        |graph:
        |  enabled: true
        |""".stripMargin
    withSite(Map(
      "_site_config.yml" -> graphConfig,
      "other.xml" ->
        """---
          |categories:
          |  - "[[Shared]]"
          |---
          |<entityLists>
          |  <title>Other</title>
          |  <listPlace n="cities"><title>Cities</title></listPlace>
          |  <listOrg n="empty"><title>Empty</title></listOrg>
          |</entityLists>
          |""".stripMargin,
      "other/rome.xml" ->
        """<place>
          |  <placeName>Rome</placeName>
          |</place>
          |""".stripMargin,
      "other/cities.md" ->
        """---
          |categories:
          |  - "[[Shared]]"
          |  - "[[InlineOnly]]"
          |tags:
          |  - inline-tag
          |---
          |[Rome](rome.html)
          |""".stripMargin,
      "Shared.md" ->
        """---
          |title: Shared
          |---
          |Shared hub.
          |""".stripMargin,
      "InlineOnly.md" ->
        """---
          |title: InlineOnly
          |---
          |Inline hub.
          |""".stripMargin
    )): (site, target) =>
      assert(File(target, "other/index.html").isFile)
      assert(!File(target, "other/cities.html").exists)
      val catalog: DirectoryPage = site.pages.pages.collectFirst:
        case directory: DirectoryPage if directory.sourcePath.exists(_.toString.contains("other.xml")) =>
          directory
      .get
      assert(catalog.hasSyntheticContent)
      assert(catalog.publishedPath.toString == "/other.html", catalog.publishedPath)
      val page: String = html(target, "other/index.html")
      assert(!page.contains("page-meta"), page)
      val chips: Seq[org.podval.xml.Xml.Element] = site.categories.chips(catalog)
      assert(chips.length == 1, chips.map(_.getText))
      assert(chips.head.hasClass("page-category"))
      assert(chips.head.getHref.contains("/Shared.html"))
      assert(chips.head.getText.contains("Shared"))
      assert(!chips.exists(_.getHref.exists(_.contains("InlineOnly"))))
      val membership = site.categories.membership.filter(_.from.path == catalog.path)
      assert(membership.length == 2, membership.map(_.to.publishedPath))
      assert(membership.exists(link => link.to.publishedPath.toString == "/Shared.html" && !link.fromInline))
      assert(membership.exists(link => link.to.publishedPath.toString == "/InlineOnly.html" && link.fromInline))
      def members(relative: String): String =
        val hub: String = html(target, relative)
        val start: Int = hub.indexOf("category-members")
        assert(start >= 0, hub)
        hub.substring(start, hub.indexOf("</article>", start))
      val sharedMembers: String = members("Shared.html")
      val inlineMembers: String = members("InlineOnly.html")
      assert(sharedMembers.split("/other.html").length - 1 == 1, sharedMembers)
      assert(inlineMembers.split("/other.html").length - 1 == 1, inlineMembers)
      val tags: String = html(target, "tags.html")
      assert(tags.contains("inline-tag"), tags)
      assert(tags.contains("/other.html"), tags)
      val romeLinks = site.backLinks.all.filter(_.to.page.publishedPath.toString == "/other/rome.html")
      assert(romeLinks.length == 1, romeLinks.map(_.from.publishedPath))
      assert(romeLinks.head.from.path == catalog.path)
      val graph: String = Files.read(File(target, "graph.json"))
      def once(edge: String): Unit =
        assert(graph.split(java.util.regex.Pattern.quote(edge), -1).length - 1 == 1, graph)
      once(""""source":"/other.html","target":"/other/rome.html","kind":"link"""")
      once(""""source":"/other.html","target":"/Shared.html","kind":"link"""")
      once(""""source":"/other.html","target":"/InlineOnly.html","kind":"link"""")
      val idAt: Int = graph.indexOf(""""id":"/other.html"""")
      assert(idAt >= 0, graph)
      val node: String = graph.substring(graph.lastIndexOf("{", idAt), graph.indexOf("}", idAt) + 1)
      assert(!node.contains("inline-tag"), node)
  }
