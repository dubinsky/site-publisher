package org.podval.tools.publish.markup

import org.podval.tools.publish.page.Page
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class EntitySpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Entity Fixture
      |description: Entity ref tests
      |url: http://entity.test
      |author: Test
      |email: test@entity.test
      |""".stripMargin

  private val defaultFiles: Map[String, String] = Map(
    "_site_config.yml" -> siteConfig,
    "index.md" ->
      """---
        |title: Home
        |---
        |See [[кагал]].
        |""".stripMargin,
    "notes.md" ->
      """---
        |title: alter-rebbe
        |---
        |A note.
        |""".stripMargin,
    "doc.xml" ->
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><title>Entities</title></titleStmt></fileDesc></teiHeader>
        |  <text><body><p>
        |    See <persName ref="alter-rebbe">the Rebbe</persName>
        |    in <placeName ref="Вильна">Vilna</placeName>
        |    and <orgName ref="кагал">the kahal</orgName>.
        |    Bad kind <persName ref="Вильна">wrong</persName>.
        |    Missing <persName ref="nobody">x</persName>.
        |    Title <persName ref="Zalman">z</persName>.
        |    File <persName ref="ab">ab person</persName>
        |    and <placeName ref="ab">ab place</placeName>.
        |    Bare <persName>not a link</persName>.
        |  </p></body></text>
        |</TEI>
        |""".stripMargin,
    "people/alter-rebbe.xml" ->
      """<person>
        |  <persName>Залман Борухович</persName>
        |  <p>founder</p>
        |</person>
        |""".stripMargin,
    "people/ab.xml" ->
      """<person>
        |  <persName>Zalman</persName>
        |</person>
        |""".stripMargin,
    "places/Вильна.xml" ->
      """<place>
        |  <placeName>Вильна</placeName>
        |</place>
        |""".stripMargin,
    "places/ab.xml" ->
      """<place>
        |  <placeName>Somewhere</placeName>
        |</place>
        |""".stripMargin,
    "orgs/кагал.xml" ->
      """<org>
        |  <orgName>кагал</orgName>
        |</org>
        |""".stripMargin
  )

  private def withSite(
    extra: Map[String, String] = Map.empty
  )(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-entities")
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

  private def pageNamed(site: Site, name: String): Page =
    site.pages.pages.find(page => page.title == name || page.titleFromPath == name).get

  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file).replaceAll("\\s+", " ").replace("= ", "=")

  test("resolves entity refs by kind and filename") {
    withSite(): (site, target) =>
      val from: Page = pageNamed(site, "Entities")
      val person: Link = Link.resolve("alter-rebbe", Some(LinkKind.Entity(EntityKind.Person)), from).get
      assert(person.url == "/people/alter-rebbe.html")
      val place: Link = Link.resolve("Вильна", Some(LinkKind.Entity(EntityKind.Place)), from).get
      assert(place.url == "/places/Вильна.html")
      val org: Link = Link.resolve("кагал", Some(LinkKind.Entity(EntityKind.Organization)), from).get
      assert(org.url == "/orgs/кагал.html")

      val page: String = html(target, "doc.html")
      assert(page.contains("""href="/people/alter-rebbe.html""""), page)
      assert(page.contains("""href="/places/Вильна.html""""), page)
      assert(page.contains("""href="/orgs/кагал.html""""), page)
      assert(File(target, "people/alter-rebbe.html").isFile)
      assert(File(target, "places/Вильна.html").isFile)
      assert(File(target, "orgs/кагал.html").isFile)
  }

  test("kind mismatch, missing id, and displayed name do not resolve") {
    withSite(): (site, target) =>
      val from: Page = pageNamed(site, "Entities")
      assert(Link.resolve("Вильна", Some(LinkKind.Entity(EntityKind.Person)), from).isEmpty)
      assert(Link.resolve("nobody", Some(LinkKind.Entity(EntityKind.Person)), from).isEmpty)
      assert(Link.resolve("Zalman", Some(LinkKind.Entity(EntityKind.Person)), from).isEmpty)
      assert(Link.resolve("alter-rebbe", None, from).nonEmpty)

      val page: String = html(target, "doc.html")
      assert(page.contains("unresolved-link"), page)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("unresolved"), errors)
      assert(errors.contains("Вильна"), errors)
      assert(errors.contains("nobody"), errors)
      assert(errors.contains("Zalman"), errors)
  }

  test("same filename different kinds") {
    withSite(): (site, _) =>
      val from: Page = pageNamed(site, "Entities")
      val person: Link = Link.resolve("ab", Some(LinkKind.Entity(EntityKind.Person)), from).get
      assert(person.url == "/people/ab.html")
      val place: Link = Link.resolve("ab", Some(LinkKind.Entity(EntityKind.Place)), from).get
      assert(place.url == "/places/ab.html")
  }

  test("markdown page with the same title does not steal a person ref") {
    withSite(): (site, target) =>
      val from: Page = pageNamed(site, "Entities")
      val person: Link = Link.resolve("alter-rebbe", Some(LinkKind.Entity(EntityKind.Person)), from).get
      assert(person.url == "/people/alter-rebbe.html")
      val page: String = html(target, "doc.html")
      assert(page.contains("""href="/people/alter-rebbe.html""""), page)
      assert(!page.contains("""href="/notes.html""""), page)
  }

  test("collector header resolves entity refs in store titles") {
    withSite(Map(
      "col.xml" ->
        """<collection n="29">
          |  <title>About <persName ref="alter-rebbe">the Rebbe</persName></title>
          |</collection>
          |""".stripMargin,
      "col/001.md" -> "body\n"
    )): (_, target) =>
      val index: String = html(target, "col/index.html")
      assert(index.contains("""href="/people/alter-rebbe.html""""), index)
      val doc: String = html(target, "col/001.html")
      assert(doc.contains("""href="/people/alter-rebbe.html""""), doc)
      assert(doc.contains("<l>документ 001</l>"), doc)
      assert(!doc.contains("<tei-head>документ"), doc)
  }

  test("collection document header table from teiHeader") {
    withSite(Map(
      "col.xml" ->
        """<collection n="1"><title>C</title></collection>""".stripMargin,
      "col/003.xml" ->
        """<TEI>
          |  <teiHeader>
          |    <fileDesc><titleStmt>
          |      <author><persName ref="alter-rebbe">the Rebbe</persName></author>
          |      <editor role="transcriber"><persName ref="ab">A</persName></editor>
          |    </titleStmt></fileDesc>
          |    <profileDesc>
          |      <abstract>A description of <persName ref="alter-rebbe">him</persName>.</abstract>
          |      <creation><date when="1798-08-11">11 августа 1798</date></creation>
          |      <correspDesc><correspAction><persName role="addressee" ref="ab">Someone</persName></correspAction></correspDesc>
          |    </profileDesc>
          |  </teiHeader>
          |  <text xml:lang="ru"><body><p>hello</p></body></text>
          |</TEI>
          |""".stripMargin
    )): (_, target) =>
      val doc: String = html(target, "col/003.html")
      assert(doc.contains("document-header"), doc)
      assert(doc.contains("Описание"), doc)
      assert(doc.contains("1798-08-11"), doc)
      assert(doc.contains("Кто"), doc)
      assert(doc.contains("Кому"), doc)
      assert(doc.contains("Расшифровка"), doc)
      assert(doc.contains("""href="/people/alter-rebbe.html""""), doc)
      assert(doc.contains("""href="/people/ab.html""""), doc)
      val post: String = doc.substring(doc.indexOf("post-content"))
      assert(!post.contains("teiHeader"), post)
      assert(post.contains("hello"), post)
      val index: String = html(target, "col/index.html")
      assert(!index.contains("<tei-head>"), index)
  }

  test("wiki link still finds an entity by file name") {
    withSite(): (site, target) =>
      val home: Page = pageNamed(site, "Home")
      val link: Link = Link.resolve("кагал", None, home).get
      assert(link.url == "/orgs/кагал.html")
      val page: String = html(target, "index.html")
      assert(page.contains("""href="/orgs/кагал.html""""), page)
  }

  test("name without ref is not a link") {
    withSite(): (_, target) =>
      val page: String = html(target, "doc.html")
      assert(page.contains("<persName"), page)
      assert(page.contains("not a link"), page)
      val person: String = html(target, "people/alter-rebbe.html")
      assert(person.contains("<persName"), person)
      assert(person.contains("Залман Борухович"), person)
  }

  test("duplicate entity id is recorded and unresolved") {
    withSite(Map(
      "other/alter-rebbe.xml" ->
        """<person>
          |  <persName>Someone else</persName>
          |</person>
          |""".stripMargin
    )): (site, target) =>
      val from: Page = pageNamed(site, "Entities")
      assert(Link.resolve("alter-rebbe", Some(LinkKind.Entity(EntityKind.Person)), from).isEmpty)
      val errors: String = html(target, "errors.html")
      assert(errors.contains("duplicate"), errors)
      assert(errors.contains("person"), errors)
      assert(errors.contains("alter-rebbe"), errors)
  }
