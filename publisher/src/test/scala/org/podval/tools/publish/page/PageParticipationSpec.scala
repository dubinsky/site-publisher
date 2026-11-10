package org.podval.tools.publish.page

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class PageParticipationSpec extends AnyFunSuite:
  import PageParticipation.Absorption

  private val files: Map[String, String] = Map(
    "_site_config.yml" ->
      """title: Participation
        |description: page participation
        |url: http://participation.test
        |author: Test
        |email: test@participation.test
        |lang: en
        |facsimiles-url: https://facsimiles.test/
        |paginate-posts: 1
        |search:
        |  enabled: true
        |graph:
        |  enabled: true
        |""".stripMargin,
    "index.md" ->
      """---
        |title: Home
        |---
        |Home.
        |""".stripMargin,
    "note.md" ->
      """---
        |title: A note
        |pdf: true
        |---
        |A note.
        |""".stripMargin,
    "long.md" ->
      """---
        |title: Long
        |chunk: true
        |---
        |Preamble.
        |
        |## Alpha
        |
        |Leaf.
        |""".stripMargin,
    "alias.md" ->
      """---
        |title: Alias source
        |aliases:
        |  - short-name
        |---
        |Alias source.
        |""".stripMargin,
    "search.md" ->
      """---
        |title: Not the search page
        |---
        |Collides.
        |""".stripMargin,
    "photo.png" -> "png",
    "_posts/2020-01-01-one.md" ->
      """---
        |title: One
        |---
        |One.
        |""".stripMargin,
    "_posts/2020-01-02-two.md" ->
      """---
        |title: Two
        |---
        |Two.
        |""".stripMargin,
    "scan.xml" ->
      """<TEI>
        |  <teiHeader>
        |    <fileDesc><titleStmt><title>Scan</title></titleStmt></fileDesc>
        |  </teiHeader>
        |  <text xml:lang="en"><body><pb n="000-1"/><p>scan</p></body></text>
        |</TEI>
        |""".stripMargin,
    "_terms.xml" ->
      """<?xml version="1.0" encoding="UTF-8"?>
        |<Terms>
        |  <term id="archive" selector="yes">
        |    <name lang="en" n="archive" plural="archives"/>
        |  </term>
        |  <term id="case" selector="yes">
        |    <name lang="en" n="case" plural="cases"/>
        |  </term>
        |</Terms>
        |""".stripMargin,
    "archive.xml" ->
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <by selector="archive">
        |    <xi:include href="archive/case.xml"/>
        |  </by>
        |</store>
        |""".stripMargin,
    "archive/case.xml" ->
      """<collection n="1">
        |  <title>Case</title>
        |</collection>
        |""".stripMargin,
    "archive/case/leaf.xml" ->
      """<TEI>
        |  <teiHeader>
        |    <fileDesc><titleStmt><title>Leaf</title></titleStmt></fileDesc>
        |    <profileDesc>
        |      <abstract>Leaf</abstract>
        |      <creation><date when="1798-08-11">11 August 1798</date></creation>
        |    </profileDesc>
        |  </teiHeader>
        |  <text xml:lang="en"><body><p>leaf</p></body></text>
        |</TEI>
        |""".stripMargin,
    "name.xml" ->
      """<entityLists>
        |  <title>Names</title>
        |  <listPerson n="people"><title>People</title></listPerson>
        |  <listPlace n="places"><title>Places</title></listPlace>
        |</entityLists>
        |""".stripMargin,
    "name/ada.xml" ->
      """<person>
        |  <persName>Ada</persName>
        |</person>
        |""".stripMargin,
    "name/town.xml" ->
      """<place>
        |  <placeName>Town</placeName>
        |  <location><geo>54.68722 25.28000</geo></location>
        |</place>
        |""".stripMargin,
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
  )

  private def withSite(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-participation")
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

  private def row(
    page: Page,
    listed: Boolean,
    leaf: Boolean,
    leafAlias: Boolean,
    absorption: Absorption,
    keep: Boolean
  ): Unit =
    assert(page.participation.listedInDirectory == listed, page.toString)
    assert(page.listedInDirectory == listed, page.toString)
    assert(page.participation.leafIndex == leaf, page.toString)
    assert(page.leafIndex == leaf, page.toString)
    assert(page.participation.leafAlias == leafAlias, page.toString)
    assert(page.participation.absorption == absorption, page.toString)
    assert(page.keepUnauthoredFragment == keep, page.toString)

  // listedInDirectory, leafIndex, leafAlias, absorption, keepUnauthoredFragment
  private def expect(
    pages: List[Page],
    label: String,
    listed: Boolean,
    leaf: Boolean,
    leafAlias: Boolean,
    absorption: Absorption,
    keep: Boolean
  )(select: PartialFunction[Page, Page]): Unit =
    val found: List[Page] = pages.collect(select)
    assert(found.nonEmpty, label)
    found.foreach(page => row(page, listed, leaf, leafAlias, absorption, keep))

  test("participation matches today's page classes") {
    withSite: (site, target) =>
      val pages: List[Page] = site.pages.pages
      val defaults: PageParticipation = PageParticipation.default
      assert(!defaults.listedInDirectory && !defaults.leafIndex && !defaults.leafAlias)
      assert(defaults.absorption == Absorption.None)

      val directories: List[DirectoryPage] = pages.collect { case page: DirectoryPage => page }
      val oneList: Option[DirectoryPage] = directories.find(_.path.toString == "/other/index.html")
      val twoList: Option[DirectoryPage] = directories.find(_.path.toString == "/name/index.html")
      assert(oneList.isDefined, pages.map(_.path))
      assert(twoList.isDefined, pages.map(_.path))
      assert(oneList.get.doc.exists(_.asEntityLists.isDefined))
      assert(twoList.get.doc.exists(_.asEntityLists.isDefined))
      assert(directories.exists(_.path.toString == "/index.html"))
      directories.foreach: page =>
        row(page, true, false, false, Absorption.None, keep = page.path.toString == "/other/index.html")

      expect(pages, "SimpleMarkupPage", true, false, false, Absorption.None, false):
        case page: SimpleMarkupPage => page
      expect(pages, "ChunkedMarkupPage", true, false, false, Absorption.None, false):
        case page: ChunkedMarkupPage => page
      expect(pages, "PagedMarkupPage", true, false, false, Absorption.None, false):
        case page: PagedMarkupPage => page
      expect(pages, "YearIndexPage", false, true, false, Absorption.AnyMarkup, true):
        case page: YearIndexPage => page
      val stores: List[StoreIndexPage] = pages.collect { case page: StoreIndexPage => page }
      assert(stores.exists(_.kind == StoreIndexPage.Kind.Tree), stores.map(_.kind))
      assert(stores.exists(_.kind == StoreIndexPage.Kind.Flat), stores.map(_.kind))
      expect(pages, "StoreIndexPage", false, true, false, Absorption.AnyMarkup, false):
        case page: StoreIndexPage => page
      expect(pages, "MapPage", false, true, true, Absorption.MarkupProse, false):
        case page: MapPage => page
      expect(pages, "EntityListPage", true, false, false, Absorption.MarkupProse, true):
        case page: EntityListPage => page
      expect(pages, "FacsimilePage", false, false, false, Absorption.None, false):
        case page: FacsimilePage => page
      expect(pages, "SearchPage", false, true, true, Absorption.None, false):
        case page: SearchPage => page
      expect(pages, "Tags", true, false, false, Absorption.None, false):
        case page: Tags => page
      expect(pages, "Posts", true, false, false, Absorption.None, false):
        case page: Posts => page
      expect(pages, "GraphPage", true, false, false, Absorption.None, false):
        case page: GraphPage => page
      expect(pages, "Errors", true, false, false, Absorption.None, false):
        case page: Errors => page
      expect(pages, "Asset", true, false, false, Absorption.None, false):
        case page: Asset => page
      expect(pages, "AssetWithSourcePath", true, false, false, Absorption.None, false):
        case page: AssetWithSourcePath => page
      expect(pages, "SyntheticAsset", true, false, false, Absorption.None, false):
        case page: SyntheticAsset => page
      expect(pages, "EmbeddedAsset", true, false, false, Absorption.None, false):
        case page: EmbeddedAsset => page
      expect(pages, "SyntheticXmlAsset", true, false, false, Absorption.None, false):
        case page: SyntheticXmlAsset => page
      expect(pages, "Feed", true, false, false, Absorption.None, false):
        case page: Feed => page
      expect(pages, "SearchJson", true, false, false, Absorption.None, false):
        case page: SearchJson => page
      expect(pages, "GraphJson", true, false, false, Absorption.None, false):
        case page: GraphJson => page
      expect(pages, "Robots", true, false, false, Absorption.None, false):
        case page: Robots => page
      expect(pages, "Sitemap", true, false, false, Absorption.None, false):
        case page: Sitemap => page
      expect(pages, "Alias", true, false, false, Absorption.None, false):
        case page: Alias => page
      expect(pages, "TeiDocumentPage", false, false, false, Absorption.None, false):
        case page: TeiDocumentPage => page
      expect(pages, "PdfPage", false, false, false, Absorption.None, false):
        case page: PdfPage => page

      val leafAliases: List[Page] = pages.filter(_.participation.leafAlias)
      assert(leafAliases.length == 2, leafAliases.map(_.toString))
      assert(leafAliases.exists(_.isInstanceOf[MapPage]))
      assert(leafAliases.exists(_.isInstanceOf[SearchPage]))

      assert(pages.count(_.path.toString == "/search.html") == 1)
      // claimPage, not installPrefixAlias: the search page is added before the scan.
      val errors: String = Files.read(File(target, "errors.html"))
      assert(errors.contains("/search.md collides with /search.html"), errors)
      assert(!errors.contains("store index alias 'search'"), errors)
  }
