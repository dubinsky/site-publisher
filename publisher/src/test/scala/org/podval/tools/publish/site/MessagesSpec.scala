package org.podval.tools.publish.site

import org.podval.metadata.{Language, Name, Names, Term}
import org.scalatest.funsuite.AnyFunSuite

final class MessagesSpec extends AnyFunSuite:
  private val keys: Set[String] = Set(
    "nav.menu", "nav.search", "nav.settings", "nav.content", "nav.path",
    "settings.color", "settings.system", "settings.light", "settings.dark",
    "settings.glossary", "settings.editorial", "settings.transclusion", "settings.facsimile",
    "settings.add", "settings.del", "settings.see", "settings.plain", "settings.hidden",
    "settings.distinct.add", "settings.distinct.del", "settings.distinct.see",
    "format.transcription", "format.onePage", "format.chunked",
    "format.facsimileAlt", "format.facsimileTitle",
    "meta.published", "meta.updated", "meta.publishedTei", "meta.githubTip",
    "meta.cite", "meta.citeTip", "meta.transcriber", "meta.gitHash",
    "doc.description", "doc.date", "doc.who", "doc.to", "doc.transcriber", "doc.translation",
    "collection.counts", "collection.missingEmpty", "collection.missingText", "collection.untranscribed",
    "pb.missing", "pb.missingNamed", "pb.missingEmpty", "pb.facsimile", "pb.facsimileNamed", "pb.scanFailed",
    "search.all", "search.typeQuery", "search.none", "search.loadFailed", "search.overflow",
    "search.kind.collection",
    "index.title", "tree.title",
    "years.undated", "years.noMonth", "years.noArchive",
    "map.page", "map.title", "map.lines", "map.caption", "map.north", "map.noCases", "map.km",
    "map.place", "map.outside",
    "language", "year", "page", "facsimile",
    "graph.title", "graph.description", "graph.filter", "graph.filterLabel", "graph.orphans",
    "graph.loadFailed", "graph.status", "graph.large",
    "pages.posts", "pages.postsAll", "pages.tags", "pages.tagsAll", "pages.byTag",
    "pages.backlinks", "pages.embedded", "pages.category", "pages.newer", "pages.older",
    "pages.pagination", "pages.rss", "pages.sitemap",
    "date.calendar", "date.date", "date.from", "date.to", "date.notBefore", "date.notAfter",
    "date.julian", "date.gregorian", "date.jewish",
    "admonition.note", "admonition.tip", "admonition.warning", "admonition.important", "admonition.caution"
  )

  test("every message has English first, then Russian, and no Hebrew"):
    val messages: Map[String, Names] = Messages.messages
    assert(messages.keySet == keys)
    messages.foreach: (key, names) =>
      val langs: Seq[Option[Language]] = names.names.map(_.languageSpec.language)
      val expected: Seq[Option[Language]] =
        if key == "facsimile" then Seq(Some(Language.English), Some(Language.English), Some(Language.Russian))
        else Seq(Some(Language.English), Some(Language.Russian))
      assert(langs == expected, key)

  test("lookup falls back to English and fills braces"):
    given Language.Spec = Language.English.toSpec
    assert(Messages.text("nav.menu") == "Menu")
    assert(Messages.text("nav.menu", "extra" -> "ignored") == "Menu")
    assert(Messages.text("settings.editorial") == "Show expansions and corrections in the text")
    assert(Messages.text("settings.distinct.see") == "<Distinct>")
    assert(Messages.text("search.overflow", "n" -> 12) == "100 of 12")
    assert(Messages.text("map.km", "km" -> "3", "unused" -> "x") == "3 km")
    assert(Messages.text("search.kind.collection") == "collection")
    assert(Messages.text("nav.search")(using Language.Russian.toSpec) == "Поиск")
    assert(Messages.text("search.kind.collection")(using Language.Russian.toSpec) == "дело")
    assert(Messages.text("nav.menu")(using Language.Hebrew.toSpec) == "Menu")
    assert(Messages.messages.contains("doc.date"))
    assert(Messages.messages.contains("date.date"))
    assertThrows[IllegalArgumentException](Messages.text("search.overflow"))
    assertThrows[NoSuchElementException](Messages.text("no.such"))
    assertThrows[NoSuchElementException](Messages.text("search.archive"))

  test("a site case axis does not replace search.kind.collection") {
    val caseTerm: Term = Term(
      "case",
      Names(Seq(
        Name("case", Language.English.toSpec, plural = Some("cases")),
        Name("дело", Language.Russian.toSpec, plural = Some("дела"))
      )),
      selector = true
    )
    val merged = Terms.catalog.merge(Seq(caseTerm))
    assert(merged.forName("дело").exists(_.id == "case"))
    assert(merged.forName("collection").isEmpty)
    assert(Messages.text("search.kind.collection")(using Language.Russian.toSpec) == "дело")
    assert(Messages.text("search.kind.collection")(using Language.English.toSpec) == "collection")
  }
