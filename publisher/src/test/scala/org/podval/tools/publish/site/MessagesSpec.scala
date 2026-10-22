package org.podval.tools.publish.site

import org.podval.metadata.{Language, Name, Names, Term}
import org.scalatest.funsuite.AnyFunSuite

final class MessagesSpec extends AnyFunSuite:
  private val keys: Set[String] = Set(
    "nav.menu", "nav.search", "nav.settings", "nav.content", "nav.path",
    "settings.color", "settings.system", "settings.light", "settings.dark",
    "settings.in-text", "settings.glossary", "settings.editorial", "settings.transclusion", "settings.facsimile",
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
    "graph.loadFailed", "graph.node", "graph.edge", "graph.status", "graph.large",
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
        else if key == "graph.status" then Seq(Some(Language.English))
        else Seq(Some(Language.English), Some(Language.Russian))
      assert(langs == expected, key)

  test("lookup falls back to English and fills braces"):
    given Language.Spec = Language.English.toSpec
    assert(Messages.text("nav.menu") == "Menu")
    assert(Messages.text("nav.menu", "extra" -> "ignored") == "Menu")
    assert(Messages.text("settings.in-text") == "In the text")
    assert(Messages.text("settings.glossary") == "Term definitions and citations")
    assert(Messages.text("settings.editorial") == "Abbreviation expansions and corrections")
    assert(Messages.text("settings.distinct.add") == "[Marked]")
    assert(Messages.text("settings.distinct.del") == "Marked")
    assert(Messages.text("settings.distinct.see") == "<Marked>")
    assert(Messages.text("settings.in-text")(using Language.Russian.toSpec) == "В тексте")
    assert(Messages.text("settings.glossary")(using Language.Russian.toSpec) == "Определения терминов и ссылки на литературу")
    assert(Messages.text("settings.editorial")(using Language.Russian.toSpec) == "Раскрытия сокращений и исправления")
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

  test("document cardinal and the collection count line"):
    val en: Language.Spec = Language.English.toSpec
    val ru: Language.Spec = Language.Russian.toSpec
    val rows: Seq[(Int, String, String, String, String)] = Seq(
      (0, "documents", "документов", "Documents: 0.", "Документов 0."),
      (1, "document", "документ", "Document: 1.", "Документ 1."),
      (2, "documents", "документа", "Documents: 2.", "Документа 2."),
      (5, "documents", "документов", "Documents: 5.", "Документов 5."),
      (11, "documents", "документов", "Documents: 11.", "Документов 11."),
      (21, "documents", "документ", "Documents: 21.", "Документ 21."),
      (22, "documents", "документа", "Documents: 22.", "Документа 22.")
    )
    def line(n: Int)(using spec: Language.Spec): String =
      val word: String = Terms.catalog.cardinal("document", n)
      val shown: String = spec.language.get.sentenceCase(word)
      Messages.text("collection.counts", "documents" -> shown, "n" -> n)
    rows.foreach: (n, enWord, ruWord, enLine, ruLine) =>
      assert(Terms.catalog.cardinal("document", n)(using en) == enWord, n)
      assert(Terms.catalog.cardinal("document", n)(using ru) == ruWord, n)
      assert(line(n)(using en) == enLine, n)
      assert(line(n)(using ru) == ruLine, n)

  test("graph status keeps the count and form holes"):
    val expected: String = "{n} {nodes}, {e} {edges}"
    def filled(using Language.Spec): String = Messages.text(
      "graph.status",
      "n" -> "{n}",
      "e" -> "{e}",
      "nodes" -> "{nodes}",
      "edges" -> "{edges}"
    )
    assert(filled(using Language.English.toSpec) == expected)
    assert(filled(using Language.Russian.toSpec) == expected)
