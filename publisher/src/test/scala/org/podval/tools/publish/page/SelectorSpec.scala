package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  test("selectorDisplayName follows Language.Spec; unknown selector is unchanged") {
    val ru: Language.Spec = Language.Russian.toSpec
    val en: Language.Spec = Language.English.toSpec
    assert(PageHeader.selectorDisplayName("category", ru) == "разряд")
    assert(PageHeader.selectorDisplayName("archive", ru) == "архив")
    assert(PageHeader.selectorDisplayName("document", ru) == "документ")
    assert(PageHeader.selectorDisplayName("case", ru) == "дело")
    assert(PageHeader.selectorDisplayName("category", en) == "category")
    assert(PageHeader.selectorDisplayName("archive", en) == "archive")
    assert(PageHeader.selectorDisplayName("document", en) == "document")
    assert(PageHeader.selectorDisplayName("case", en) == "case")
    assert(PageHeader.selectorDisplayName("item", ru) == "item")
    assert(PageHeader.selectorDisplayName("item", en) == "item")
    assert(Selectors.getForName("archive").plural.exists(_.hasName("Archives")))
    assert(Selectors.getForName("archive").plural.exists(_.hasName("Архивы")))
    assert(Selectors.getForName("case").plural.exists(_.hasName("Cases")))
    assert(Selectors.getForName("name").plural.exists(_.hasName("Names")))
    assert(Selectors.getForName("name").plural.exists(_.hasName("Имена")))
    assert(Selectors.forName("names").isEmpty)
    assert(Selectors.forName("имена").isEmpty)
  }

  test("forName matches any language name") {
    assert(Selectors.forName("разряд").isDefined)
    assert(Selectors.forName("category").isDefined)
    assert(Selectors.forName("book").isDefined)
    assert(Selectors.forName("книга").isDefined)
    assert(Selectors.forName("item").isEmpty)
  }
