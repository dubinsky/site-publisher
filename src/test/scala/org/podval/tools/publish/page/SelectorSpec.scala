package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.store.Selector
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
  }

  test("forName matches any language name") {
    assert(Selector.forName("разряд").isDefined)
    assert(Selector.forName("category").isDefined)
    assert(Selector.forName("book").isDefined)
    assert(Selector.forName("книга").isDefined)
    assert(Selector.forName("item").isEmpty)
  }
