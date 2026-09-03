package org.podval.tools.publish.page

import org.podval.store.Selector
import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  test("selectorDisplayName prefers Russian; unknown selector is unchanged") {
    assert(PageHeader.selectorDisplayName("category") == "разряд")
    assert(PageHeader.selectorDisplayName("archive") == "архив")
    assert(PageHeader.selectorDisplayName("document") == "документ")
    assert(PageHeader.selectorDisplayName("case") == "дело")
    assert(PageHeader.selectorDisplayName("item") == "item")
  }

  test("forName matches any language name") {
    assert(Selector.forName("разряд").isDefined)
    assert(Selector.forName("category").isDefined)
    assert(Selector.forName("book").isDefined)
    assert(Selector.forName("книга").isDefined)
    assert(Selector.forName("item").isEmpty)
  }
