package org.podval.tools.publish.markup

import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  test("displayName prefers Russian; unknown selector is unchanged") {
    assert(Selector.displayName("category") == "разряд")
    assert(Selector.displayName("archive") == "архив")
    assert(Selector.displayName("document") == "документ")
    assert(Selector.displayName("case") == "дело")
    assert(Selector.displayName("item") == "item")
  }

  test("find matches any language name") {
    assert(Selector.find("разряд").isDefined)
    assert(Selector.find("category").isDefined)
    assert(Selector.find("book").isDefined)
    assert(Selector.find("книга").isDefined)
    assert(Selector.find("item").isEmpty)
  }
