package org.podval.store

import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  test("Selector.getForName") {
    val inventory: Selector = Selector.getForName("inventory")
    assert(inventory.names.hasName("inventory"))
    assert(Selector.getForName("names").title.contains("Имена"))
    assert(Selector.getForName("parsha").names.hasName("parsha"))
  }
