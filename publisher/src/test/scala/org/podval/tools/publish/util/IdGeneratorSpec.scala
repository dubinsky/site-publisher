package org.podval.tools.publish.util

import org.scalatest.funsuite.AnyFunSuite

final class IdGeneratorSpec extends AnyFunSuite:
  test("toId trims and replaces spaces") {
    assert(IdGenerator.toId("  a b  ") == "a-b")
  }
