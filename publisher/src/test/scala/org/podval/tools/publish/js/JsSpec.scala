package org.podval.tools.publish.js

import org.scalatest.funsuite.AnyFunSuite

final class JsSpec extends AnyFunSuite:
  test("MathJax adds single-dollar inline delimiters") {
    val config: String = MathJax.inlineJs.get
    assert(config.contains("[['$', '$']]"), config)
    assert(!config.contains("$$"), config)
  }

  test("quote wraps the value in double quotes") {
    assert(Js.quote("G-XXXX") == "\"G-XXXX\"")
    assert(Js.quote("https://cdn.example/x.mjs") == "\"https://cdn.example/x.mjs\"")
  }

  test("quote escapes quotes, ampersand, angle brackets, and controls") {
    val s: String = "\"'\\\n\r\t<>&\u2028\u2029\u0001\b\f"
    assert(
      Js.quote(s) ==
        "\"\\\"\\'\\\\\\n\\r\\t\\u003c\\u003e\\u0026\\u2028\\u2029\\u0001\\u0008\\u000c\""
    )
  }
