package org.podval.tools.publish.js

import org.scalatest.funsuite.AnyFunSuite

final class JsSpec extends AnyFunSuite:
  test("js interpolator quotes a String hole") {
    val url: String = "https://cdn.example/x.mjs"
    assert(js"import x from $url".value == """import x from "https://cdn.example/x.mjs"""")
  }

  test("gtag config interpolates the id as a quoted string without extra quotes") {
    val id: String = "G-XXXX"
    assert(js"gtag('config', $id)".value == """gtag('config', "G-XXXX")""")
  }

  test("Js hole is spliced raw") {
    val inner: Js = js"1 + 1"
    assert(js"return $inner;".value == "return 1 + 1;")
  }

  test("String holes escape quotes, ampersand, angle brackets, and controls") {
    val s: String = "\"'\\\n\r\t<>&\u2028\u2029\u0001\b\f"
    assert(
      js"$s".value ==
        "\"\\\"\\'\\\\\\n\\r\\t\\u003c\\u003e\\u0026\\u2028\\u2029\\u0001\\u0008\\u000c\""
    )
  }
