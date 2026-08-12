package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.html.*

final class HtmlAttributesSpec extends AnyFunSuite:
  test("className += values merge into a single class attribute for XmlWriter") {
    val el: Html.Element = span(
      className += "icon-span",
      className += "grey fa-classic fa-regular",
      className += "fa-file"
    )
    val attrs = el.getAttributes
    assert(attrs.count(_._1 == "class") == 1)
    assert(attrs.find(_._1 == "class").map(_._2).contains("icon-span grey fa-classic fa-regular fa-file"))
    val rendered = HtmlXmlDialect.render(el)
    assert(rendered.contains("""class="icon-span grey fa-classic fa-regular fa-file""""))
    assert(!rendered.matches("""(?s).*class="[^"]*".*class=".*"""))
  }

  test(":= base then += appends for any attribute name") {
    val el = div(className := "base", className += "extra")
    assert(el.getAttributes == zio.blocks.chunk.Chunk(("class", "base extra")))
  }

  test("duplicate KeyValue last wins without AppendValue") {
    val el = div(className := "a", className := "b")
    assert(el.getAttributes == zio.blocks.chunk.Chunk(("class", "b")))
  }
