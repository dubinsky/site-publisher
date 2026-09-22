package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlAttribute, XmlElement}
import Xml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class WikiLinkSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    HtmlXmlWriterConfig.render(Xml.element(XmlElement.P).setChildren(Chunk(element)))

  private def embedImage(ref: String, title: Option[String]): Xml.Element =
    val a: Xml.Element = WikiLink.make(transclude = true, ref, title)
    WikiLink.embed(a, ref).get

  test("embed image without size has no width or height") {
    val img: Xml.Element = embedImage("pixel.svg", None)
    val dumped: String = render(img)
    assert(img.isElement(XmlElement.Img), dumped)
    assert(img.get(XmlAttribute.Src).contains("pixel.svg"), dumped)
    assert(img.get("width").isEmpty, dumped)
    assert(img.get("height").isEmpty, dumped)
  }

  test("embed image |WIDTH sets img width") {
    val img: Xml.Element = embedImage("pixel.svg", Some("320"))
    val dumped: String = render(img)
    assert(img.get(XmlAttribute.Src).contains("pixel.svg"), dumped)
    assert(img.get("width").contains("320"), dumped)
    assert(img.get("height").isEmpty, dumped)
  }

  test("embed image |WIDTHxHEIGHT sets img width and height") {
    val img: Xml.Element = embedImage("pixel.svg", Some("320x240"))
    val dumped: String = render(img)
    assert(img.get("width").contains("320"), dumped)
    assert(img.get("height").contains("240"), dumped)
  }
