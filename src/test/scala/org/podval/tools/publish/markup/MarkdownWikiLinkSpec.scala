package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class MarkdownWikiLinkSpec extends AnyFunSuite:
  private def convert(text: String): Xml.Nodes =
    MarkdownWikiLink.convert(Chunk.empty, text)

  private def render(nodes: Xml.Nodes): String =
    HtmlXmlDialect.render(Xml.element("p").setChildren(nodes))

  private def links(nodes: Xml.Nodes): Seq[Xml.Element] =
    nodes.flatMap(_.asElement).filter(_.isA).toSeq

  test("[[notes]] becomes a wiki-link with href notes") {
    val nodes: Xml.Nodes = convert("see [[notes]] here")
    val dumped: String = render(nodes)
    val found: Seq[Xml.Element] = links(nodes)
    assert(found.size == 1, dumped)
    assert(found.head.getHref.contains("notes"), dumped)
    assert(found.head.hasClass("wiki-link"), dumped)
    assert(!WikiLink.isTranscluded(found.head), dumped)
    assert(dumped.contains("see "), dumped)
    assert(dumped.contains(" here"), dumped)
    assert(dumped.contains("[[notes]]"), dumped)
  }

  test("[[notes|Notes]] uses the alias as link text and notes as href") {
    val nodes: Xml.Nodes = convert("[[notes|Notes]]")
    val dumped: String = render(nodes)
    val found: Seq[Xml.Element] = links(nodes)
    assert(found.size == 1, dumped)
    assert(found.head.getHref.contains("notes"), dumped)
    assert(found.head.getText == "[[Notes]]", dumped)
  }

  test("[[#id]] is an intrapage href") {
    val found: Seq[Xml.Element] = links(convert("[[#posuk]]"))
    assert(found.size == 1)
    assert(found.head.getHref.contains("#posuk"))
  }

  test("![[clip.mp4]] is a transclude wiki-link") {
    val found: Seq[Xml.Element] = links(convert("see ![[clip.mp4]]"))
    assert(found.size == 1)
    assert(WikiLink.isTranscluded(found.head))
    assert(found.head.getHref.contains("clip.mp4"))
    assert(found.head.getText == "![[clip.mp4]]")
  }

  test("several wiki links in one string") {
    val found: Seq[Xml.Element] = links(convert("[[a]] and [[b|Bee]]"))
    assert(found.map(_.getHref) == Seq(Some("a"), Some("b")))
    assert(found(1).getText == "[[Bee]]")
  }

  test("unclosed [[ stays text") {
    val nodes: Xml.Nodes = convert("see [[notes")
    assert(links(nodes).isEmpty)
    assert(render(nodes).contains("[[notes"))
  }

  test("empty alias [[notes|]] uses the ref as text") {
    val found: Seq[Xml.Element] = links(convert("[[notes|]]"))
    assert(found.size == 1)
    assert(found.head.getHref.contains("notes"))
    assert(found.head.getText == "[[notes]]")
  }
