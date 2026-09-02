package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class VideoSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def render(element: Xml.Element): String = HtmlXmlWriterConfig.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def fromAsciiDoc(source: String): Xml.Element =
    AsciiDocMarkup.process(
      parse(AsciiDocMarkup.convert(source, File("t.adoc").getAbsoluteFile, asciidoctor)),
      PageErrorReporter.Silent
    )._1

  private def fromMarkdown(source: String): Xml.Element =
    MarkdownMarkup.process(
      parse(MarkdownMarkup.xmlContent(source, File("t.md"))),
      PageErrorReporter.Silent
    )._1

  private def fromDocBook(source: String): Xml.Element =
    DocBookMarkup.process(parse(source), PageErrorReporter.Silent)._1

  private def videos(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Video.is(element))(element)).toSeq

  private def embeds(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Video.isEmbed(element))(element)).toSeq

  test("AsciiDoc video:: local file becomes video IR") {
    val xml: Xml.Element = fromAsciiDoc("video::clip.mp4[]\n")
    val dumped: String = render(xml)
    assert(videos(xml).size == 1, dumped)
    assert(dumped.contains("clip.mp4"), dumped)
    assert(dumped.contains("controls"), dumped)
    assert(!dumped.contains("videoblock"), dumped)
  }

  test("AsciiDoc video:: with title wraps in a figure") {
    val xml: Xml.Element = fromAsciiDoc(
      """.A clip
        |video::clip.mp4[]
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(videos(xml).size == 1, dumped)
    assert(xml.gather(el => Option.when(Figure.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("A clip"), dumped)
    assert(!dumped.contains("videoblock"), dumped)
  }

  test("AsciiDoc youtube videoblock becomes iframe.video-embed") {
    val xml: Xml.Element = fromAsciiDoc("video::dQw4w9WgXcQ[youtube]\n")
    val dumped: String = render(xml)
    assert(embeds(xml).size == 1, dumped)
    assert(dumped.contains("youtube.com/embed"), dumped)
    assert(!dumped.contains("videoblock"), dumped)
    assert(videos(xml).isEmpty, dumped)
  }

  test("WikiLink.embed of a video transclusion") {
    val a: Xml.Element = Xml
      .element("a")
      .addClass("wiki-link")
      .addClass("transclude")
      .setHref("clip.mp4")
      .setText("![[clip.mp4]]")
    val embedded: Xml.Element = WikiLink.embed(a, "clip.mp4").get
    val dumped: String = render(embedded)
    assert(Video.is(embedded), dumped)
    assert(dumped.contains("""src="clip.mp4""""), dumped)
    assert(dumped.contains("Open video"), dumped)
    assert(dumped.contains("clip.mp4"), dumped)
  }

  test("WikiLink.embed uses alias as label") {
    val a: Xml.Element = Xml
      .element("a")
      .addClass("wiki-link")
      .addClass("transclude")
      .setHref("clip.mp4")
      .setText("![[Demo]]")
    val embedded: Xml.Element = WikiLink.embed(a, "clip.mp4").get
    val dumped: String = render(embedded).replaceAll("\\s+", " ")
    assert(dumped.contains("Open video: Demo"), dumped)
  }

  test("HTML video gets class video and controls") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse("""<div><video src="clip.mp4"></video></div>""")
    )
    val dumped: String = render(xml)
    assert(videos(xml).size == 1, dumped)
    assert(dumped.contains("controls"), dumped)
    assert(dumped.contains("Open video"), dumped)
  }

  test("HTML youtube iframe gets class video-embed") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse("""<div><iframe src="https://www.youtube.com/embed/dQw4w9WgXcQ"></iframe></div>""")
    )
    val dumped: String = render(xml)
    assert(embeds(xml).size == 1, dumped)
  }

  test("unrelated iframe is not a video embed") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse("""<div><iframe src="https://example.com/"></iframe></div>""")
    )
    val dumped: String = render(xml)
    assert(embeds(xml).isEmpty, dumped)
    assert(dumped.contains("example.com"), dumped)
  }

  test("Markdown wiki embed stub is a transclude link until PageContent") {
    val xml: Xml.Element = fromMarkdown("See ![[clip.mp4]] here.\n")
    val dumped: String = render(xml)
    assert(videos(xml).isEmpty, dumped)
    assert(dumped.contains("wiki-link"), dumped)
    assert(dumped.contains("transclude"), dumped)
    assert(dumped.contains("clip.mp4"), dumped)
  }

  test("DocBook videodata becomes video IR") {
    val xml: Xml.Element = fromDocBook(
      """<article>
        |<mediaobject><videoobject><videodata fileref="clip.mp4"/></videoobject></mediaobject>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(videos(xml).size == 1, dumped)
    assert(dumped.contains("clip.mp4"), dumped)
    assert(dumped.contains("controls"), dumped)
    assert(!dumped.contains("<videodata"), dumped)
  }

  test("DocBook videodata YouTube URL becomes iframe.video-embed") {
    val xml: Xml.Element = fromDocBook(
      """<article>
        |<mediaobject><videoobject>
        |  <videodata fileref="https://www.youtube.com/embed/dQw4w9WgXcQ"/>
        |</videoobject></mediaobject>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(embeds(xml).size == 1, dumped)
    assert(dumped.contains("youtube.com/embed"), dumped)
    assert(videos(xml).isEmpty, dumped)
  }
