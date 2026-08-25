package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlDialect, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class CalloutSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def render(element: Xml.Element): String = HtmlXmlDialect.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def fromAsciiDoc(source: String): Xml.Element =
    AsciiDocMarkup.process(
      parse(AsciiDocMarkup.convert(source, File("t.adoc").getAbsoluteFile, asciidoctor)),
      PageErrorReporter.Silent
    )._1

  private def marks(xml: Xml.Element): Seq[Xml.Element] =
    XmlDialect.Plain.gather(xml, element => Option.when(Callout.isMark(element))(element)).toSeq

  private def lists(xml: Xml.Element): Seq[Xml.Element] =
    XmlDialect.Plain.gather(xml, element => Option.when(Callout.isList(element))(element)).toSeq

  test("AsciiDoc <1> markers and colist become callout IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """[source,ruby]
        |----
        |require 'sinatra' <1>
        |get '/hi' do <2>
        |  "Hello World!"
        |end
        |----
        |<1> Library import
        |<2> URL mapping
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = marks(xml)
    assert(found.size == 2, dumped)
    assert(found.map(_.getText) == Seq("1", "2"), dumped)
    assert(found.forall(_.get("data-value").isDefined), dumped)
    assert(!dumped.contains("""class="conum""""), dumped)
    assert(!dumped.contains("""class="colist""""), dumped)
    val foundLists: Seq[Xml.Element] = lists(xml)
    assert(foundLists.size == 1, dumped)
    val items: Seq[String] = foundLists.head.getChildren.flatMap(_.asElement).filter(_.getName == "li")
      .map(_.getText.trim).toSeq
    assert(items.exists(_.contains("Library import")), dumped)
    assert(items.exists(_.contains("URL mapping")), dumped)
  }

  test("callouts on a listing without [source] become callout IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """----
        |require 'sinatra' <1>
        |get '/hi' do <2>
        |  "Hello World!"
        |end
        |----
        |<1> Library import
        |<2> URL mapping
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = marks(xml)
    assert(found.size == 2, dumped)
    assert(found.map(_.getText) == Seq("1", "2"), dumped)
    assert(found.forall(_.get("data-value").isDefined), dumped)
    assert(!dumped.contains("""class="conum""""), dumped)
    assert(!dumped.contains("""class="colist""""), dumped)
    val foundLists: Seq[Xml.Element] = lists(xml)
    assert(foundLists.size == 1, dumped)
    val items: Seq[String] = foundLists.head.getChildren.flatMap(_.asElement).filter(_.getName == "li")
      .map(_.getText.trim).toSeq
    assert(items.exists(_.contains("Library import")), dumped)
    assert(items.exists(_.contains("URL mapping")), dumped)
  }

  test("AsciiDoc <.> automatic numbering becomes callout IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """----
        |first <.>
        |second <.>
        |----
        |<.> one
        |<.> two
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(marks(xml).map(_.getText) == Seq("1", "2"), dumped)
    assert(lists(xml).size == 1, dumped)
  }

  test("icons=font conum i+b soup becomes a single marker") {
    val xml: Xml.Element = AsciiDocMarkup.cleanup(parse(
      """<div><pre>line <i class="conum" data-value="1"></i><b>(1)</b></pre>
        |<div class="colist arabic"><table>
        |<tr><td><i class="conum" data-value="1"></i><b>1</b></td><td>note</td></tr>
        |</table></div></div>""".stripMargin
    ))
    val dumped: String = render(xml)
    assert(marks(xml).size == 1, dumped)
    assert(marks(xml).head.get("data-value").contains("1"), dumped)
    assert(!dumped.contains("""class="conum""""), dumped)
    assert(lists(xml).size == 1, dumped)
    assert(lists(xml).head.getChildren.flatMap(_.asElement).exists(_.getText.contains("note")), dumped)
  }

  test("HTML that is already IR is unchanged by HtmlMarkup.process") {
    val ir: Xml.Element = parse(
      """<div><pre>line <span class="callout" data-value="1">1</span></pre>
        |<ol class="callout-list"><li>note</li></ol></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(dumped.contains("""class="callout""""), dumped)
    assert(dumped.contains("""class="callout-list""""), dumped)
    assert(marks(processed).size == 1, dumped)
  }
