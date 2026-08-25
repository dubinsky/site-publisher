package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class TaskListSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def render(element: Xml.Element): String = HtmlXmlDialect.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def fromMarkdown(source: String): Xml.Element =
    MarkdownMarkup.process(
      parse(MarkdownMarkup.xmlContent(source, File("t.md"))),
      PageErrorReporter.Silent
    )._1

  private def fromAsciiDoc(source: String): Xml.Element =
    AsciiDocMarkup.process(
      parse(AsciiDocMarkup.convert(source, File("t.adoc").getAbsoluteFile, asciidoctor)),
      PageErrorReporter.Silent
    )._1

  private def assertTaskList(xml: Xml.Element, dumped: String): Unit =
    assert(dumped.contains("""class="task-list""""), dumped)
    assert(dumped.contains("""class="task-list-item""""), dumped)
    assert(dumped.contains("task-list-item-checkbox"), dumped)
    assert(dumped.contains("""type="checkbox""""), dumped)
    assert(dumped.contains("""disabled="disabled""""), dumped)
    assert(!dumped.contains("""class="checklist""""), dumped)
    val items: Seq[Xml.Element] = xml.getChildren.flatMap(_.asElement)
      .filter(el => el.getName == "ul" || el.getName == "ol")
      .flatMap(_.getChildren.flatMap(_.asElement).filter(_.has(TaskList.ItemClass)))
      .toSeq
    assert(items.size == 2, dumped)
    val boxes: Seq[Xml.Element] = items.flatMap(_.getChildren.flatMap(_.asElement).filter(_.has(TaskList.CheckboxClass)))
    assert(boxes.size == 2, dumped)
    assert(boxes.head.get("checked").isEmpty, dumped)
    assert(boxes(1).get("checked").contains("checked"), dumped)

  test("Markdown - [ ] / - [x] become task-list IR") {
    val xml: Xml.Element = fromMarkdown(
      """- [ ] open
        |- [x] done
        |""".stripMargin
    )
    assertTaskList(xml, render(xml))
  }

  test("AsciiDoc * [ ] / * [x] become task-list IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """* [ ] open
        |* [x] done
        |""".stripMargin
    )
    assertTaskList(xml, render(xml))
  }

  test("Markdown convert is identity on IR") {
    val xml: Xml.Element = fromMarkdown("- [x] done\n")
    val again: Xml.Element = MarkdownMarkup.convert(xml)
    assert(render(again) == render(xml))
  }

  test("Markdown mixed task and plain items") {
    val xml: Xml.Element = fromMarkdown(
      """- [ ] open
        |- plain
        |- [x] done
        |""".stripMargin
    )
    assertMixed(xml, render(xml))
  }

  test("AsciiDoc mixed task and plain items") {
    val xml: Xml.Element = fromAsciiDoc(
      """* [ ] open
        |* plain
        |* [x] done
        |""".stripMargin
    )
    assertMixed(xml, render(xml))
  }

  private def assertMixed(xml: Xml.Element, dumped: String): Unit =
    val lists: Seq[Xml.Element] = HtmlXmlDialect.gather(xml, element =>
      Option.when(element.getName == "ul" || element.getName == "ol")(element)
    ).toSeq
    val list: Xml.Element = lists.find(_.has(TaskList.ListClass)).getOrElse:
      throw new AssertionError(s"no task-list: $dumped")
    val lis: Seq[Xml.Element] = list.getChildren.flatMap(_.asElement).filter(_.getName == "li").toSeq
    assert(lis.size == 3, dumped)
    assert(lis(0).has(TaskList.ItemClass), dumped)
    assert(!lis(1).has(TaskList.ItemClass), dumped)
    assert(lis(2).has(TaskList.ItemClass), dumped)
    assert(lis(1).getText.contains("plain"), dumped)
    val boxes: Seq[Xml.Element] = lis.flatMap(_.getChildren.flatMap(_.asElement).filter(_.has(TaskList.CheckboxClass)))
    assert(boxes.size == 2, dumped)
    assert(boxes.head.get("checked").isEmpty, dumped)
    assert(boxes(1).get("checked").contains("checked"), dumped)

  test("HTML that is already IR is unchanged by HtmlMarkup.process") {
    val ir: Xml.Element = parse(
      """<div><ul class="task-list"><li class="task-list-item"><input type="checkbox" class="task-list-item-checkbox" disabled="disabled" checked="checked"/>done</li></ul></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(dumped.contains("""class="task-list""""), dumped)
    assert(dumped.contains("""class="task-list-item""""), dumped)
    assert(dumped.contains("task-list-item-checkbox"), dumped)
    assert(dumped.contains("""checked="checked""""), dumped)
  }
