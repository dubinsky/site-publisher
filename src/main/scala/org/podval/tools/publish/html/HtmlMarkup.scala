package org.podval.tools.publish.html

import org.podval.tools.publish.markup.{Markup, Processor}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlXmlDialect, Xml}
import java.io.File

object HtmlMarkup extends Markup(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  override def xmlContent(content: String, sourceFile: File): String =
    // Wrap HTML in a 'div' to accommodate multi-root documents.
    s"<div>$content</div>"

  override def process(
    source: PageSource,
    ids: IdGenerator,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) = process(
    source,
    ids,
    xml,
    Seq.empty
  )
    
  def process(
    source: PageSource,
    ids: IdGenerator,
    xml: Xml.Element,
    processors: Seq[Processor]
  ): (Xml.Element, Option[Xml.Element]) =
    val xmlProcessed: Xml.Element = Processor.process(source.xmlDialect, xml, processors)
    val (xmlAfterTitle: Xml.Element, title: Option[Xml.Element]) = retrieveTitle(xmlProcessed)
    // Nest HTML sections once the title ('h1') is removed.
    val xmlSectioned: Xml.Element = Processor.process(source.xmlDialect, xmlAfterTitle, Seq(
      HtmlSectionsTransformer(ids)
    ))
    (xmlSectioned, title)
  
  private def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = xml
    .getChildren
    .flatMap(_.asElement)
    .find(element => headerLevel(element).contains(1))
    .fold((xml, None)): h1 =>
      (xml.setChildren(xml.getChildren.filterNot(_ eq h1)), Some(h1))

  def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = element.getName
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None
