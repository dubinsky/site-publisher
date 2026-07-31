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

  override def processors(
    ids: IdGenerator,
    source: PageSource
  ): Seq[Processor] = Seq(
    HtmlSectionsTransformer(ids)
  )

  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = (xml, None)

  //  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = xml
//    .getChildren
//    .flatMap(_.asElement)
//    .find(element => headerLevel(element).contains(1))
//    .fold((xml, None)): h1 =>
//      (xml.setChildren(xml.getChildren.filterNot(_ eq h1)), Some(h1))

  def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = element.getName
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None
