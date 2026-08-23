package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml, XmlDialect}

// Note: written by Grok ;)
object Glossary:
  object ItemClass extends HtmlClass("dlist-item")

  val tip: Tip = Tip("glossary")

  def definitions(
    xml: Xml.Element,
    xmlDialect: XmlDialect
  ): Map[String, Xml.Nodes] =
    xmlDialect.gatherWithContext(
      xml,
      // TODO this looks like an AsciiDoc-specific class;
      // I should define - and convert to - a markup-independent internal representation for glossary...
      isContext = _.hasClass("glossary"),
      gatherElement = (element, glossary) =>
        for
          _ <- glossary
          id <- element.getId
          if element.has(ItemClass)
          dd <- element.getChildren.flatMap(_.asElement).find(_.getName == "dd")
          children = dd.getChildren.filterNot(_.isWhitespace)
          if children.nonEmpty
        yield id -> children
    ).toMap
