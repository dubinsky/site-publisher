package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

/** TEI-native bibliography entries (`listBibl` / `bibl`). Not citeproc `csl-entry`. */
object BibliographyItem:
  object ItemClass extends HtmlClass("bibliography-item")

  val tip: Tip = Tip("citation")

  def isList(element: Xml.Element): Boolean =
    element.getName == "listBibl" && element.has(Citation.ListClass)

  def isItem(element: Xml.Element): Boolean = element.has(ItemClass)

  def isEntryName(name: String): Boolean =
    name == "bibl" || name == "biblStruct" || name == "biblFull"

  def definitions(xml: Xml.Element): Map[String, Xml.Nodes] =
    xml.gather(element =>
      for
        id <- element.getId.filter(_.nonEmpty)
        if isItem(element)
      yield id -> element.getChildren.filterNot(_.isWhitespace)
    ).toMap
