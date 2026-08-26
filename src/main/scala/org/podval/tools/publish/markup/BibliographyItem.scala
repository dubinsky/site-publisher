package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

/** Native in-document bibliography entries (`class="bibliography-item"` + authored id).
  * Not citeproc `csl-entry`. Dialects convert native lists into this IR. */
object BibliographyItem:
  object ItemClass extends HtmlClass("bibliography-item")

  val tip: Tip = Tip("citation")

  def isList(element: Xml.Element): Boolean =
    element.has(Citation.ListClass) && !Citation.isList(element)

  def isItem(element: Xml.Element): Boolean = element.has(ItemClass)

  def definitions(xml: Xml.Element): Map[String, Xml.Nodes] =
    xml.gather(element =>
      for
        id <- element.getId.filter(_.nonEmpty)
        if isItem(element)
      yield id -> element.getChildren.filterNot(_.isWhitespace)
    ).toMap
