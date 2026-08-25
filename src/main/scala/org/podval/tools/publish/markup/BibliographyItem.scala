package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

/** Native in-document bibliography entries (TEI `listBibl` / `bibl`, AsciiDoc
  * `[bibliography]` / `[[[id]]]`). Not citeproc `csl-entry`. */
object BibliographyItem:
  object ItemClass extends HtmlClass("bibliography-item")

  val tip: Tip = Tip("citation")

  def isList(element: Xml.Element): Boolean =
    element.has(Citation.ListClass) && !Citation.isList(element)

  def isItem(element: Xml.Element): Boolean = element.has(ItemClass)

  def isEntryName(name: String): Boolean =
    name == "bibl" || name == "biblStruct" || name == "biblFull"

  /** Mark an entry and hoist a leading empty `<a id>` (AsciiDoc `[[[id]]]`). */
  def asItem(element: Xml.Element): Xml.Element =
    val (fromAnchor, stripped): (Option[String], Xml.Nodes) = hoistEmptyIdAnchor(element.getChildren)
    val id: Option[String] = fromAnchor.orElse(element.getId.filter(_.nonEmpty))
    id.fold(element): value =>
      element.add(ItemClass).setId(value).setChildren(stripped)

  def definitions(xml: Xml.Element): Map[String, Xml.Nodes] =
    xml.gather(element =>
      for
        id <- element.getId.filter(_.nonEmpty)
        if isItem(element)
      yield id -> element.getChildren.filterNot(_.isWhitespace)
    ).toMap

  private def hoistEmptyIdAnchor(nodes: Xml.Nodes): (Option[String], Xml.Nodes) =
    val (leading, rest): (Xml.Nodes, Xml.Nodes) = nodes.span(node =>
      node.isWhitespace || node.asElement.exists(isEmptyIdAnchor)
    )
    val fromAnchor: Option[String] = leading.flatMap(_.asElement).flatMap(_.getId).headOption
    if fromAnchor.isDefined then (fromAnchor, rest)
    else
      var found: Option[String] = None
      val walked: Xml.Nodes = nodes.map: node =>
        if found.isDefined then node
        else node.asElement.filter(el => el.getName == "p" || el.getName == "span") match
          case Some(wrapper) =>
            val (inner, innerNodes): (Option[String], Xml.Nodes) = hoistEmptyIdAnchor(wrapper.getChildren)
            found = inner
            if inner.isDefined then wrapper.setChildren(innerNodes) else node
          case None =>
            node
      (found, walked)

  private def isEmptyIdAnchor(element: Xml.Element): Boolean =
    element.isA &&
    element.getId.nonEmpty &&
    element.getHref.isEmpty &&
    element.getChildren.forall(_.isWhitespace)
