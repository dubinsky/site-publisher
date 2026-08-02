package org.podval.tools.publish.asciidoc

import org.podval.tools.publish.markup.HtmlMarkup
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

// TODO simplify
// Distill the soup of meaningless `div`s that Asciidoctor emits;
// see, for example, https://tiffnix.com/soupault#html-de-uglifier-plugin.
object AsciiDocDivSoup:
  def cleanUp(element: Xml.Element): Xml.Element =
    removeSpuriousClasses(element).setChildren(removeSpuriousElements(element.getChildren))

  private def removeSpuriousClasses(element: Xml.Element): Xml.Element =
    val classes = element.getClasses
    if classes.isEmpty then element else
      element.setClasses(classes.filterNot(spuriousClasses.contains))

  private def removeSpuriousElements(children: Xml.Nodes): Xml.Nodes = children.flatMap: child =>
    val replacement: Option[Xml.Nodes] = child.asElement.flatMap: element =>
      if
        // Remove spurious 'div's.
        element.getName == "div" && (
          element
            .getClasses
            .exists(cls => spuriousDivClasses.contains(cls))
          ||
          element
            .getChildren
            .flatMap(_.asElement)
            .headOption
            .flatMap(HtmlMarkup.headerLevel)
            .exists(headerLevel => element.hasClass(s"sect${headerLevel - 1}"))
          )
      then
        Some(removeSpuriousElements(element.getChildren))
      else if element.getName == "td" || element.getName == "li" then
        // Remove 'p's in 'td's and 'li's.
        val (init, tail) = element.getChildren.span(_.asElement.isEmpty)
        for
          head <- tail.headOption.map(_.asElement.get)
          if head.getName == "p"
        yield
          Chunk(element.setChildren(init ++ head.getChildren ++ tail.tail))
      else
        None

    replacement match
      case None =>
        Chunk(child)
      case Some(result) =>
        result

  private val spuriousDivClasses: Set[String] = Set(
    "paragraph",
    "sectionbody",
    "ulist",
    "olist",
    "quoteblock",
    "openblock",
    "content"
  )

  private val spuriousClasses: Set[String] = Set(
    "tableblock",
    "halign-left",
    "valign-top",
    "frame-all",
    "grid-all",
    "fit-content",
    "stretch"
  )
