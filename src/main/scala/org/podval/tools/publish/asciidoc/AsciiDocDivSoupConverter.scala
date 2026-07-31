package org.podval.tools.publish.asciidoc

import org.podval.tools.publish.html.HtmlSection
import org.podval.tools.publish.markup.Converter
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// Distill the soup of meaningless `div`s that Asciidoctor emits;
// see, for example, https://tiffnix.com/soupault#html-de-uglifier-plugin.
final class AsciiDocDivSoupConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =

    Some(removeSpuriousClasses(element).setChildren(removeSpuriousElements(element.getChildren)))

  private def removeSpuriousClasses(element: Xml.Element): Xml.Element =
    val classes = element.getClasses
    if classes.isEmpty then element else
      element.setClasses(classes.filterNot(AsciiDocDivSoupConverter.spuriousClasses.contains))

  @tailrec
  private def removeSpuriousElements(children: Xml.Nodes): Xml.Nodes =
    var changed: Boolean = false
    val result: Xml.Nodes = children.flatMap: child =>
      // TODO redo with Option/.getOrElse(Chunk(child))
      val replacement: Option[Xml.Nodes] = child.asElement.flatMap: element =>
        if
          // Remove spurious 'div's.
          element.getName == "div" && (
            element
              .getClasses
              .exists(cls => AsciiDocDivSoupConverter.spuriousDivClasses.contains(cls))
              ||
              element
                .getChildren
                .flatMap(_.asElement)
                .headOption
                .flatMap(HtmlSection.headerLevel)
                .exists(headerLevel => element.hasClass(s"sect${headerLevel - 1}"))
            )
        then
          Some(element.getChildren)
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
          changed = true
          result

    if !changed
    then result
    else removeSpuriousElements(result)

  object AsciiDocDivSoupConverter:
    val spuriousDivClasses: Set[String] = Set(
      "paragraph",
      "sectionbody",
      "ulist",
      "olist",
      "quoteblock",
      "openblock",
      "content"
    )

    val spuriousClasses: Set[String] = Set(
      "tableblock",
      "halign-left",
      "valign-top",
      "frame-all",
      "grid-all",
      "fit-content",
      "stretch"
    )
