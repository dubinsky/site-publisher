package org.podval.tools.publish.markup

import org.podval.tools.publish.util.Strings
import org.podval.xml.{HtmlClass, Xml, XmlAttribute, XmlElement}
import zio.blocks.chunk.Chunk

/** Markup-neutral PDF embed IR. CSS styles only these classes.
  * Wrapper `div.pdf-embed`: `<object type="application/pdf">` (inner fallback link)
  * plus a sibling `p.pdf-embed-link` that survives print. */
object PdfEmbed:
  object Class extends HtmlClass("pdf-embed")
  object LinkClass extends HtmlClass("pdf-embed-link")

  private val HeightVar: String = "--pdf-embed-height"
  private val PdfType: String = "application/pdf"

  def is(element: Xml.Element): Boolean =
    element.getName == "div" && element.has(Class)

  def fromRef(ref: String, label: String): Xml.Element =
    val (path: String, fragment: Option[String]) = Strings.splitFirst(ref, '#')
    val params: Map[String, String] = parseParams(fragment)
    val src: String = params.get("page").map(page => s"$path#page=$page").getOrElse(path)
    make(src, label, params.get("height"))

  def make(src: String, label: String, height: Option[String] = None): Xml.Element =
    val href: String = src
    val text: String = Option(label).map(_.trim).filter(_.nonEmpty).getOrElse(src)
    val objectElement: Xml.Element = Xml
      .element("object")
      .set("data", href)
      .set("type", PdfType)
      .set("aria-label", text)
      .setChildren(Chunk(openLink(href, text)))
    val sibling: Xml.Element = Xml.element("p").add(LinkClass).setChildren(Chunk(openLink(href, text)))
    val wrapper: Xml.Element = Xml.element("div").add(Class).setChildren(Chunk(objectElement, sibling))
    height.map(_.trim).filter(_.nonEmpty).fold(wrapper): raw =>
      wrapper.set("style", s"$HeightVar: ${cssHeight(raw)}")

  def normalize(element: Xml.Element): Xml.Element =
    if is(element) then element
    else
      element.setChildren(element.getChildren.map: node =>
        node.asElement.filter(isPdfObject) match
          case Some(obj) => fromObject(obj)
          case None => node
      )

  private def isPdfObject(element: Xml.Element): Boolean =
    element.getName == "object" && (
      element.get("type").exists(_.toLowerCase.contains("pdf")) ||
      element.get("data").exists(data =>
        val path: String = Strings.splitFirst(data, '#')._1
        path.toLowerCase.endsWith(".pdf")
      )
    )

  private def fromObject(element: Xml.Element): Xml.Element =
    val src: String = element.get("data").filter(_.nonEmpty).getOrElse("")
    val label: String = element
      .get("aria-label")
      .orElse(element.get("title"))
      .filter(_.nonEmpty)
      .getOrElse(Strings.splitFirst(src, '#')._1.split('/').lastOption.getOrElse(src))
    val height: Option[String] = element.get("height")
    make(src, label, height)

  private def openLink(href: String, label: String): Xml.Element =
    Xml.element(XmlElement.A).set(XmlAttribute.Href, href).setText(s"Open PDF: $label")

  private def parseParams(fragment: Option[String]): Map[String, String] =
    fragment.toList
      .flatMap(_.split('&').toList)
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap: part =>
        val (key: String, value: Option[String]) = Strings.splitFirst(part, '=')
        value.map(_.trim).filter(_.nonEmpty).map(key.trim.toLowerCase -> _)
      .toMap

  private def cssHeight(raw: String): String =
    if raw.nonEmpty && raw.forall(_.isDigit) then s"${raw}px" else raw
