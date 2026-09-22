package org.podval.tools.publish.markup

import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}
import Xml.given

/** Markup-neutral figure IR. CSS styles only these classes.
  * `<figure class="figure">`, optional `figcaption.figure-caption`. */
object Figure:
  object Class extends CssClass("figure")
  object CaptionClass extends CssClass("figure-caption")

  def is(element: Xml.Element): Boolean =
    element.isElement(XmlElement.Figure) && element.has(Class)

  def isCaption(element: Xml.Element): Boolean =
    element.isElement(XmlElement.Figcaption) && element.has(CaptionClass)

  def make(caption: Option[String], body: Xml.Nodes): Xml.Element =
    make(caption.map(_.trim).filter(_.nonEmpty).map(Xml.text).toSeq, body)

  def make(caption: Xml.Nodes, body: Xml.Nodes): Xml.Element =
    val captionElement: Option[Xml.Element] =
      Option.when(caption.nonEmpty)(
        Xml.element(XmlElement.Figcaption).add(CaptionClass).setChildren(caption)
      )
    Xml
      .element(XmlElement.Figure)
      .add(Class)
      .setChildren(body.filterNot(_.isWhitespace) ++ captionElement.toSeq)

  def normalize(element: Xml.Element): Xml.Element =
    if element.isElement(XmlElement.Figure) then
      val withClass: Xml.Element = if is(element) then element else element.add(Class)
      withClass.setChildren(withClass.getChildren.map(normalizeCaptionNode))
    else if element.isElement(XmlElement.P) then
      wrapStandaloneImage(element).getOrElse(element)
    else element

  private def normalizeCaptionNode(node: Xml.Node): Xml.Node =
    node.asElement match
      case Some(el) if el.isElement(XmlElement.Figcaption) && !el.has(CaptionClass) =>
        el.add(CaptionClass)
      case _ =>
        node

  // FlexMark (and HTML) wrap a block image in `<p>`. Title becomes figcaption.
  private def wrapStandaloneImage(paragraph: Xml.Element): Option[Xml.Element] =
    val children: Xml.Nodes = paragraph.getChildren.filterNot(_.isWhitespace)
    for
      only <- children.headOption.flatMap(_.asElement) if children.length == 1
      if isStandaloneImage(only)
    yield
      val (caption: Option[String], body: Xml.Element) = takeTitle(only)
      make(caption, Seq(body))

  private def isStandaloneImage(element: Xml.Element): Boolean =
    element.isElement(XmlElement.Img) ||
    (
      element.isA &&
      element.getChildren.filterNot(_.isWhitespace).toList.match
        case List(child) => child.asElement.exists(_.isElement(XmlElement.Img))
        case _ => false
    )

  private def takeTitle(element: Xml.Element): (Option[String], Xml.Element) =
    if element.isElement(XmlElement.Img) then
      val caption: Option[String] = element.get(XmlAttribute.Title).map(_.trim).filter(_.nonEmpty)
      (caption, caption.fold(element)(_ => element.set(XmlAttribute.Title, "")))
    else
      val children: Xml.Nodes = element.getChildren
      val img: Xml.Element = children.flatMap(_.asElement).find(_.isElement(XmlElement.Img)).get
      val (caption: Option[String], stripped: Xml.Element) = takeTitle(img)
      val body: Xml.Nodes = children.map: node =>
        if node.asElement.contains(img) then stripped else node
      (caption, element.setChildren(body))
