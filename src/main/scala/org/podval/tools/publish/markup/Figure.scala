package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import zio.blocks.chunk.Chunk

/** Markup-neutral figure IR. CSS styles only these classes.
  * `<figure class="figure">`, optional `figcaption.figure-caption`. */
object Figure:
  object Class extends HtmlClass("figure")
  object CaptionClass extends HtmlClass("figure-caption")

  def is(element: Xml.Element): Boolean =
    element.getName == "figure" && element.has(Class)

  def isCaption(element: Xml.Element): Boolean =
    element.getName == "figcaption" && element.has(CaptionClass)

  def make(caption: Option[String], body: Xml.Nodes): Xml.Element =
    make(caption.map(_.trim).filter(_.nonEmpty).map(Xml.text).toSeq, body)

  def make(caption: Seq[Xml.Node], body: Xml.Nodes): Xml.Element =
    val captionElement: Option[Xml.Element] =
      Option.when(caption.nonEmpty)(
        Xml.element("figcaption").add(CaptionClass).setChildren(Chunk.from(caption))
      )
    Xml
      .element("figure")
      .add(Class)
      .setChildren(body.filterNot(_.isWhitespace) ++ Chunk.from(captionElement.toSeq))

  def normalize(element: Xml.Element): Xml.Element =
    if element.getName == "figure" then
      val withClass: Xml.Element = if is(element) then element else element.add(Class)
      withClass.setChildren(withClass.getChildren.map(normalizeCaptionNode))
    else if element.getName == "p" then
      wrapStandaloneImage(element).getOrElse(element)
    else element

  private def normalizeCaptionNode(node: Xml.Node): Xml.Node =
    node.asElement match
      case Some(el) if el.getName == "figcaption" && !el.has(CaptionClass) =>
        el.add(CaptionClass)
      case Some(el) if el.getName == "head" || el.getName == "tei-head" =>
        el.rename("figcaption").add(CaptionClass)
      case _ =>
        node

  // FlexMark (and HTML) wrap a block image in `<p>`. Title becomes figcaption.
  private def wrapStandaloneImage(paragraph: Xml.Element): Option[Xml.Element] =
    val children: Xml.Nodes = paragraph.getChildren.filterNot(_.isWhitespace)
    for
      only <- children.headOption.flatMap(_.asElement) if children.length == 1
      if isStandaloneImage(only)
    yield
      val (caption, body): (Option[String], Xml.Element) = takeTitle(only)
      make(caption, Chunk(body))

  private def isStandaloneImage(element: Xml.Element): Boolean =
    element.getName == "img" ||
    (
      element.getName == "a" &&
      element.getChildren.filterNot(_.isWhitespace).toList.match
        case List(child) => child.asElement.exists(_.getName == "img")
        case _ => false
    )

  private def takeTitle(element: Xml.Element): (Option[String], Xml.Element) =
    if element.getName == "img" then
      val caption: Option[String] = element.get("title").map(_.trim).filter(_.nonEmpty)
      (caption, caption.fold(element)(_ => element.set("title", "")))
    else
      val children: Xml.Nodes = element.getChildren
      val img: Xml.Element = children.flatMap(_.asElement).find(_.getName == "img").get
      val (caption, stripped): (Option[String], Xml.Element) = takeTitle(img)
      val body: Xml.Nodes = children.map: node =>
        if node.asElement.contains(img) then stripped else node
      (caption, element.setChildren(body))
