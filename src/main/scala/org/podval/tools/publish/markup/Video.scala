package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, HtmlElement, Xml, XmlAttribute}
import zio.blocks.chunk.Chunk

/** Markup-neutral video IR. CSS styles only these classes.
  * Local file: `<video class="video" controls>`. YouTube/Vimeo: `<iframe class="video-embed">`. */
object Video:
  object Class extends HtmlClass("video")
  object EmbedClass extends HtmlClass("video-embed")

  def is(element: Xml.Element): Boolean =
    element.getName == "video" && element.has(Class)

  def isEmbed(element: Xml.Element): Boolean =
    element.getName == "iframe" && element.has(EmbedClass)

  def make(src: String, label: String): Xml.Element =
    val href: String = src
    val text: String = Option(label).map(_.trim).filter(_.nonEmpty).getOrElse(src)
    Xml
      .element("video")
      .add(Class)
      .set("src", href)
      .set("controls", "controls")
      .set("aria-label", text)
      .setChildren(Chunk(openLink(href, text)))

  def normalize(element: Xml.Element): Xml.Element =
    if element.getName == "video" then normalizeLocal(element)
    else if isRemotePlayer(element) then
      if isEmbed(element) then element else element.add(EmbedClass)
    else element

  private def normalizeLocal(element: Xml.Element): Xml.Element =
    val withClass: Xml.Element = if is(element) then element else element.add(Class)
    val withControls: Xml.Element =
      if withClass.get("controls").isDefined then withClass
      else withClass.set("controls", "controls")
    val src: Option[String] = withControls.get("src").filter(_.nonEmpty)
    val hasMarkup: Boolean = withControls.getChildren.flatMap(_.asElement).nonEmpty
    if hasMarkup || src.isEmpty then withControls
    else
      val label: String = withControls
        .get("aria-label")
        .filter(_.nonEmpty)
        .getOrElse(src.get.split('/').lastOption.getOrElse(src.get))
      withControls.setChildren(Chunk(openLink(src.get, label)))

  private def isRemotePlayer(element: Xml.Element): Boolean =
    element.getName == "iframe" && element.get("src").exists: src =>
      val lower: String = src.toLowerCase
      lower.contains("youtube.com/embed") ||
      lower.contains("youtube-nocookie.com/embed") ||
      lower.contains("player.vimeo.com/video")

  private def openLink(href: String, label: String): Xml.Element =
    Xml.element(HtmlElement.A).set(XmlAttribute.Href, href).setText(s"Open video: $label")
