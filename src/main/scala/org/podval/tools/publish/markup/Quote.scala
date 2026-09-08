package org.podval.tools.publish.markup

import org.podval.xml.{CssClass, Xml}

/** Markup-neutral quote IR. CSS styles only these classes.
  * `<blockquote class="quote">`, optional title and attribution. */
object Quote:
  object Class extends CssClass("quote")
  object TitleClass extends CssClass("quote-title")
  object AttributionClass extends CssClass("quote-attribution")

  def is(element: Xml.Element): Boolean =
    element.qName == "blockquote" && element.has(Class)

  def isTitle(element: Xml.Element): Boolean = element.has(TitleClass)

  def isAttribution(element: Xml.Element): Boolean =
    element.qName == "footer" && element.has(AttributionClass)

  def make(
    title: Option[String],
    attribution: Xml.Nodes,
    body: Xml.Nodes
  ): Xml.Element =
    val titleElement: Option[Xml.Element] = title.map(_.trim).filter(_.nonEmpty).map: label =>
      Xml.element("div").add(TitleClass).setText(label)
    val attributionElement: Option[Xml.Element] =
      Option.when(attribution.nonEmpty)(
        Xml.element("footer").add(AttributionClass).setChildren(attribution)
      )
    Xml
      .element("blockquote")
      .add(Class)
      .setChildren(
        titleElement.toSeq ++
        body.filterNot(_.isWhitespace) ++
        attributionElement.toSeq
      )

  def normalize(element: Xml.Element): Xml.Element =
    if element.qName != "blockquote" then element
    else
      val withClass: Xml.Element = if is(element) then element else element.add(Class)
      withClass.setChildren(withClass.getChildren.map: node =>
        node.asElement.filter(el => el.qName == "footer" && !el.has(AttributionClass)) match
          case Some(footer) => footer.add(AttributionClass)
          case None => node
      )
