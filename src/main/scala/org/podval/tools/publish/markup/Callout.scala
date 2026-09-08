package org.podval.tools.publish.markup

import org.podval.xml.{CssClass, Xml}

/** Markup-neutral callout IR. CSS styles only these classes.
  * Markers sit in verbatim (`span.callout`); the numbered list is `ol.callout-list`. */
object Callout:
  object MarkClass extends CssClass("callout")
  object ListClass extends CssClass("callout-list")

  def isMark(element: Xml.Element): Boolean = element.has(MarkClass)

  def isList(element: Xml.Element): Boolean =
    element.getName == "ol" && element.has(ListClass)

  def marker(number: String): Xml.Element =
    Xml
      .element("span")
      .add(MarkClass)
      .set("data-value", number)
      .setText(number)
