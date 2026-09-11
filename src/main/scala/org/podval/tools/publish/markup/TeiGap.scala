package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlAttribute}

/** TEI `<gap reason>` → hover tip with the reason text. */
object TeiGap:
  val tip: Tip = Tip("gap")
  private object Converted extends XmlAttribute("data-gap-tip")

  def convert(element: Xml.Element): Xml.Element =
    if !element.isNamed("gap") || element.get(Converted).isDefined then element
    else
      element.get("reason").map(_.trim).filter(_.nonEmpty).fold(element): reason =>
        tip.attachTip(element.set(Converted, "true"), Seq(Xml.text(reason)))
