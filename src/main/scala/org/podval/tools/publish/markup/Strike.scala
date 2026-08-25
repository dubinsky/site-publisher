package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** Markup-neutral strikethrough IR is HTML `<del>`. Browser default is the style. */
object Strike:
  def is(element: Xml.Element): Boolean = element.getName == "del"

  def normalize(element: Xml.Element): Xml.Element =
    if is(element) then element
    else if element.getName == "s" then element.rename("del")
    else if isLineThroughWrapper(element) then
      element
        .setClasses(element.getClasses.filterNot(_ == "line-through"))
        .rename("del")
    else element

  private def isLineThroughWrapper(element: Xml.Element): Boolean =
    element.hasClass("line-through") &&
    (element.getName == "span" || element.getName == "mark")
