package org.podval.tools.publish.link

import org.podval.xml.{HtmlClass, Xml}

sealed abstract class LinkKind(val htmlClass: String)

// TODO split into Entity snd not
// TODO Tag, Category, Pb/facsimile
object LinkKind:
  case object Person extends LinkKind("persName")

  case object Place extends LinkKind("placeName")

  case object Organization extends LinkKind("orgName")

  val all: List[LinkKind] = List(Person, Place, Organization)

  def of(element: Xml.Element): Option[LinkKind] = all.find(kind => element.has(HtmlClass(kind.htmlClass)))
