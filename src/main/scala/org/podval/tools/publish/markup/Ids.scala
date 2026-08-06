package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlDialect}

final class Ids private(anchors: Seq[Ids.Id]):
  private def getById(id: String): Option[Ids.Id] = anchors.find(_.id == id)
  def sectionById(id: String): Option[String] = getById(id).get.sectionId
  def resolve(id: String): Option[Link.ToId] = getById(id).map(anchor => Link.ToId(anchor.id))

object Ids:
  private final class Id(
    val id: String,
    val sectionId: Option[String]
  )
  
  def apply(
    xml: Xml.Element,
    xmlDialect: XmlDialect
  ): Ids = new Ids(
    xmlDialect.gatherWithContext(
      xml,
      isContext = Section.is,
      gatherElement = (element, section) => element.getId.map(id => Id(id, section.map(_.getId.get)))
    )
  )
