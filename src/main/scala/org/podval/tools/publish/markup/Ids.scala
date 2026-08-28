package org.podval.tools.publish.markup

import org.podval.xml.Xml

final class Ids private(anchors: Seq[Ids.Id]):
  private def getById(id: String): Option[Ids.Id] = anchors.find(_.id == id)
  def sectionById(id: String): Option[String] = getById(id).get.sectionId
  def resolve(id: String): Option[Link.ToId] = getById(id).map(anchor => Link.ToId(anchor.id))

object Ids:
  val empty: Ids = new Ids(Seq.empty)

  private final class Id(
    val id: String,
    val sectionId: Option[String]
  )
  
  def apply(
    xml: Xml.Element
  ): Ids = new Ids(
    xml.gatherWithContext(
      gatherElement = (element, section) => element.getId.map(id => Id(id, section.map(_.getId.get))),
      isContext = Section.is
    )
  )
