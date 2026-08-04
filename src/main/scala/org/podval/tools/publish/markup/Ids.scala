package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlDialect}

final class Ids private(anchors: Seq[Id]):
  def resolve(id: String): Option[Link.ToId] = anchors.find(_.id == id).map(anchor => Link.ToId(anchor.id))

object Ids:
  def apply(
    xml: Xml.Element,
    xmlDialect: XmlDialect
  ): Ids =
    val result: Seq[Id] = xmlDialect.gather(xml, _.getId.map(Id(_)))
    new Ids(result)
