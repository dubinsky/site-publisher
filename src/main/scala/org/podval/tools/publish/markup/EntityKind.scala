package org.podval.tools.publish.markup

import org.podval.xml.XmlTag
import zio.blocks.schema.Schema

enum EntityKind(
  val element: String,
  val nameElement: String,
  val listElement: String
) derives CanEqual:
  case Person       extends EntityKind("person",  "persName", "listPerson")
  case Place        extends EntityKind("place" , "placeName", "listPlace" )
  case Organization extends EntityKind("org"   ,   "orgName", "listOrg"   )

object EntityKind:
  given schema: Schema[EntityKind] = Schema.derived

  def forElement(name: String): Option[EntityKind] =
    values.find(_.element == name)

  def forNameElement(name: String): Option[EntityKind] =
    values.find(_.nameElement == name)

  def forListElement(name: String): Option[EntityKind] =
    values.find(_.listElement == name)

  val asRoot: XmlTag[EntityKind] = XmlTag(_.element, forElement, values.map(_.element).toSeq)
  val asName: XmlTag[EntityKind] = XmlTag(_.nameElement, forNameElement, values.map(_.nameElement).toSeq)
  val asList: XmlTag[EntityKind] = XmlTag(_.listElement, forListElement, values.map(_.listElement).toSeq)
