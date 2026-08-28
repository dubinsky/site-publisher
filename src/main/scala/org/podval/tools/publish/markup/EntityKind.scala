package org.podval.tools.publish.markup

enum EntityKind(
  val element: String,
  val nameElement: String,
  val listElement: String
) derives CanEqual:
  case Person       extends EntityKind("person",  "persName", "listPerson")
  case Place        extends EntityKind("place" , "placeName", "listPlace" )
  case Organization extends EntityKind("org"   ,   "orgName", "listOrg"   )

object EntityKind:
  def forElement(name: String): Option[EntityKind] =
    values.find(_.element == name)
