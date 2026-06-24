package org.podval.tei

enum EntityKind(
  val element: String,
  val nameElement: String,
  val listElement: String
) derives CanEqual:
  case Person       extends EntityKind("person",  "persName", "listPerson")
  case Place        extends EntityKind("place" , "placeName", "listPlace" )
  case Organization extends EntityKind("org"   ,   "orgName", "listOrg"   )
