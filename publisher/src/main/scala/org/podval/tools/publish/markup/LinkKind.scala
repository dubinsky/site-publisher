package org.podval.tools.publish.markup

import org.podval.xml.Xml

sealed abstract class LinkKind

object LinkKind:
  final case class Entity(kind: EntityKind) extends LinkKind

  // TODO use
  case object Facsimile extends LinkKind

  // TODO use
  case object Tag extends LinkKind

  // TODO use
  case object Category extends LinkKind

  def of(element: Xml.Element): Option[LinkKind] = EntityKind
    .values
    .find(entityKind => element.hasClass(entityKind.nameElement))
    .map(Entity(_))
