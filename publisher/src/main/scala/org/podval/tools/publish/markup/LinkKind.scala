package org.podval.tools.publish.markup

import org.podval.xml.Xml
import org.podval.xml.Xml.given

sealed abstract class LinkKind

object LinkKind:
  final case class Entity(kind: EntityKind) extends LinkKind

  def of(element: Xml.Element): Option[LinkKind] = EntityKind
    .values
    .find(entityKind => element.hasClass(entityKind.nameElement))
    .map(Entity(_))
