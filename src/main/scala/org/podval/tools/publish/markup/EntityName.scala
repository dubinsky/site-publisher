package org.podval.tools.publish.markup

import zio.blocks.schema.Modifier

final case class EntityName(
//  val entityType: EntityType,
  @Modifier.config("xml.attribute", "") id: Option[String] = None,
  @Modifier.config("xml.attribute", "") ref: Option[String] = None,
  name: String // TODO Text
)
