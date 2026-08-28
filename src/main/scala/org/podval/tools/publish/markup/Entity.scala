package org.podval.tools.publish.markup

import org.podval.xml.WithRawXml
import zio.blocks.schema.Modifier

final case class Entity(
  @Modifier.config("xml.attribute", "") id: Option[String],
//  val entityType: EntityType,
  @Modifier.config("xml.attribute", "") role: Option[String],
  names: Seq[EntityName],
) extends WithRawXml
