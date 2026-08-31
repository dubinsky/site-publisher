package org.podval.tools.publish.markup

import org.podval.xml.WithRawXml
import zio.blocks.schema.Modifier

//final class Entity(
//  val entityType: EntityType,
//  val role: Option[String],
//  override val name: String,
//  val mainName: String  // Note: can mostly be reconstructed from the name...
//)
final case class Entity(
  @Modifier.config("xml.attribute", "") id: Option[String],
//  val entityType: EntityType,
  @Modifier.config("xml.attribute", "") role: Option[String],
  names: Seq[EntityName],
) extends WithRawXml
