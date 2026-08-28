package org.podval.tools.publish.markup

import org.podval.xml.WithRawXml
import zio.blocks.schema.Modifier

final case class EntityReference(
//  @Modifier.config("xml.attribute", "") `type`: EntityType,
  @Modifier.config("xml.attribute", "") id: Option[String],
  @Modifier.config("xml.attribute", "") role: Option[String],
  @Modifier.config("xml.attribute", "") ref: Option[String]
) extends WithRawXml
