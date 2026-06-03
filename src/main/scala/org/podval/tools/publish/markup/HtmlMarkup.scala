package org.podval.tools.publish.markup

import org.podval.tools.publish.features.*

object HtmlMarkup extends HtmlLikeMarkup with Markup.XmlParsable:
  override def name: String = xmlDialect.name
  override val extension: String = "html"
  override val additionalExtensions: Set[String] = Set.empty

  def features: List[Feature] = List(
    HtmlSectionIdsFeature,
    AnchorIdsFeature,
    InternalLinksFeature,
    FootnotesFeature
  )

