package org.podval.tools.publish.markup

import org.podval.tools.publish.feature.*
import org.podval.tools.publish.processor.Processors

object HtmlMarkup extends HtmlLikeMarkup with XmlParsableMarkup:
  override def name: String = xmlDialect.name
  override val extension: String = "html"
  override val additionalExtensions: Set[String] = Set.empty

  def processors: Processors = Processors(
    new HtmlSectionIdsConverter,
    new AnchorIdsConverter,
    new InternalLinksProcessor,
    new FootnotesTransformer
  )
