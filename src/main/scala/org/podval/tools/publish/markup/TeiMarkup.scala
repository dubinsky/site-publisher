package org.podval.tools.publish.markup

import org.podval.tei.TeiXmlDialect
import org.podval.tools.publish.feature.*
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Processors
import org.podval.xml.{Xml, XmlDialect}

object TeiMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = TeiXmlDialect

  def processors: Processors = Processors(
    new TeiConverter,
    new TeiFootnotesConverter,
    new TeiSectionIdsConverter,
    new AnchorIdsConverter,
    new InternalLinksProcessor,
    new FootnotesTransformer
  )

  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty // TODO

