package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Features
import org.podval.xml.XmlDialect

object XmlMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = XmlDialect.Plain

  def features: Features = Features(Seq.empty)
  
  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty

