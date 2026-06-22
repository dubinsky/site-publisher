package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Processors
import org.podval.xml.{Html, XmlDialect}

object XmlMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = XmlDialect.Plain

  override def processors: Processors = Processors()

  override def pageHeader(content: PageContent): Html.Element = Markup.pageHeader(content)

  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty
