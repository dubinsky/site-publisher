package org.podval.tools.publish.markup

import org.podval.tei.TeiXmlDialect
import org.podval.tools.publish.feature.*
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.processor.Features
import org.podval.xml.{Xml, XmlDialect}

object TeiMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = TeiXmlDialect

  def features: Features = Features(Seq(
    TeiFeature(),
    TeiFootnotesFeature(),
    TeiSectionIdsFeature(),
    AnchorIdsFeature(),
    InternalLinksFeature(),
    FootnotesFeature()
  ))

  override def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty // TODO

