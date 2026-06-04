package org.podval.tools.publish.markup

import org.podval.store.StoreXmlDialect
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.processor.Features
import org.podval.xml.{Xml, XmlDialect}

object StoreMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = StoreXmlDialect

  def features: Features = TeiMarkup.features
  
  override def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section] =
    TeiMarkup.sections(element, errorReporter)
