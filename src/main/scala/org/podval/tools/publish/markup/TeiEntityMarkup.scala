package org.podval.tools.publish.markup

import org.podval.tei.TeiXmlDialect
import org.podval.tools.publish.feature.Feature
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.xml.{Xml, XmlDialect}

object TeiEntityMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = TeiXmlDialect

  def features: List[Feature] = TeiMarkup.features
  
  override def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section] =
    TeiMarkup.sections(element, errorReporter)


