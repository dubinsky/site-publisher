package org.podval.tools.publish.markup

import org.podval.tools.publish.feature.Feature
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.xml.{Xml, XmlDialect}

object XmlMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = XmlDialect.Plain

  def features: List[Feature] = List.empty
  
  override def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty

