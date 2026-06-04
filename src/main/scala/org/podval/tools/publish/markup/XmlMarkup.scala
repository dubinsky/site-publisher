package org.podval.tools.publish.markup

import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.processor.Features
import org.podval.xml.{Xml, XmlDialect}

object XmlMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = XmlDialect.Plain

  def features: Features = Features(Seq.empty)
  
  override def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty

