package org.podval.tools.publish.markup

import org.podval.tei.TeiXmlDialect
import org.podval.tools.publish.features.*
import org.podval.tools.publish.{Fragment, PageError}
import org.podval.xml.{Xml, XmlDialect}

object XmlMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = XmlDialect.Plain

  def features: List[Feature] = List.empty
  
  override def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty

