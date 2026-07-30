package org.podval.tools.publish.markup

import org.podval.xml.Xml

abstract class Transformer extends Processor:
  def transform(element: Xml.Element): Xml.Element
  