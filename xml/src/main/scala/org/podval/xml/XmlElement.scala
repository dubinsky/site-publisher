package org.podval.xml

open class XmlElement(val name: String)

object XmlElement:
  object A extends XmlElement("a")

  object Code extends XmlElement("code")
