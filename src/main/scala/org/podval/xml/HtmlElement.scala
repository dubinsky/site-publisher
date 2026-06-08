package org.podval.xml

object HtmlElement:
  object A extends XmlElement("a")

  object Code extends XmlElement("code")
  
  val reservedElements: Set[String] = Set("head" , "body", "title", "div", "p")
  