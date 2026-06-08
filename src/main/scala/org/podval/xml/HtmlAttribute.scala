package org.podval.xml

object HtmlAttribute:
  object Href extends XmlAttribute("href")

  val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")
