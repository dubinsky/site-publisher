package org.podval.xml

open class XmlAttribute(val name: String)

object XmlAttribute:
  object Id extends XmlAttribute("id"):
    def toId(text: String): String = text.trim.replace(' ', '-')

  object Xmlns extends XmlAttribute("xmlns"):
    def apply(ns: String): XmlAttribute = XmlAttribute(s"$name:$ns")
    