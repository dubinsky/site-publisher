package org.podval.xml

open class XmlAttribute(val name: String)

object XmlAttribute:
  object Id extends XmlAttribute("id")
  object XmlId extends XmlAttribute("xml:id")
  // TODO lang?
  object XmlBase extends XmlAttribute("xml:base")

  object Xmlns extends XmlAttribute("xmlns"):
    def apply(ns: String): XmlAttribute = XmlAttribute(s"$name:$ns")

  object Href extends XmlAttribute("href")
