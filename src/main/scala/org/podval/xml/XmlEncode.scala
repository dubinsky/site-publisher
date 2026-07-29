package org.podval.xml

object XmlEncode:
  def encodeXmlSpecials(string: String): String = string
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")

  def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&apos;")

  // Note: maybe use single quotes if the value contains double quote?
  def quote(value: String): String = "\"" + encodeXmlSpecials(value) + "\""
