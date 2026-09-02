package org.podval.xml

/** Attribute and child helpers for hand-written codecs. */
object XmlDecode:
  def childrenNamed[E: XmlAst](element: E, name: String): Seq[E] =
    element.getChildren.flatMap(_.asElement).filter(_.localName == name)

  def requireName[E: XmlAst](element: E, name: String): Unit =
    if element.localName != name then throw XmlError(s"Expected '$name', found '${element.getName}'")

  def requireAttr[E: XmlAst](element: E, name: String): String =
    element.get(name).map(_.trim).filter(_.nonEmpty).getOrElse:
      throw XmlError(s"Missing attribute '$name'")

  def intOpt[E: XmlAst](element: E, name: String): Option[Int] =
    element.get(name).map(_.trim).filter(_.nonEmpty).map: raw =>
      raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))

  def requireNoOther[E: XmlAst](element: E, allowed: Set[String]): Unit =
    val extra: Seq[String] = element.getChildren.flatMap(_.asElement).map(_.localName).filterNot(allowed.contains)
    if extra.nonEmpty then throw XmlError(s"Unparsed elements: $extra")

  def intAttr[E: XmlAst](element: E, name: String): Int =
    val raw: String = requireAttr(element, name)
    raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))

  def positiveInt[E: XmlAst](element: E, name: String): Int =
    val n: Int = intAttr(element, name)
    if n <= 0 then throw XmlError(s"Non-positive integer: $n")
    n

  def positiveIntOpt[E: XmlAst](element: E, name: String): Option[Int] =
    intOpt(element, name).map: n =>
      if n <= 0 then throw XmlError(s"Non-positive integer: $n")
      n

  def booleanOpt[E: XmlAst](element: E, name: String): Option[Boolean] =
    element.get(name).map(_.trim).filter(_.nonEmpty).map:
      case "true" | "yes" | "1" => true
      case "false" | "no" | "0" => false
      case other => throw XmlError(s"Invalid boolean for $name: $other")
