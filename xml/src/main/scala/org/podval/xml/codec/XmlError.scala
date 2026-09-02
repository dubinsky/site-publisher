package org.podval.xml.codec

final class XmlError(
  val message: String,
  val path: List[String] = Nil
) extends Exception(
  if path.isEmpty then message else s"${path.mkString("/")}: $message"
) derives CanEqual:
  def at(segment: String): XmlError = new XmlError(message, segment :: path)

object XmlError:
  def apply(message: String): XmlError = new XmlError(message)
  def apply(message: String, path: List[String]): XmlError = new XmlError(message, path)
