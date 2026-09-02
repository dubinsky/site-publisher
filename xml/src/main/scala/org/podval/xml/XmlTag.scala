package org.podval.xml

/** Maps a Scala discriminator to XML element names (and back).
  * Different records may use different tables for the same `K` (e.g. `EntityKind.asRoot` vs `asName`). */
final class XmlTag[K](
  val toName: K => String,
  val fromName: String => Option[K],
  val names: Seq[String]
):
  def erased: XmlTag[Any] = XmlTag[Any](
    k => toName(k.asInstanceOf[K]),
    name => fromName(name).map(k => k: Any),
    names
  )

object XmlTag:
  def apply[K](
    toName: K => String,
    fromName: String => Option[K],
    names: Seq[String]
  ): XmlTag[K] = new XmlTag(toName, fromName, names)
