package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, HtmlXmlDialect, Xml, XmlAttribute}
import zio.blocks.chunk.Chunk

object Citation:
  object CiteClass extends HtmlClass("citation")
  object ItemClass extends HtmlClass("citation-item")
  object ListClass extends HtmlClass("bibliography")
  object UnresolvedClass extends HtmlClass("unresolved-citation")

  private object ModeAttr extends XmlAttribute("data-mode")
  private object KeyAttr extends XmlAttribute("data-key")
  private object LocatorAttr extends XmlAttribute("data-locator")

  enum Mode derives CanEqual:
    case Parenthetical, Narrative, SuppressAuthor
    def attr: String = this match
      case Parenthetical => "parenthetical"
      case Narrative => "narrative"
      case SuppressAuthor => "suppress-author"

  object Mode:
    def fromAttr(value: String): Mode = value match
      case "narrative" => Narrative
      case "suppress-author" => SuppressAuthor
      case _ => Parenthetical

  final class Item(
    val key: String,
    val locator: Option[String] = None
  )

  def isCite(element: Xml.Element): Boolean = element.has(CiteClass)
  def isList(element: Xml.Element): Boolean = element.getName == "div" && element.has(ListClass)

  def entryId(key: String): String = s"bibl-$key"
  def entryHref(key: String): String = s"#${entryId(key)}"

  def cite(mode: Mode, items: Seq[Item]): Xml.Element =
    Xml
      .element("span")
      .add(CiteClass)
      .set(ModeAttr, mode.attr)
      .setChildren(Chunk.from(items.map(itemToElement)))

  def listPlaceholder: Xml.Element =
    Xml.element("div").add(ListClass)

  def toHtmlString(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  def modeOf(element: Xml.Element): Mode =
    Mode.fromAttr(element.get(ModeAttr).getOrElse("parenthetical"))

  def itemsOf(element: Xml.Element): Seq[Item] =
    element
      .getChildren
      .flatMap(_.asElement)
      .filter(_.has(ItemClass))
      .map: item =>
        Item(
          key = item.get(KeyAttr).getOrElse(""),
          locator = item.get(LocatorAttr).filter(_.nonEmpty)
        )
      .filter(_.key.nonEmpty)
      .toSeq

  def gather(xml: Xml.Element): Chunk[Xml.Element] =
    xml.gather(element => Option.when(isCite(element))(element))

  /** AsciiDoc `cite:[key]`, `cite:[key, 33]`, `cite:[key1, key2]`. */
  def parseAsciiDocTarget(raw: String): Seq[Item] =
    val parts: Seq[String] = raw.split(',').toSeq.map(_.trim).filter(_.nonEmpty)
    if parts.isEmpty then Seq.empty
    else if parts.length == 1 then Seq(Item(parts.head))
    else
      val keys: Seq[String] = parts.filter(isBibKey)
      val rest: Seq[String] = parts.filterNot(isBibKey)
      if rest.isEmpty then keys.map(Item(_))
      else if keys.isEmpty then Seq(Item(parts.head, Some(parts.tail.mkString(", "))))
      else keys.init.map(Item(_)) :+ Item(keys.last, Some(rest.mkString(", ")))

  def isBibKey(token: String): Boolean =
    token.nonEmpty && token.charAt(0).isLetter && token.forall(c => c.isLetterOrDigit || c == '_' || c == '-' || c == ':')

  private def itemToElement(item: Item): Xml.Element =
    Xml
      .element("span")
      .add(ItemClass)
      .set(KeyAttr, item.key)
      .set(LocatorAttr, item.locator)
