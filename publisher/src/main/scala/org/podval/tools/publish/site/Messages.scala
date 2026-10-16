package org.podval.tools.publish.site

import org.podval.metadata.{Language, Name, Names}
import org.podval.xml.{Xml, XmlCodec, XmlError, XmlParser}
import zio.blocks.schema.Schema

/** Interface strings. English is first in each message, so [Names.doFind] falls back to it. */
object Messages:
  def text(key: String, values: (String, Any)*)(using spec: Language.Spec): String =
    fill(messages(key).doFind(spec).name, values.iterator.map((k, v) => k -> v.toString).toMap)

  private[site] lazy val messages: Map[String, Names] =
    val loaded: Seq[Message] = XmlParser.loadCatalog(this, messageCodec)
    val duplicateKeys: Seq[String] =
      loaded.groupBy(_.key).collect { case (key, group) if group.length > 1 => key }.toSeq
    require(duplicateKeys.isEmpty, s"Duplicate message keys: ${duplicateKeys.mkString(", ")}")
    loaded.map(message => message.key -> message.names).toMap

  /** A `{name}` with no supplied value throws. Extra values are ignored. Values are not re-scanned. */
  private def fill(template: String, values: Map[String, String]): String =
    def loop(rest: String): String = rest.span(_ != '{') match
      case (head, "") => head
      case (head, tail) =>
        val close: Int = tail.indexOf('}')
        if close < 0 then throw IllegalArgumentException(s"Unclosed brace in message: $template")
        val name: String = tail.substring(1, close)
        val value: String = values.getOrElse(name, throw IllegalArgumentException(
          s"No value for {$name} in message: $template"
        ))
        head + value + loop(tail.substring(close + 1))
    loop(template)

  private final case class Message(key: String, names: Names)

  private final case class Data(
    key: String,
    names: Seq[Name] = Seq.empty
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  private val messageCodec: XmlCodec[Message] = new XmlCodec[Message]:
    override def elementName: String = "message"
    override def isRecordLike: Boolean = true

    override def unsafeDecode(element: Xml.Element): Message =
      val data: Data = Data.codec.unsafeDecode(element)
      if data.key.isBlank then throw XmlError("Message key is blank")
      if data.names.isEmpty then throw XmlError(s"${data.key}: no names")
      Message(data.key, Names(data.names))

    override def encodeNamed(elName: String, value: Message): Xml.Element =
      Data.codec.encodeNamed(elName, Data(key = value.key, names = value.names.names))
