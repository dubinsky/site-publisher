package org.podval.xml

import zio.blocks.schema.Schema
import zio.blocks.schema.derive.Deriver
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

/** Document-shaped XML codec over any `XmlAst`.
  *
  * Derive with `XmlCodec.derived` from a `Schema`. Binding hints are Schema modifiers
  * (`Modifier` is sealed, so `@xmlAttribute` is not visible to `Schema.derived`):
  *
  * {{{
  * final case class Language(
  *   @Modifier.config(XmlCodec.Attribute, "") ident: String
  * )
  * given Schema[Language] = Schema.derived
  * val codec: XmlCodec[Language] = XmlCodec.derived
  * val el: Xml.Element = codec.encode(Language("ru"))
  * }}}
  *
  * `encode` is polymorphic in the AST; pin it with a type ascription when more than one
  * `XmlAst` is in scope.
  */
object XmlCodec:
  /** `@Modifier.config(XmlCodec.Attribute, "")` or `@Modifier.config(XmlCodec.Attribute, "xml:id")`. */
  final val Attribute = "xml.attribute"
  /** Type or field element name: `@Modifier.config(XmlCodec.Element, "persName")`. */
  final val Element = "xml.element"
  /** Character content of this element: `@Modifier.config(XmlCodec.Text, "")`. */
  final val Text = "xml.text"
  /** Leftover attributes and children: `@Modifier.config(XmlCodec.Extras, "")`. */
  final val Extras = "xml.extras"
  final val NamespaceUri = "xml.namespace.uri"
  final val NamespacePrefix = "xml.namespace.prefix"

  val deriver: Deriver[XmlCodec] = XmlCodecDeriver

  def derived[A](using schema: Schema[A]): XmlCodec[A] = schema.derive(deriver)

  /** Derive a record whose XML tag comes from `tagField` via `tag`.
    * Nested records that also tag this way need `.instance(TypeId.of[Nested], Nested.codec)`. */
  def derived[A, K](tagField: String, tag: XmlTag[K])(using schema: Schema[A], typeId: TypeId[A]): XmlCodec[A] =
    schema.derive(XmlCodecDeriver.tagged(tagField, tag))

trait XmlCodec[A]:
  def elementName: String

  def decode[E: XmlAst](element: E): Either[XmlError, A] =
    try Right(unsafeDecode(element))
    catch
      case e: XmlError => Left(e)
      case e if NonFatal(e) => Left(XmlError(Option(e.getMessage).getOrElse(e.toString)))

  def elementNameOf(value: A): String = elementName

  def encode[E: XmlAst](value: A): E = encodeNamed(elementNameOf(value), value)

  def encodeNamed[E: XmlAst](name: String, value: A): E

  def unsafeDecode[E: XmlAst](element: E): A

  def unsafeDecodeText(text: String): A =
    throw XmlError("Type does not decode from text")

  def encodeText(value: A): String =
    throw XmlError("Type does not encode as text")

  def caseNames: Seq[String] = Seq.empty

  def isEnumeration: Boolean = false

  /** Nested record/identity: child name comes from the type (or an override), not from a primitive wrapper. */
  def isRecordLike: Boolean = false
