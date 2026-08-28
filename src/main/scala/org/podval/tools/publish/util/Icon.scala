package org.podval.tools.publish.util

import org.podval.xml.{Html, Xml}
import zio.blocks.chunk.Chunk
import zio.blocks.html.*
import zio.blocks.schema.yaml.{Yaml, YamlCodec}

final class Icon(val name: String, val style: Icon.Style):
  def html: Html.Element = span(
    className += "icon-span",
    className += style.classNames,
    className += s"fa-$name"
  )

  def xml: Xml.Element =
    val classes: Seq[String] =
      Seq("icon-span") ++ style.classNames.split(" ").filter(_.nonEmpty).toSeq :+ s"fa-$name"
    Xml.element("span").setClasses(Chunk.from(classes))

object Icon:
  val file = Icon("file", Regular)
  val key = Icon("key", Solid)
  val folder = Icon("folder", Regular)
  val note = Icon("note-sticky", Regular)
  val envelope = Icon("envelope", Regular)
  val calendar = Icon("calendar", Regular)
  val tag = Icon("tag", Solid)
  val tags = Icon("tags", Solid)
  val list = Icon("list", Solid)
  val errors = Icon("circle-xmark", Regular)
  val pdf = Icon("file-pdf", Regular)
  val fileLines = Icon("file-lines", Regular)
  val images = Icon("images", Regular)
  val tableList = Icon("table-list", Solid)
  val book = Icon("book", Solid)
  val gear = Icon("gear", Solid)
  val arrowUp = Icon("arrow-up", Solid)
  val arrowLeft = Icon("arrow-left", Solid)
  val arrowRight = Icon("arrow-right", Solid)
  val arrowRightLong = Icon("arrow-right-long", Solid)
  val rss = Icon("rss", Solid)
  def brand(name: String) = Icon(name, Brands)

  sealed abstract class Style:
    final def classNames: String = s"grey $additions fa-$name"
    def name: String
    def additions: String

  case object Solid extends Style:
    override def name: String = "solid"
    override def additions: String = "fa-classic"

  case object Regular extends Style:
    override def name: String = "regular"
    override def additions: String = "fa-classic"

  case object Brands extends Style:
    override def name: String = "brands"
    override def additions: String = "fa-lg"

  object Style:
    private val all: Set[Style] = Set(Solid, Regular, Brands)

    def codec: YamlCodec[Style] = new YamlCodec[Style]:
      def encodeValue(style: Style): Yaml = Yaml.Scalar(style.name)

      def decodeValue(yaml: Yaml): Style = yaml match
        case Yaml.Scalar(value, _) => all.find(_.name == value.trim) match
          case Some(style) => style
          case None => error(s"Unknown style: $value")
        case _ => error("Expected scalar value")
