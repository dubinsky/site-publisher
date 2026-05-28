package org.podval.tools.publish

import org.podval.tools.publish.util.{Date, Icon, SchemaUtil}
import zio.blocks.chunk.Chunk
import zio.blocks.schema.{Schema, SchemaError}
import zio.blocks.schema.yaml.{Yaml, YamlCodec, YamlFormat, YamlReader, YamlWriter}
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

final case class FrontMatter(
  title: Option[String] = None,
  description: Option[String] = None,
  author: Option[String] = None,
  lang: Option[String] = None,
  math: Boolean = false,
  tags: List[String] = List.empty,
  categories: List[String] = List.empty,
  aliases: List[String] = List.empty,
  permalink: Option[String] = None,
  post: Boolean = false,
  postTitle: Option[String] = None,
  date: Option[Date] = None,
  icon: Option[Icon] = None,
//  modified_time: Option[Date] = None, TODO does not work because of the hard-coded camel case; see `modifiedTime()`
  headerPage: Option[FrontMatter.HeaderPage] = None
):
  private var extraKeys: Chunk[(Yaml, Yaml)] = Chunk.empty
  private def findExtraKey(name: String): Option[Yaml] = extraKeys
    .find((key, _) => key match
      case Yaml.Scalar(key, _) => key == name
      case _ => false
    )
    .map(_._2)

  def modifiedTime: Option[Date] = findExtraKey("modified_time").map(Date.codec.decodeValue)

  private var absent: Boolean = false

  def write: String = if absent then "" else
    val mapping: String = YamlWriter.write(Yaml.Mapping(
      FrontMatter.codec.encodeValue(this).asInstanceOf[Yaml.Mapping].entries ++ extraKeys
    ))

    s"---\n$mapping\n---\n"

object FrontMatter:
  final case class HeaderPage(
    include: Boolean = false,
    priority: Option[Int] = None
  )
  
  val empty: FrontMatter = FrontMatter()

  val absent: FrontMatter =
    val result = FrontMatter()
    result.absent = true
    result

  private val schema: Schema[FrontMatter] = Schema.derived

  private val fieldNames: Set[String] = SchemaUtil.fieldNames(schema)

  private val codec: YamlCodec[FrontMatter] = schema
    .deriving(YamlFormat.deriver)
    .instance(TypeId.of[Date], Date.codec)
    .instance(TypeId.of[Icon.Style], Icon.Style.codec)
    .derive

  def parse(
    input: String,
    errorReporter: PageError.Reporter
  ): (FrontMatter, String) =
    val frontMatterEnd: Int = if !input.startsWith("---\n") then -1 else input.indexOf("\n---\n", 3)
    if frontMatterEnd == -1 then (absent, input) else
      val frontMatterInput: String = input.substring(3, frontMatterEnd)
      val frontMatter: FrontMatter = if frontMatterInput.isEmpty then empty else decode(frontMatterInput) match
        case Right(frontMatter) => frontMatter
        case Left(error) =>
          errorReporter.error(PageError.Parsing, s"Malformed FrontMatter: [$input]", Some(error))
          FrontMatter.empty 
      val frontMatterLines: Int = frontMatterInput.count(_ == '\n') + 2
      val content: String =
        "\n"*frontMatterLines +
        input.substring(frontMatterEnd + 5)
      (frontMatter, content)
  
  private def decode(input: String): Either[SchemaError, FrontMatter] =
    try
      val yaml: Yaml = YamlReader.read(input)
      val result: FrontMatter = codec.decodeValue(yaml)
      result.extraKeys = yaml
        .asInstanceOf[Yaml.Mapping]
        .entries
        .filter(_._1 match
          case Yaml.Scalar(key, _) => !fieldNames.contains(key)
          case _ => true
        )
      Right(result)
    catch
      case error: Throwable if NonFatal(error) => new Left(SchemaError(error.getMessage))
