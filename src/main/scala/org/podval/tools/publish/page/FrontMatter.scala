package org.podval.tools.publish.page

import org.podval.tools.publish.util.{Date, Icon, SchemaUtil}
import zio.blocks.chunk.Chunk
import zio.blocks.schema.yaml.{Yaml, YamlCodec, YamlFormat, YamlReader, YamlWriter}
import zio.blocks.schema.{NameMapper, Schema}
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
  // Note: not using nested `Icon` to avoid weird-looking JSON display of the property in Obsidian.
  icon: Option[String] = None,
  iconStyle: Option[Icon.Style] = None,
  // Note: not using nested `HeaderPage` to avoid weird-looking JSON display of the property in Obsidian.
  headerPage: Boolean = false,
  headerPagePriority: Option[Int] = None,
//  modified_time: Option[Date] = None, TODO does not work because of the hard-coded camel case; see `modifiedTime()`
  tocDepth: Option[Int] = None,
  chunk: Boolean = false,
  chunkDepth: Option[Int] = None
):
  private var extraKeys: Chunk[(Yaml, Yaml)] = Chunk.empty
  
  private def setExtraKeys(extraKeys: Chunk[(Yaml, Yaml)]): Unit =
    this.extraKeys = extraKeys
    modifiedTimeVar = findExtraKey("modified_time").map(Date.codec.decodeValue)

  private def findExtraKey(name: String): Option[Yaml] = extraKeys
    .find((key, _) => key match
      case Yaml.Scalar(key, _) => key == name
      case _ => false
    )
    .map(_._2)
  
  private var modifiedTimeVar: Option[Date] = None
  def modifiedTime: Option[Date] = modifiedTimeVar

  private var absent: Boolean = false

  def write: String = if absent then "" else
    val mapping: String = YamlWriter.write(Yaml.Mapping(
      FrontMatter.codec.encodeValue(this).asInstanceOf[Yaml.Mapping].entries ++ extraKeys
    ))

    s"---\n$mapping\n---\n"

object FrontMatter:
  private val standAloneExtensions: Seq[String] = Seq("yaml", "yml")
  def isStandAloneExtension(extension: Option[String]): Boolean = extension.exists(standAloneExtensions.contains)
  
  val empty: FrontMatter = FrontMatter()

  val absent: FrontMatter =
    val result = FrontMatter()
    result.absent = true
    result

  private val schema: Schema[FrontMatter] = Schema.derived

  private val fieldNames: Set[String] = SchemaUtil.fieldNames(schema)
  private val fieldNamesMangled: Set[String] = fieldNames.map(NameMapper.KebabCase.apply)

  private val codec: YamlCodec[FrontMatter] = schema
    .deriving(YamlFormat.deriver)
    .instance(TypeId.of[Date], Date.codec)
    .instance(TypeId.of[Icon.Style], Icon.Style.codec)
    .derive

  def split(input: String): (Option[String], String) =
    val frontMatterEnd: Int = if !input.startsWith("---\n") then -1 else input.indexOf("\n---\n", 3)
    if frontMatterEnd == -1 then (None, input) else
      val frontMatterContent: String = input.substring(3, frontMatterEnd)
      val frontMatterLines: Int = frontMatterContent.count(_ == '\n') + 2
      val content: String = "\n" * frontMatterLines + input.substring(frontMatterEnd + 5)
      (Some(frontMatterContent), content)

  def parse(input: Option[String]): Either[Throwable, FrontMatter] =
    input.fold(Right(absent)): input =>
      if input.isEmpty
      then Right(empty)
      else decode(input)
    
  private def decode(input: String): Either[Throwable, FrontMatter] =
    try
      val yaml: Yaml = YamlReader.read(input)
      val result: FrontMatter = codec.decodeValue(yaml)
      result.setExtraKeys(extraKeys = yaml
        .asInstanceOf[Yaml.Mapping]
        .entries
        .filter(_._1 match
          case Yaml.Scalar(key, _) => !fieldNamesMangled.contains(key)
          case _ => true
        ))
      Right(result)
    catch
      case error: Throwable if NonFatal(error) => Left(error)
