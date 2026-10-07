package org.podval.tools.publish.page

import org.podval.tools.publish.util.{Date, Icon, SchemaUtil}
import zio.blocks.chunk.Chunk
import zio.blocks.schema.yaml.{Yaml, YamlCodec, YamlFormat, YamlReader, YamlTag, YamlWriter}
import zio.blocks.schema.{NameMapper, Schema}
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

// `extraKeys`, `modifiedTime`, and `absent` are case-class fields so `equals` and `copy` keep them.
// They are not YAML fields: the codec spells record fields in kebab-case, and Obsidian writes `modified_time`.
final case class FrontMatter(
  fields: FrontMatter.Fields = FrontMatter.Fields(),
  extraKeys: Chunk[(Yaml, Yaml)] = Chunk.empty,
  modifiedTime: Option[Date] = None,
  absent: Boolean = false
):
  export fields.{
    title, description, author, lang, math, tags, categories, aliases, permalink,
    post, postTitle, date, icon, iconStyle, tocDepth, chunk, chunkDepth, pdf, asset,
    bibliography, csl
  }

  // Debug dump of the decoded fields plus stashed keys.
  // Omits default `false` and empty lists.
  // Author files are not rewritten here; pretty-print keeps the original fence.
  def write: String = if absent then "" else
    val entries: Chunk[(Yaml, Yaml)] = FrontMatter.knownEntries(fields) ++ extraKeys
    val body: String = if entries.isEmpty then "" else YamlWriter.write(Yaml.Mapping(entries)) + "\n"
    s"---\n$body---\n"

object FrontMatter:
  final case class Fields(
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
    tocDepth: Option[Int] = None,
    chunk: Boolean = false,
    chunkDepth: Option[Int] = None,
    pdf: Boolean = false,
    // Stand-alone sidecar only: copy the markup file as an asset (`Pages.forName`).
    asset: Boolean = false,
    bibliography: Option[String] = None,
    csl: Option[String] = None,
  )

  private val standAloneExtensions: Seq[String] = Seq("yaml", "yml")
  def isStandAloneExtension(extension: Option[String]): Boolean = extension.exists(standAloneExtensions.contains)

  val empty: FrontMatter = FrontMatter()

  val absent: FrontMatter = FrontMatter(absent = true)

  private val schema: Schema[Fields] = Schema.derived

  private val fieldNamesMangled: Set[String] = SchemaUtil.fieldNames(schema).map(NameMapper.KebabCase.apply)

  private val codec: YamlCodec[Fields] = schema
    .deriving(YamlFormat.deriver)
    .instance(TypeId.of[Date], Date.codec)
    .instance(TypeId.of[Icon.Style], Icon.Style.codec)
    .derive

  private def knownEntries(fields: Fields): Chunk[(Yaml, Yaml)] =
    codec.encodeValue(fields).asInstanceOf[Yaml.Mapping].entries.filterNot(isOmittedDefault)

  private def isOmittedDefault(entry: (Yaml, Yaml)): Boolean = entry._2 match
    case Yaml.Scalar("false", Some(YamlTag.Bool)) => true
    case Yaml.Sequence(elements) if elements.isEmpty => true
    case _ => false

  // `modified_time` cannot be a field: the codec would look for `modified-time`.
  // A value that is not a date stays in `extraKeys` and leaves `modifiedTime` empty.
  private def modifiedTimeOf(extraKeys: Chunk[(Yaml, Yaml)]): Option[Date] =
    extraValue(extraKeys, "modified_time").flatMap: value =>
      try Some(Date.codec.decodeValue(value))
      catch case NonFatal(_) => None

  private def extraValue(extraKeys: Chunk[(Yaml, Yaml)], name: String): Option[Yaml] =
    extraKeys.collectFirst:
      case (Yaml.Scalar(key, _), value) if key == name => value

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
      val fields: Fields = codec.decodeValue(yaml)
      val extraKeys: Chunk[(Yaml, Yaml)] = yaml match
        case Yaml.Mapping(entries) => entries.filter(_._1 match
          case Yaml.Scalar(key, _) => !fieldNamesMangled.contains(key)
          case _ => true
        )
        case _ => Chunk.empty
      Right(FrontMatter(fields, extraKeys, modifiedTimeOf(extraKeys)))
    catch
      case error: Throwable if NonFatal(error) => Left(error)
