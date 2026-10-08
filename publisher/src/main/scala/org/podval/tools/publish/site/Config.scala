package org.podval.tools.publish.site

import org.podval.tools.publish.util.SchemaUtil
import zio.blocks.schema.Schema
import zio.blocks.schema.yaml.{Yaml, YamlCodec, YamlFormat, YamlReader}
import scala.util.control.NonFatal

final class Config(
  val title: String,
  val description: String,
  val url: String,
  val author: String,
  val email: String,
  val math: Boolean = false,
  val timezone: Option[String] = None,
  val lang: Option[String] = None,
  val favicon: Option[String] = None,
  val license: Option[Config.License] = None,
  val googleAnalytics: Option[String] = None,
  val paginatePosts: Option[Int] = None,
  val headerPages: List[String] = List.empty,
  val home: Option[String] = None,
  val facsimilesUrl: Option[String] = None,
  val namedWindows: Boolean = false,
  val checkLinks: Boolean = false,
  val teiDefaultCalendar: Option[String] = None,
  val social: Config.Social = Config.Social(),
  val graph: Config.Graph = Config.Graph()
)

object Config:
  final class Social(
    val github: Option[String] = None,
    val twitter: Option[String] = None,
    val linkedin: Option[String] = None
  )

  final class Graph(
    val enabled: Boolean = false,
    val includeTransclusions: Boolean = true,
    val excludePathPrefixes: List[String] = List.empty
  )

  final class License(
    val name: String,
    val link: Option[String] = None,
    val holder: Option[String] = None,
    val holderLink: Option[String] = None
  )

  private val schema: Schema[Config] = Schema.derived

  private val codec: YamlCodec[Config] = schema
    .deriving(YamlFormat.deriver)
    .derive

  def encodeToString(config: Config): String = codec.encodeToString(config)

  /** Decode `_site_config.yml`. An unknown key, including under `social`, `graph`, or `license`, is an error. */
  def decode(input: String): Either[Throwable, Config] =
    try
      val yaml: Yaml = YamlReader.read(input)
      val unknown: List[String] = SchemaUtil.unknownKeys(yaml, schema)
      if unknown.nonEmpty then Left(IllegalArgumentException(s"Unknown config keys: ${unknown.mkString(", ")}"))
      else
        val config: Config = codec.decodeValue(yaml)
        licenseError(config) match
          case Some(message) => Left(IllegalArgumentException(message))
          case None => Right(config)
    catch
      case error: Throwable if NonFatal(error) => Left(error)

  // Trim is checked here and applied at render. The YAML text is left as written.
  private def licenseError(config: Config): Option[String] = config.license.flatMap: license =>
    if license.name.trim.isEmpty then Some("license.name is blank")
    else if license.link.exists(_.trim.isEmpty) then Some("license.link is blank")
    else if license.holder.exists(_.trim.isEmpty) then Some("license.holder is blank")
    else if license.holderLink.exists(_.trim.isEmpty) then Some("license.holder-link is blank")
    else if license.holderLink.isDefined && license.holder.isEmpty then
      Some("license.holder-link requires license.holder")
    else None
