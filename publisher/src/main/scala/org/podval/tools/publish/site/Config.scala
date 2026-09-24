package org.podval.tools.publish.site

import zio.blocks.schema.Schema
import zio.blocks.schema.yaml.{YamlCodec, YamlFormat}

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
  val license: Option[String] = None,
  val licenseLink: Option[String] = None,
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

  private val schema: Schema[Config] = Schema.derived

  val codec: YamlCodec[Config] = schema
    .deriving(YamlFormat.deriver)
    .derive
