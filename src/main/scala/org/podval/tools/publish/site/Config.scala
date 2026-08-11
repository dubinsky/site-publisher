package org.podval.tools.publish.site

import zio.blocks.schema.Schema
import zio.blocks.schema.yaml.{YamlCodec, YamlFormat}

// TODO add favicon:
// opentorah: <link rel="icon" type="image/ico" href="/favicon.ico">;
// chumashquestions: <link rel="icon" type="image/png" href="/images/favicon-author.png">
// TODO add license:
// chumashquestions: <link rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/" title="CC by-nc-nd">
final class Config(
  val title: String,
  val description: String,
  val url: String,
  val author: String,
  val email: String,
  val timezone: Option[String] = None,
  val lang: Option[String] = None,
  val math: Boolean = false,
  val googleAnalytics: Option[String] = None,
  val social: Config.Social = Config.Social()
)

object Config:
  final class Social(
    val github: Option[String] = None,
    val twitter: Option[String] = None,
    val linkedin: Option[String] = None
  )

  private val schema: Schema[Config] = Schema.derived

  val codec: YamlCodec[Config] = schema
    .deriving(YamlFormat.deriver)
    .derive
