package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, ObsidianConfig}
import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.schema.Schema
import zio.blocks.schema.yaml.{YamlCodec, YamlFormat}
import java.io.File

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
  val social: Config.Social = Config.Social(),
  val exclude: List[String] = List.empty,
):
  // Set via method to avoid confusing the codec.
  private var sourceDirectoryVar: Option[File] = None
  def sourceDirectory: File = sourceDirectoryVar.get

  private var targetDirectoryVar: Option[File] = None
  def targetDirectory: File = targetDirectoryVar.get
  
  private var includeDraftsVar: Option[Boolean] = None
  private def includeDrafts: Boolean = includeDraftsVar.get
  
  def setExternal(
    sourceDirectory: File, 
    targetDirectory: File,
    includeDrafts: Boolean
  ): Unit =
    sourceDirectoryVar = Some(sourceDirectory)
    targetDirectoryVar = Some(targetDirectory)
    includeDraftsVar = Some(includeDrafts)

  private lazy val obsidianConfig: ObsidianConfig = ObsidianConfig(sourceDirectory)

  def postsDirectoryName: String = "_posts"
  def draftsDirectoryName: Option[String] = Option.when(includeDrafts)("_drafts")
  def dailyNotesDirectoryName: Option[String] = obsidianConfig.daysFolder

  private lazy val includedSet: Set[String] =
    Set(postsDirectoryName) ++
    draftsDirectoryName.toSet ++
    dailyNotesDirectoryName.toSet

  private val excludeSet: Set[String] = exclude.toSet

  def exclude(file: File): Boolean =
    val name: String = file.getName
    if includedSet.contains(name) then false
    else if excludeSet.contains(name) then
      Config.log.debug(s"excluded: $name")
      true
    else
      Config.special.contains(name) ||
      Config.specialStartsWith.exists(name.startsWith)

  def socialLinks: Seq[SocialLink] = Seq(
    social.github.map(SocialLink.GitHub(_)),
    social.twitter.map(SocialLink.Twitter(_)),
    social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten
  
object Config:
  final class Social(
    val github: Option[String] = None,
    val twitter: Option[String] = None,
    val linkedin: Option[String] = None
  )

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  
  private val special: Set[String] = Set(
    ".jekyll-cache",
    ".sass-cache",
    "Gemfile",
    "Gemfile.lock",
    "LICENSE",
    "README.md",
    "build",
    "build.gradle",
    //    "bundle",
    "gradle",
    "gradlew",
    "gradlew.bat",
    "node_modules",
    "settings.gradle",
    "src",
    "tags.html", // my Jekyll customization; I am going to synthesize it
    "vendor",
  )

  private val specialStartsWith: Set[String] = Set(
    ".",
    "_",
    "~",
    "#"
  )

  private val schema: Schema[Config] = Schema.derived

  private val codec: YamlCodec[Config] = schema
    .deriving(YamlFormat.deriver)
    .derive

  val fileName: String = "_site_config.yml"

  def apply(
    sourceDirectoryPath: String,
    targetDirectoryName: String,
    includeDrafts: Boolean
  ): Config =
    val sourceDirectory: File =
      val result: File = File(sourceDirectoryPath).getAbsoluteFile

      Files.requireExists(result)
      Files.requireDirectory(result)

      log.info(s"source directory: $result")
      result

    val targetDirectory: File =
      val result: File = File(sourceDirectory, targetDirectoryName)
      result.mkdirs()
      Files.requireExists(result)
      Files.requireDirectory(result)
      result

    val configFile: File =
      val result: File = File(sourceDirectory, Config.fileName)
      Files.requireExists(result)
      Files.requireFile(result)
      log.info(s"configuration file: $result")
      result

    codec.decode(Files.read(configFile)) match
      case Left(error) => throw IllegalArgumentException("Malformed Config", error)
      case Right(result) =>
        result.setExternal(
          sourceDirectory = sourceDirectory,
          targetDirectory = targetDirectory,
          includeDrafts = includeDrafts
        )
        log.debug(s"Config:\n" + codec.encodeToString(result))
        result
