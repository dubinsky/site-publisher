package org.podval.tools.publish.util

final class SiteOptions(
  val sourceDirectoryPath: String,
  _targetDirectoryName: Option[String] = None,
  includeDrafts: Boolean = false,
  val treatErrorsAsWarnings: Boolean = false,
  val production: Boolean = false,
  _logLevel: Option[String] = None
):
  def targetDirectoryName: String = _targetDirectoryName.getOrElse("_site")
  def draftsDirectoryName: Option[String] = Option.when(includeDrafts)("_drafts")
  def logLevel: String = _logLevel.getOrElse("DEBUG")

object SiteOptions:
  def forArgs(args: Array[String]): SiteOptions = forOptions(
    Options(args, environmentVariablesPrefix = "SITE_PUBLISHER")
  )
    
  def forOptions(options: Options): SiteOptions = SiteOptions(
    sourceDirectoryPath = options.positional(0),
    _targetDirectoryName = options.option("target-directory-name"),
    includeDrafts = options.booleanOption("include-drafts"),
    treatErrorsAsWarnings = options.booleanOption("treat-errors-as-warnings"),
    production = options.booleanOption("production"),
    _logLevel = options.option("log-level")
  )
  