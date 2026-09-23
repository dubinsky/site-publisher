package org.podval.tools.publish.util

final class SiteOptions(
  val sourceDirectoryPath: String,
  targetDirectoryNameOpt: Option[String] = None,
  includeDrafts: Boolean = false,
  val treatErrorsAsWarnings: Boolean = false,
  val production: Boolean = false,
  val serve: Boolean = false,
  logLevelOpt: Option[String] = None,
  val prettyPrint: Boolean = false
):
  def targetDirectoryName: String = targetDirectoryNameOpt.getOrElse("_site")
  def draftsDirectoryName: Option[String] = Option.when(includeDrafts)("_drafts")
  def logLevel: String = logLevelOpt.getOrElse("INFO")

object SiteOptions:
  def forArgs(args: Array[String]): SiteOptions = forOptions(
    Options(args, environmentVariablesPrefix = "SITE_PUBLISHER")
  )
    
  def forOptions(options: Options): SiteOptions = SiteOptions(
    sourceDirectoryPath = options.positional(0),
    targetDirectoryNameOpt = options.option("target-directory-name"),
    includeDrafts = options.booleanOption("include-drafts"),
    treatErrorsAsWarnings = options.booleanOption("treat-errors-as-warnings"),
    production = options.booleanOption("production"),
    serve = options.booleanOption("serve"),
    logLevelOpt = options.option("log-level"),
    prettyPrint = options.booleanOption("pretty-print")
  )
  