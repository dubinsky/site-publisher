package org.podval.tools.publish.util

import org.scalatest.funsuite.AnyFunSuite

final class SiteOptionsSpec extends AnyFunSuite:
  test("positional source directory and defaults") {
    val options: SiteOptions = SiteOptions.forArgs(Array("/path/to/source"))
    assert(options.sourceDirectoryPath == "/path/to/source")
    assert(options.targetDirectoryName == "_site")
    assert(options.draftsDirectoryName.isEmpty)
    assert(!options.treatErrorsAsWarnings)
    assert(!options.production)
    assert(options.logLevel == "DEBUG")
  }

  test("--name=value and boolean flags without a value") {
    val options: SiteOptions = SiteOptions.forArgs(Array(
      "/src",
      "--target-directory-name=/abs/out",
      "--log-level=INFO",
      "--include-drafts",
      "--treat-errors-as-warnings",
      "--production"
    ))
    assert(options.sourceDirectoryPath == "/src")
    assert(options.targetDirectoryName == "/abs/out")
    assert(options.logLevel == "INFO")
    assert(options.draftsDirectoryName.contains("_drafts"))
    assert(options.treatErrorsAsWarnings)
    assert(options.production)
  }

  test("--include-drafts=false is off") {
    val options: SiteOptions = SiteOptions.forArgs(Array("/src", "--include-drafts=false"))
    assert(options.draftsDirectoryName.isEmpty)
  }

  test("Options.option is None for an unknown name") {
    val raw: Options = Options(Array("/src", "--log-level=WARN"), "SITE_PUBLISHER")
    assert(raw.positional(0) == "/src")
    assert(raw.option("log-level").contains("WARN"))
    assert(raw.option("missing").isEmpty)
    assert(!raw.booleanOption("production"))
  }
