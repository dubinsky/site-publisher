package org.podval.tools.publish.site

import org.podval.tools.publish.util.SiteOptions
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class IgnoreSpec extends AnyFunSuite:
  private val repositoryRoot: File =
    val anchor = getClass.getResource("/anchor.txt")
    require(anchor != null, "missing test resource /anchor.txt")
    File(anchor.toURI)
      .getParentFile
      .getParentFile
      .getParentFile
      .getParentFile

  private lazy val ignore: Ignore = Site(SiteOptions(
    sourceDirectoryPath = File(repositoryRoot, "src/test/site").getAbsolutePath,
    treatErrorsAsWarnings = true,
    logLevelOpt = Some("WARN")
  )).ignore

  test("*.bib is ignored at any depth; markup is not") {
    assert(ignore.isIgnored("/library.bib", false))
    assert(ignore.isIgnored("/paper/refs.bib", false))
    assert(ignore.isIgnored("/_bibliography.bib", false))
    assert(!ignore.isIgnored("/notes.md", false))
    assert(!ignore.isIgnored("/book.adoc", false))
    assert(!ignore.isIgnored("/about.html", false))
  }
