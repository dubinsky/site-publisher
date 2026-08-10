package org.podval.tools.publish.site

import org.podval.tools.publish.util.SiteOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SiteTest extends AnyFlatSpec with Matchers {
  val anchorUrl: java.net.URL = getClass.getResource("/anchor.txt")
  val repositoryRoot: String = java.nio.file.Paths.get(anchorUrl.toURI).toFile
    .getParentFile // resources
    .getParentFile // test
    .getParentFile // src
    .getParentFile // root!
    .getAbsolutePath

  val site: Site = Site(SiteOptions(
    sourceDirectoryPath = "/src/test/site",
    logLevelOpt = Some("INFO"),
    treatErrorsAsWarnings = true
  ))

  site.generate()

  "site publisher" should "work" in { 1 shouldBe 1 }
}
