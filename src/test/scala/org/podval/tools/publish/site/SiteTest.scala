package org.podval.tools.publish.site

import org.podval.tools.publish.util.Options
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

  val siteRoot: String = repositoryRoot + "/src/test/site"
  val options: Options = Options(
    environmentVariablesPrefix = "Nooo!",
    args = Array(
      "--log-level=INFO",
      "--treat-errors-as-warnings=true",
      siteRoot
    )
  )
  val site: Site = Site(options)

  site.generate()

  "site publisher" should "work" in { 1 shouldBe 1 }
}
