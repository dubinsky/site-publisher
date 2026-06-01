package org.podval.tools.publish

import org.eclipse.jgit.ignore.IgnoreNode
import org.eclipse.jgit.ignore.IgnoreNode.MatchResult
import org.podval.tools.publish.util.Files
import java.io.{ByteArrayInputStream, File}

final class Ignore(site: Site):
  val rules: String =
    fromGitIgnore ++
    Ignore.internal ++
    internalIncludes ++
    fromSiteIgnore

  private def fromGitIgnore: String =
    val gitIgnoreFile = File(site.sourceDirectory, ".gitignore")
    if !gitIgnoreFile.exists
    then "# there is no '.gitignore' file\n"
    else "# from '.gitignore' file\n" ++ Files.read(gitIgnoreFile)

  private def fromSiteIgnore: String =
    val siteIgnoreFile = File(site.sourceDirectory, "_site_ignore")
    if !siteIgnoreFile.exists
    then "# there is no '_site_ignore' file\n"
    else "# from '_site_ignore' file\n" ++ Files.read(siteIgnoreFile)

  private def internalIncludes: String =
    "# internal un-ignore rules\n" ++
    s"!/${site.postsDirectoryName}\n" ++
    site.draftsDirectoryName.fold("")(name => s"!/$name\n") ++
    site.dailyNotesDirectoryName.fold("")(name => s"!/$name\n") ++
    "\n"

  private val ignoreNode: IgnoreNode = new IgnoreNode()
  ignoreNode.parse(ByteArrayInputStream(rules.getBytes))

  def isIgnored(path: String, isDirectory: Boolean): Boolean =
    given CanEqual[MatchResult, MatchResult] = CanEqual.derived
    ignoreNode.isIgnored(path, isDirectory) == MatchResult.IGNORED

object Ignore:
  private def internal: String =
    """
      |# internal ignore rules
      |
      |/.jekyll-cache/
      |/.sass-cache/
      |/Gemfile
      |/Gemfile.lock
      |/LICENSE
      |/README.md
      |/README.adoc
      |build/
      |build.gradle
      |/bundle/
      |/gradle/
      |/gradlew
      |/gradlew.bat
      |node_modules/
      |settings.gradle
      |src/
      |vendor/
      |
      |# special files
      |.*
      |_*
      |~*
      |\#*
      |
      |""".stripMargin
