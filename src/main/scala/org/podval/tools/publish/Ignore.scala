package org.podval.tools.publish

import java.io.File

// TODO use `.gitignore`, internal and `_site_ignore`
final class Ignore(site: Site):
  private val include: Set[String] =
    Set(site.postsDirectoryName) ++
    site.draftsDirectoryName.toSet ++
    site.dailyNotesDirectoryName.toSet

  def isIgnored(file: File): Boolean =
    val name: String = file.getName
    if include.contains(name) then false
    else if site.config.exclude.contains(name) then
      site.log.debug(s"ignored: $name")
      true
    else
      Ignore.special.contains(name) ||
      Ignore.specialStartsWith.exists(name.startsWith)

object Ignore:
  private val special: Set[String] = Set(
    ".jekyll-cache",
    ".sass-cache",
    "Gemfile",
    "Gemfile.lock",
    "LICENSE",
    "README.md",
    "build",
    "build.gradle",
    "bundle",
    "gradle",
    "gradlew",
    "gradlew.bat",
    "node_modules",
    "settings.gradle",
    "src",
    "vendor",
  )

  private val specialStartsWith: Set[String] = Set(
    ".",
    "_",
    "~",
    "#"
  )
