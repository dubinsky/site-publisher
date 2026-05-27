package org.podval.tools.publish.util

object Media:
  private val imageExtensions: Set[String] = Set("jpg")
  def isImage(extension: String): Boolean = imageExtensions.contains(extension)

  private val audioExtensions: Set[String] = Set("ogg")
  def isAudio(extension: String): Boolean = audioExtensions.contains(extension)

  def icon(extension: Option[String]): Option[Icon] = extension.flatMap(icons.get)
  private val icons: Map[String, Icon] = Map(
    "pgp" -> Icon.key,
    "gpg" -> Icon.key,
    "pub" -> Icon.key,
    "css" -> Icon("css", Icon.Brands)
  )

