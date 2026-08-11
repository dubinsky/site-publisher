package org.podval.tools.publish.util

object Media:
  private val imageExtensions: Set[String] = Set("jpg", "jpeg", "png", "gif", "webp", "svg", "ico")
  def isImage(extension: String): Boolean = imageExtensions.contains(extension.toLowerCase)

  private val audioExtensions: Set[String] = Set("ogg", "mp3", "wav", "m4a")
  def isAudio(extension: String): Boolean = audioExtensions.contains(extension.toLowerCase)

  def icon(extension: Option[String]): Option[Icon] = extension.map(_.toLowerCase).flatMap(icons.get)
  private val icons: Map[String, Icon] = Map(
    "pgp" -> Icon.key,
    "gpg" -> Icon.key,
    "pub" -> Icon.key,
    "css" -> Icon("css", Icon.Brands)
  )

