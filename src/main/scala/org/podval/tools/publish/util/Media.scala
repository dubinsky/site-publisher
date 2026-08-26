package org.podval.tools.publish.util

object Media:
  private val imageExtensions: Set[String] = Set("jpg", "jpeg", "png", "gif", "webp", "svg", "ico")
  def isImage(extension: String): Boolean = imageExtensions.contains(extension.toLowerCase)

  private val audioExtensions: Set[String] = Set("ogg", "mp3", "wav", "m4a")
  def isAudio(extension: String): Boolean = audioExtensions.contains(extension.toLowerCase)

  private val videoExtensions: Set[String] = Set("mp4", "webm", "ogv", "m4v")
  def isVideo(extension: String): Boolean = videoExtensions.contains(extension.toLowerCase)

  def isAsset(extension: String): Boolean =
    val lower: String = extension.toLowerCase
    isImage(lower) || isAudio(lower) || isVideo(lower) || lower == "pdf"

  def icon(extension: Option[String]): Option[Icon] = extension.map(_.toLowerCase).flatMap(icons.get)
  private val icons: Map[String, Icon] = Map(
    "pgp" -> Icon.key,
    "gpg" -> Icon.key,
    "pub" -> Icon.key,
    "css" -> Icon("css", Icon.Brands)
  )

