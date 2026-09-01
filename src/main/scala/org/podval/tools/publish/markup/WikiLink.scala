package org.podval.tools.publish.markup

import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.{HtmlClass, Xml, XmlAttribute, XmlElement}

object WikiLink:
  private object WikiLinkClass extends HtmlClass("wiki-link")

  private object TranscludeClass extends HtmlClass("transclude")

  private[markup] val startTransclusion: String = "![["
  private[markup] val startLink: String = "[["
  private[markup] val end: String = "]]"

  def isTranscluded(element: Xml.Element): Boolean = element.has(TranscludeClass)

  def make(transclude: Boolean, ref: String, title: Option[String]): Xml.Element = Xml
    .element(XmlElement.A)
    .add(WikiLinkClass)
    .add(Option.when(transclude)(TranscludeClass))
    .set(XmlAttribute.Href, Option.when(ref.nonEmpty)(ref))
    .setText(wikiLinkText(transclude, title.getOrElse(ref)))

  private[markup] def wikiLinkStart(transclude: Boolean): String =
    if transclude then startTransclusion else startLink

  private[markup] def wikiLinkText(transclude: Boolean, text: String): String =
    s"${wikiLinkStart(transclude)}$text$end"

  def linkText(element: Xml.Element, text: String): String =
    if element.has(WikiLinkClass)
    then wikiLinkText(isTranscluded(element), text)
    else text

  // see https://obsidian.md/help/embeds
  def embed(element: Xml.Element, ref: String): Option[Xml.Element] =
    val (path: String, _) = Strings.splitFirst(ref, '#')
    val embedded: Option[Xml.Element] = Files.nameAndExtension(path)._2.map(_.toLowerCase) match
      case Some(extension) if Media.isImage(extension) =>
        // Note: FlexMark inlines image links for the ![]() references, so I do not get to do it -
        // and it does not process image sizes...
        val (width: Option[Int], height: Option[Int]) = imageSize(wikiEmbedInner(element))
        Some(Xml
          .element("img")
          .set("src", path)
          .set("alt", s"Image: $path")
          .set("width", width.map(_.toString))
          .set("height", height.map(_.toString))
        )
      case Some(extension) if Media.isAudio(extension) =>
        Some(Xml
          .element("audio")
          .set("src", path)
          .set("controls", true.toString)
        )
      case Some(extension) if Media.isVideo(extension) =>
        Some(Video.make(path, embedLabel(element, path)))
      case Some("pdf") =>
        Some(PdfEmbed.fromRef(ref, embedLabel(element, path)))
      case _ =>
        None
    embedded.map(AssetRef.markWikiEmbed)

  private def embedLabel(element: Xml.Element, path: String): String =
    val name: String = wikiEmbedInner(element)
    val fileName: String = path.split('/').lastOption.getOrElse(path)
    val ext: Option[String] = Files.nameAndExtension(name)._2.map(_.toLowerCase)
    if name.isEmpty || name == path || ext.exists(isMediaExtension) then fileName
    else name

  // Obsidian `![[image|WIDTH]]` / `![[image|WIDTHxHEIGHT]]` (pixels). Alias is the wiki-link text.
  private val imageSizeBoth = raw"(\d+)[xX](\d+)".r
  private val imageSizeWidth = raw"(\d+)".r

  private def wikiEmbedInner(element: Xml.Element): String =
    val inner: String = element.getText.trim.stripPrefix(startTransclusion).stripSuffix(end).trim
    Strings.splitFirst(inner, '#')._1.trim

  private def imageSize(text: String): (Option[Int], Option[Int]) =
    text.trim match
      case imageSizeBoth(width, height) =>
        (positivePx(width), positivePx(height)) match
          case (Some(w), Some(h)) => (Some(w), Some(h))
          case _ => (None, None)
      case imageSizeWidth(width) => (positivePx(width), None)
      case _ => (None, None)

  private def positivePx(raw: String): Option[Int] =
    raw.toIntOption.filter(_ > 0)

  private def isMediaExtension(extension: String): Boolean =
    Media.isImage(extension) || Media.isAudio(extension) || Media.isVideo(extension) || extension == "pdf"
