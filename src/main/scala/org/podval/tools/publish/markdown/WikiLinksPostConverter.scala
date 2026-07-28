package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.{Links, PostConverter}
import org.podval.tools.publish.util.{Files, Media}
import org.podval.xml.Xml

// see https://obsidian.md/help/links
final class WikiLinksPostConverter extends PostConverter:
  override protected def postConvert(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.isA && Links.isTranscluded(element))(
      element.getHref.fold(element)(embed(element, _).getOrElse(element))
    )

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embed(element: Xml.Element, ref: String): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.fold(None): extension =>
      if Media.isImage(extension) then
        val (width: Option[Int], height: Option[Int]) =
          // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
          (None, None)

        Some(Xml
          .element("img")
          .set("src", ref)
          .set("alt", s"Image: $ref")
          .set("width", width.map(_.toString))
          .set("height", height.map(_.toString))
        )
      else if Media.isAudio(extension) then Some(Xml
        .element("audio")
        .set("src", ref)
        .set("controls", true.toString)
      )
      else if extension == "pdf" then
        // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
        None
      else
        None
