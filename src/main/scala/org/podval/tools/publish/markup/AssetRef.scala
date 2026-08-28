package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

/** File refs on media IR (`img@src`, `video`/`audio`/`source@src`, `object@data`).
  * Not page links: no title-walk, backlinks, or tips. Lookup is `Pages.resolveAsset`. */
object AssetRef:
  object UnresolvedClass extends HtmlClass("unresolved-asset")

  private val WikiEmbedAttr: String = "data-wiki-embed"

  def markWikiEmbed(element: Xml.Element): Xml.Element =
    if resourceAttr(element).isDefined then element.set(WikiEmbedAttr, "true")
    else element.transform: el =>
      if resourceAttr(el).isDefined then el.set(WikiEmbedAttr, "true") else el

  def isWikiEmbed(element: Xml.Element): Boolean =
    element.get(WikiEmbedAttr).contains("true")

  def clearWikiEmbed(element: Xml.Element): Xml.Element =
    element.set(WikiEmbedAttr, "")

  def resourceAttr(element: Xml.Element): Option[String] =
    element.getName match
      case "img" | "video" | "audio" | "source" => Some("src")
      case "object" => Some("data")
      case _ => None
