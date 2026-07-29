package org.podval.tools.publish.markup

import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml}

object Links:
  object BlockClass extends HtmlClass("wiki-block")

  private object WikiLinkClass extends HtmlClass("wiki-link")

  private object TranscludeClass extends HtmlClass("transclude")

  object InternalLinkClass extends HtmlClass("internal-link")

  // TODO remove; add/check classes directly
  def markBlock(element: Xml.Element): Xml.Element = element.add(Links.BlockClass)
  def isTranscluded(element: Xml.Element): Boolean = element.has(TranscludeClass)

  object WikiLink:
    val startTransclusion: String = "![["
    val startLink: String = "[["
    val end: String = "]]"

    def wikiLinkStart(transclude: Boolean): String = if transclude then startTransclusion else startLink
    
    // TODO move outside of WikiLink
    def wikiLinkText(transclude: Boolean, text: String) = s"${wikiLinkStart(transclude)}$text$end"
  
  def wikiLink(
    transclude: Boolean,
    ref: String,
    title: Option[String]
  ): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(Links.WikiLinkClass)
    .add(Option.when(transclude)(Links.TranscludeClass))
    .set(HtmlAttribute.Href, Option.when(ref.nonEmpty)(ref))
    .setText(WikiLink.wikiLinkText(transclude, title.getOrElse(ref)))

  def linkText(element: Xml.Element, text: String): String =
    if element.has(Links.WikiLinkClass)
    then WikiLink.wikiLinkText(isTranscluded(element), text)
    else text
