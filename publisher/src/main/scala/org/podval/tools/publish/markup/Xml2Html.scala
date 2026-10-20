package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlAttribute, XmlElement, XmlName}

object Xml2Html:
  // Names that cannot be reused as-is when TEI (or similar) is serialized as HTML:
  // - `head`/`body`/`title` are HTML document chrome.
  // - HTML `p` is phrasing-content only. The parser auto-closes it before any block
  //   (`p`, `div`, `ul`/`ol`/`dl`, `table`, `blockquote`, `figure`, headings, …).
  //   TEI `p` may contain those, and footnote/glossary tips copy body nodes that may
  //   too, so it is renamed (`tei-p`). TEI `div` stays `div`.
  private val reservedHtmlElements: Set[String] = Set(
    XmlElement.Head,
    XmlElement.Body,
    XmlElement.Title,
    XmlElement.P
  ).map(_.localName)

  private val reservedAttributes: Set[String] = Set(
    XmlAttribute.CssClass,
    XmlAttribute.Target,
    XmlAttribute.Lang,
    XmlAttribute.Frame
  ).map(_.localName)

// Prefix attribute and element names that collide with the HTML ones.
// Element and attribute sets match local names; `xml:*` is left as-is.
// Namespace-based styling didn't work: browser DOM elements seem to be
// in the HTML5 xhtml namespace unless `xmlns` attribute is present
// on that very element (no inheritance).
final class Xml2Html(val prefix: String):
  private def withPrefix(name: String): String = s"$prefix-$name"

  /** Local name after `convert`: reserved HTML tags get `$prefix-…`; others stay. */
  def elementName(elem: XmlElement): String =
    if Xml2Html.reservedHtmlElements.contains(elem.localName)
    then withPrefix(elem.localName)
    else elem.localName

  /** Attribute qName after `convert`: reserved HTML names get `$prefix-…`; `xml:*` stays. */
  def attributeName(attr: XmlAttribute): String = rewriteAttribute(attr.name).qName

  def is(element: Xml.Element, elem: XmlElement): Boolean =
    element.isNamed(elementName(elem)) || element.isElement(elem)

  /** Prefixed name first, then the catalog name — so HTML `class` added by
    * `renameKeepingClass` does not hide the original reserved `class`. */
  def get(element: Xml.Element, attr: XmlAttribute): Option[String] =
    element.get(attributeName(attr)).orElse(element.get(attr))

  def convert(element: Xml.Element): Xml.Element =
    val attributesConverted: Xml.Element = element.setAttributes(
      element.getAttributes.map((name, value) => (rewriteAttribute(name), value))
    )
    if !element.getName.localNameIn(Xml2Html.reservedHtmlElements)
    then attributesConverted
    else attributesConverted.renameKeepingClass(withPrefix(element.getName.localName))

  private def rewriteAttribute(name: XmlName): XmlName =
    if name.isXml || !name.localNameIn(Xml2Html.reservedAttributes)
    then name
    else XmlName(withPrefix(name.localName))
