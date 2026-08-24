package org.podval.xml

object Xml2Html:
  // Names that cannot be reused as-is when TEI (or similar) is serialized as HTML:
  // - `head`/`body`/`title` are HTML document chrome.
  // - HTML `p` is phrasing-content only. The parser auto-closes it before any block
  //   (`p`, `div`, `ul`/`ol`/`dl`, `table`, `blockquote`, `figure`, headings, …).
  //   TEI `p` may contain those, and footnote/glossary tips copy body nodes that may
  //   too, so it is renamed (`tei-p`). TEI `div` stays `div`.
  private val reservedHtmlElements: Set[String] = Set("head", "body", "title", "p")

  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def fromXml(element: Xml.Element): Html.Element = Html
    .element(element.getName)
    .setAttributes(element.getAttributes)
    .setChildren(element.getChildren.flatMap: child =>
      // ZIO Blocks HTML does not support comments nor processing instructions
      child.asElement.map(fromXml)
        .orElse(child.asAtom.map(Html.text))
    )

/*
I tried to define CSS namespaces like this:
@namespace tei   url("http://www.tei-c.org/ns/1.0");
@namespace db    url("http://docbook.org/ns/docbook");
@namespace xhtml url("http://www.w3.org/1999/xhtml");
and use them in CSS rules like this: tei|div, docbook|title.

It seems that in browser DOM all elements are in the HTML5 xhtml namespace
unless xmlns attribute is present on that element;
why are the namespace declarations not inherited is not clear.

So, I prefix names that clash with HTML (see Xml2Html.reservedHtmlElements).
In particular HTML `p` cannot contain blocks; TEI `p` can, so it becomes `tei-p`.
*/
// Prefix attribute and element names that collide with the HTML ones
final class Xml2Html(prefix: String):
  private def withPrefix(name: String): String = s"$prefix-$name"

  def convert(element: Xml.Element): Xml.Element =
    val attributesConverted: Xml.Element = element.setAttributes(element.getAttributes.map((name, value) =>
      val nameNew: String =
        if !HtmlAttribute.reservedAttributes.contains(name)
        then name
        else withPrefix(name)
      (nameNew, value)
    ))

    val name: String = element.getName

    if !Xml2Html.reservedHtmlElements.contains(name)
    then attributesConverted
    else XmlUtil.renameElement(withPrefix(name), attributesConverted)
