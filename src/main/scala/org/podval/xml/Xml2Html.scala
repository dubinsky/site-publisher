package org.podval.xml

object Xml2Html:
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

So, I prefix the names of the elements from non-HTML namespaces with the namespace prefix
if their names clash with the HTML namespace in a way that makes CSS styling difficult.
E.g.: HTML disallows tables within paragraphs, so to have a tooltip inside a TEI paragraph,
it needs to not be an HTML <p> (and of course, namespace is ignored...)
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

    if !HtmlElement.reservedElements.contains(name)
    then attributesConverted
    else XmlUtil.renameElement(withPrefix(name), attributesConverted)
