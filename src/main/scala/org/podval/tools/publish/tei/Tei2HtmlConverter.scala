package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.Xml2HtmlConverter
import org.podval.xml.Xml

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
For instance, I use <div> to structure the layout, but need to be able to style TEI
depending on the level of nesting of TEI divs.
Also, HTML disallows tables within paragraphs, so to have a tooltip inside a TEI paragraph,
it needs to not be an HTML <p> (and of course, namespace is ignored...)
*/
private object Tei2HtmlConverter:
  private def withPrefix(name: String): String = s"tei-$name"

final class Tei2HtmlConverter extends Xml2HtmlConverter("tei"):
  protected override def convertMore(element: Xml.Element): Xml.Element = element.getName match
    case "row" =>
      renameElement("tr", element)

    case "cell" =>
      renameElement("td", copyAttribute("cols", "colspan", element))

    case "graphic" =>
      renameElement("image", copyAttribute("url", "src", element))

    case "ref" | "ptr" =>
      renameElement("a", copyAttribute("target", "href", element))

    case _ =>
      element
