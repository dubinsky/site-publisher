package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlClass, Xml, XmlAttribute}

// TODO split so that TEI-style entity references can be processed in non-TEI markup ;)
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
final class TeiFeature extends Feature(
  converter = Some(TeiFeature.TeiConverter())
)

object TeiFeature:
  private val facsimileSymbol: String = "⎙"

  private val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

  private def withPrefix(name: String): String = s"tei-$name"

  private final class TeiConverter extends Converter:
    override def convert(
      element: Xml.Element,
      content: PageContent,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      val name: String = element.getName
  
      val result: Xml.Element = element.setAttributes(element.getAttributes.map((name, value) =>
        val nameNew =
          if !reservedAttributes.contains(name)
          then name
          else withPrefix(name)
        (nameNew, value)
      ))
  
      name match
        case "head" | "body" | "title" | "div" | "p" =>
          renameElement(withPrefix(name), result)
  
        case "row" =>
          renameElement("tr", result)
  
        case "cell" =>
          renameElement("td", copyAttribute("cols", "colspan", result))
  
        case "graphic" =>
          renameElement("image", copyAttribute("url", "src", result))
  
        case "ref" | "ptr" =>
          renameElement("a", copyAttribute("target", "href", result))
  
        // TODO turn those into As *only* if 'ref' attribute is present!
        case "persName" | "placeName" | "orgName" =>
          renameElement("a", copyAttribute("ref", "href", result))
  
        case "pb" =>
          renameElement("a", result.setText(facsimileSymbol))
  
        // TODO tooltips on dates and gaps
  
        case _ => element
    
  private def renameElement(
    name: String,
    element: Xml.Element
  ): Xml.Element = element
    .add(HtmlClass(element.getName))
    .rename(name)

  private def copyAttribute(
    from: String,
    to: String,
    element: Xml.Element
  ): Xml.Element =
    element.get(XmlAttribute(from)).fold(element)(element.set(XmlAttribute(to), _))



  