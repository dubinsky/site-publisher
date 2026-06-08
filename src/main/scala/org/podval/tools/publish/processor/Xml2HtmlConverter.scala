package org.podval.tools.publish.processor

import org.podval.xml.{HtmlAttribute, HtmlElement, Xml}

// Prefix attribute and element names that collide with the HTML ones
abstract class Xml2HtmlConverter(prefix: String) extends ConverterSimple:
  private def withPrefix(name: String): String = s"$prefix-$name"

  // Dialect-specific conversions
  protected def convertMore(element: Xml.Element): Xml.Element = element

  final override protected def convert(element: Xml.Element): Xml.Element =
    val attributesConverted: Xml.Element = element.setAttributes(element.getAttributes.map((name, value) =>
      val nameNew: String =
        if !HtmlAttribute.reservedAttributes.contains(name)
        then name
        else withPrefix(name)
      (nameNew, value)
    ))

    val name: String = element.getName

    val elementsConverted: Xml.Element =
      if !HtmlElement.reservedElements.contains(name)
      then attributesConverted
      else renameElement(withPrefix(name), attributesConverted)

    convertMore(elementsConverted)
