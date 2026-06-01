package org.podval.xml

import zio.blocks.html.Dom as XML
import zio.blocks.schema.xml.Xml as From

// TODO express better in the AST terms
object Xml2Html:
  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def fromXml(element: From.Element): Html.Element = XML.Element.Generic(
    tag = element.name.qualifiedName,
    attributes = element.attributes.map((name, value) => Html.mkAttribute(name.qualifiedName, value)),
    children = element.children.flatMap {
      case From.Comment(value) => None
      case From.ProcessingInstruction(target, data) => None
      case From.Text(value) => Some(Html.text(value))
      case From.CData(value) => Some(Html.text(value))
      case element: From.Element => Some(fromXml(element))
    }
  )

  
