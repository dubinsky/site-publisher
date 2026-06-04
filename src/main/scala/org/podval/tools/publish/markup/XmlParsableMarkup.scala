package org.podval.tools.publish.markup

import org.podval.tools.publish.PageError
import org.podval.xml.{HtmlClass, Xml, XmlElement, XmlParser}

trait XmlParsableMarkup extends Markup:
  final override def parse(
    content: String,
    errorReporter: PageError.Reporter
  ): Xml.Element = XmlParser.parse(content) match
    case Right(node) => node.asElement.get
    case Left(error) =>
      errorReporter.error(PageError.Parsing, s"$name parsing error", Some(error))

      Xml
        .element(XmlElement(xmlDialect.root.head))
        .add(HtmlClass(s"malformed-$extension"))
        .setText(s"Malformed $name: $error")
