package org.podval.tools.publish

import org.podval.xml.{HtmlClass, Xml, XmlElement, XmlParser}

object HtmlMarkup extends HtmlLikeMarkup:
  override val extension: String = "html"
  override val additionalExtensions: Set[String] = Set.empty

  override protected def toHtml(element: Xml.Element): Xml.Element = element

  override def parse(
    content: String,
    errorReporter: PageError.Reporter
  ): Xml.Element = XmlParser.parse(content) match
    case Right(node) => node.asElement.get
    case Left(error) =>
      errorReporter.error(PageError.Parsing, "HTML parsing error", Some(error))
      malformedHtml(error)

  private def malformedHtml(error: Throwable): Xml.Element = Xml
    .element(XmlElement("div"))
    .add(HtmlClass("malformed-xml"))
    .setText(s"Malformed HTML: $error")
