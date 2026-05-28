package org.podval.tools.publish

import org.podval.xml.{Xml, XmlAst, XmlParser}

object Tei extends Markup:
  override val extension: String = "xml"
  override val additionalExtensions: Set[String] = Set.empty
  override protected def recognizeWikiLinks: Boolean = false
  override protected def recognizeBlocks: Boolean = false
  override protected def stop(xml: XmlAst)(element: xml.Element): Boolean = false

  override protected def isSectionElement(element: Xml.Element): Boolean =
    Xml.qName(element) == "div"

  override protected def sectionTitle(element: Xml.Element): Option[String] = Xml
    .children(element)
    .flatMap(Xml.asElement)
    .find(element => Xml.qName(element) == "head")
    .flatMap(Xml.toStringOpt)

  override protected def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty // TODO

  override protected def convertLinks(element: Xml.Element): Xml.Element =
    element // TODO

  override def parse(
    content: String,
    errorReporter: PageError.Reporter
  ): Xml.Element = XmlParser.parse(content) match
    case Right(xml) => Xml.asElement(xml).get
    case Left(error) =>
      errorReporter.error(PageError.Parsing, "TEI parsing error", Some(error))
      malformedTei(error)

  private def malformedTei(error: Throwable): Xml.Element =
    var result = Xml.element("TEI")
    result = Xml.ClassName.add(result, "malformed-xml")
    result = Xml.setText(result, s"Malformed TEI: $error")
    result
