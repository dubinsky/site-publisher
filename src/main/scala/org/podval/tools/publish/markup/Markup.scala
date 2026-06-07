package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.Features
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.xml.{Xml, XmlDialect, XmlParser}
import javax.xml.stream.XMLStreamException

abstract class Markup derives CanEqual:
  def name: String

  final override def toString: String = name

  def extension: String

  def additionalExtensions: Set[String]

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)
  
  final def parse(content: String): Either[XMLStreamException, Xml.Element] =
    XmlParser.parse(xmlContent(content))

  def xmlContent(content: String): String

  // TODO use xmlDialect.plus(HtmlXmlDialect) for processing/printing
  // and xmlDialect for pretty-printing.
  def xmlDialect: XmlDialect

  def features: Features

  def sections(content: PageContent): Seq[Fragment.Section]

object Markup:
  val xmlLike: List[XmlLikeMarkup] = List(
    TeiMarkup
  )

  // Some XmlLike markups can have extensions other than `.xml`, so we add `xmlLike` to `all`:
  val all: List[Markup] = xmlLike ++ List(
    MarkdownMarkup,
    HtmlMarkup
  )
