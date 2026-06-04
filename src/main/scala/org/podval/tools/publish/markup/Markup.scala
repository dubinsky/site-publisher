package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.Features
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.Fragment
import org.podval.xml.{Xml, XmlDialect}

abstract class Markup derives CanEqual:
  def name: String

  final override def toString: String = name

  def extension: String

  def additionalExtensions: Set[String]

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(content: String, errorReporter: PageError.Reporter): Xml.Element

  // TODO use xmlDialect.plus(HtmlXmlDialect) for processing/printing
  // and xmlDialect for pretty-printing.
  def xmlDialect: XmlDialect

  def features: Features

  def sections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section]

object Markup:
  val xmlLike: List[XmlLikeMarkup] = List(
    TeiMarkup,
    TeiEntityMarkup,
    StoreMarkup
  )

  // Some XmlLike markups can have extensions other than `.xml`, so we add `xmlLike` to `all`:
  val all: List[Markup] = xmlLike ++ List(
    MarkdownMarkup,
    HtmlMarkup
  )
