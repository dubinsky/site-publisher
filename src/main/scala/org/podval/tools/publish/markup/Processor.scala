package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlDialect}
import scala.annotation.tailrec

trait Processor

object Processor:
  def process(
    xmlDialect: XmlDialect,
    element: Xml.Element,
    processors: Seq[Processor]
  ): Xml.Element =
    @tailrec
    def process(
      element: Xml.Element,
      processors: Seq[Processor]
    ): Xml.Element = processors match
      case Nil =>
        element
      case (transformer: Transformer) :: tail =>
        process(transformer.transform(element), tail)
      case _ =>
        val (converters: Seq[Processor], tail: Seq[Processor]) = processors.span(_.isInstanceOf[Converter])
        def convert(element: Xml.Element): Xml.Element = converters
          .map(_.asInstanceOf[Converter])
          .foldLeft(element)((result, converter) => converter.convert(result).getOrElse(result))
        process(xmlDialect.transform(element, convert), tail)
        
    process(element, processors)