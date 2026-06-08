package org.podval.tools.publish.processor

open class Feature(
  val converter: Option[ConverterWithIds] = None,
  val transformer: Option[Transformer] = None,
  val postConverter: Option[PostConverter] = None,
  val htmlConverter: Option[HtmlConverter] = None
)
