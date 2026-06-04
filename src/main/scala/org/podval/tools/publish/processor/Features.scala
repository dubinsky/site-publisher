package org.podval.tools.publish.processor

final class Features(features: Seq[Feature]):
  lazy val converters: Seq[Converter] = sort(_.converter)
  lazy val transformers: Seq[Transformer] = sort(_.transformer)
  lazy val postConverters: Seq[PostConverter] = sort(_.postConverter)
  lazy val htmlConverters: Seq[HtmlConverter] = sort(_.htmlConverter)

  private def sort[A <: Processor](what: Feature => Option[A]): Seq[A] = features.flatMap(what).sortBy(_.runLast)
  