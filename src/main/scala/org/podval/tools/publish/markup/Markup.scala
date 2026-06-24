package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.{ConverterWithIds, HtmlConverter, PostConverter, SingleProcessor, Transformer}

final class Markup(
  val kind: MarkupKind,
  processors: Seq[SingleProcessor]
) derives CanEqual:
  lazy val converters: Seq[ConverterWithIds] = processors
    .collect { case converter: ConverterWithIds => converter }
    .sortBy(_.convertLinks)

  lazy val transformers: Seq[Transformer] = processors
    .collect { case transformer: Transformer => transformer }
    .sortBy(_.transformsFootnotes)

  lazy val postConverters: Seq[PostConverter] = processors
    .collect { case postConverter: PostConverter => postConverter }

  lazy val htmlConverters: Seq[HtmlConverter] = processors
    .collect { case htmlConverter: HtmlConverter => htmlConverter }
