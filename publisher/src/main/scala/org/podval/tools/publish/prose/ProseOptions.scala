package org.podval.tools.publish.prose

import org.podval.tools.publish.util.Options

/** Column budget and whether each sentence starts a line. */
final case class ProseOptions(
  width: Int = ProseOptions.DefaultWidth,
  sentencePerLine: Boolean = true
)

object ProseOptions:
  val DefaultWidth: Int = 120

  /** `--width=N` and `--sentence-per-line`. A bad value is a message, not a write. */
  def from(options: Options): Either[String, ProseOptions] =
    width(options).flatMap: parsed =>
      sentencePerLine(options).map(sentences => ProseOptions(parsed, sentences))

  private def width(options: Options): Either[String, Int] =
    options.option("width") match
      case None => Right(DefaultWidth)
      case Some(value) if value.nonEmpty && value.forall(_.isDigit) => Right(value.toInt)
      case Some(_) => Left("width")

  private def sentencePerLine(options: Options): Either[String, Boolean] =
    options.option("sentence-per-line") match
      case None => Right(true)
      case Some("true") => Right(true)
      case Some("false") => Right(false)
      case Some(_) => Left("sentence-per-line")
