package org.podval.tools.publish.prose

/** A markup that pretty-prints its own source. Absence means the extension is not prose. */
trait ProseFormatter:
  def format(source: String, options: ProseOptions): Either[ProseFailure, String]
  def kind: ProseKind

enum ProseKind derives CanEqual:
  case Markdown, AsciiDoc
