package org.podval.tools.publish.prose

/** A prose file was not rewritten. `reason` is the skip text. */
enum ProseFailure(val reason: String) derives CanEqual:
  case Crlf extends ProseFailure("crlf")
  case Malformed(detail: String) extends ProseFailure(detail)
