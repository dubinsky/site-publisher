package org.podval.tools.publish.prose

/** Copy a closed YAML or TOML fence. An unclosed `+++` is not parsed. An unclosed `---` stays in the body. */
object FrontMatterPeel:
  def yaml(raw: String): (String, String) =
    if !raw.startsWith("---\n") then ("", raw)
    else raw.indexOf("\n---\n", 3) match
      case -1 => ("", raw)
      case end =>
        val cut: Int = end + "\n---\n".length
        (raw.take(cut), raw.drop(cut))

  def prose(raw: String): Either[ProseFailure, (String, String)] =
    if raw.startsWith("+++\n") then
      raw.indexOf("\n+++\n", 4) match
        case -1 => Left(ProseFailure.Malformed("unclosed-front-matter"))
        case end =>
          val cut: Int = end + "\n+++\n".length
          Right((raw.take(cut), raw.drop(cut)))
    else Right(yaml(raw))
