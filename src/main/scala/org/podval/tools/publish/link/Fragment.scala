package org.podval.tools.publish.link

sealed abstract class Fragment(val id: String)

object Fragment:
  final class Block(
    id: String
  ) extends Fragment(id)

  final class Section(
    id: String,
    val title: String,
    val sections: Seq[Section]
  ) extends Fragment(id)
