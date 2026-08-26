package org.podval.tools.publish.markup

abstract class Sections(val sections: Seq[Section]):
  final def flatten: Seq[Section] = sections ++ sections.flatMap(_.flatten)
  
  final def resolve(
    result: Seq[Section],
    names: Seq[String],
    includeNested: Boolean
  ): Option[Seq[Section]] =

    if names.isEmpty then Some(result) else sections
      .find(section => section.title == names.head || section.id == names.head)
      .flatMap(section => section.resolve(
        result = result :+ section,
        names = names.tail,
        includeNested = false
      ))
      .orElse:
        if !includeNested then None else sections
          .flatMap(section => section.resolve(result, names, includeNested = true))
          .headOption
