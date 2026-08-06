package org.podval.tools.publish.markup

abstract class Sections(val sections: Seq[Section]):
  final def resolve(
    result: Seq[Section],
    names: Seq[String],
    includeNested: Boolean
  ): Option[Seq[Section]] =

    def next(section: Section, includeNested: Boolean) = section.resolve(
      result = result :+ section,
      names = names.tail,
      includeNested = includeNested
    )

    if names.isEmpty then Some(result) else sections
      .find(section => section.title == names.head || section.id == names.head)
      .flatMap(section => next(section, includeNested = false))
      .orElse:
        if !includeNested then None else sections
          .flatMap(section => next(section, includeNested = true))
          .headOption
