package org.podval.tools.publish.markup

/** Ordered children of a TEI `store` / `collection`. `hrefs` are page references, not XInclude. */
final class StoreIndex(
  val selector: Option[String],
  val hrefs: Seq[String],
  val names: Seq[StoreIndex.Name]
):
  def displayName: Option[String] =
    names.find(_.lang.contains("ru")).orElse(names.headOption).map(_.n)

object StoreIndex:
  final class Name(
    val n: String,
    val lang: Option[String]
  )
