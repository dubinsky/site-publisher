package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlUtil}

/** Ordered children of a TEI `store` / `collection`. `hrefs` are page references, not XInclude.
  * Harvested from the raw tree (includes are not expanded). */
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

  def apply(xml: Xml.Element): Option[StoreIndex] =
    Option.when(isStoreRoot(xml)):
      new StoreIndex(
        selector = xml.gather(el =>
          Option.when(el.localName == "by")(el.get("selector").map(_.trim).filter(_.nonEmpty))
        ).flatten.headOption,
        hrefs = xml.gather(el =>
          Option.when(XmlUtil.isInclude(el))(el.get("href").map(_.trim).filter(_.nonEmpty))
        ).flatten,
        names = storeNames(xml)
      )

  private def isStoreRoot(element: Xml.Element): Boolean =
    val name: String = element.localName
    name == "store" || name == "collection"

  private def storeNames(root: Xml.Element): Seq[StoreIndex.Name] =
    val fromChildren: Seq[StoreIndex.Name] =
      root.getChildren.flatMap(_.asElement)
        .filter(_.localName == "name")
        .flatMap(storeName)
    val fromN: Option[StoreIndex.Name] =
      root.get("n").map(_.trim).filter(_.nonEmpty).map(n => StoreIndex.Name(n, None))
    if fromChildren.nonEmpty then fromChildren else fromN.toSeq

  private def storeName(element: Xml.Element): Option[StoreIndex.Name] =
    val n: String = element.get("n").map(_.trim).filter(_.nonEmpty).getOrElse(element.getText.trim)
    Option.when(n.nonEmpty)(
      StoreIndex.Name(n, element.get("lang").map(_.trim).filter(_.nonEmpty))
    )
