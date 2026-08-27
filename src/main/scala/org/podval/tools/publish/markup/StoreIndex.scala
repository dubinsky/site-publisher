package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlUtil}

/** Ordered children of a TEI `store` / `collection`. `hrefs` are page references, not XInclude.
  * Harvested from the raw tree (includes are not expanded). Names, title, and abstract are
  * header chrome (`PageHeader.collectorPageHeader`), not body content. */
final class StoreIndex(
  val selector: Option[String],
  val hrefs: Seq[String],
  val names: Seq[StoreIndex.Name],
  val title: Option[Xml.Element],
  val description: Option[Xml.Element]
):
  def displayName: Option[String] =
    names.find(_.lang.contains("ru")).orElse(names.headOption).map(_.n)

object StoreIndex:
  final class Name(
    val n: String,
    val lang: Option[String]
  )

  def apply(xml: Xml.Element): Option[StoreIndex] =
    Option.when(TeiMarkup.isStoreRoot(xml)):
      new StoreIndex(
        selector = xml.gather(el =>
          Option.when(el.localName == "by")(el.get("selector").map(_.trim).filter(_.nonEmpty))
        ).flatten.headOption,
        hrefs = xml.gather(el =>
          Option.when(XmlUtil.isInclude(el))(el.get("href").map(_.trim).filter(_.nonEmpty))
        ).flatten,
        names = storeNames(xml),
        title = storeTitle(xml),
        description = storeDescription(xml)
      )

  private def storeTitle(root: Xml.Element): Option[Xml.Element] =
    val candidates: Seq[Xml.Element] =
      root.getChildren.flatMap(_.asElement).filter(_.localName == "title")
    val nonempty: Seq[Xml.Element] = candidates.filter(_.getText.trim.nonEmpty)
    nonempty.find(_.get("type").contains("main")).orElse(nonempty.headOption)

  private def storeDescription(root: Xml.Element): Option[Xml.Element] =
    root.getChildren.flatMap(_.asElement).find: el =>
      el.localName == "abstract" && el.getChildren.nonEmpty

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
