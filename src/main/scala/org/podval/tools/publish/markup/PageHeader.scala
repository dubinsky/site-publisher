package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{FullMarkupPage, Page}
import org.podval.tools.publish.util.Date
import org.podval.xml.{Html, Xml, Xml2Html}
import zio.blocks.chunk.Chunk
import zio.blocks.html.*

// TODO move this to `page`.
object PageHeader:
  def of(page: FullMarkupPage): Html.Element =
    // TODO also documents under a collection (ancestor store path), not only the store root.
    if page.content.flatMap(_.storeIndex).isDefined
    then collectorPageHeader(page)
    else pageHeader(page)

  def pageHeader(page: FullMarkupPage): Html.Element =
    header(className := "post-header",
      postPath(page),
      h1(className := "post-title p-name", itemProp := "name headline", page.title),
      Option.when(!page.hasSyntheticContent)(articleMeta(page))
    )

  private def postPath(page: Page): Html.Element =
    def parents(page: Page): Seq[Page] = page.parent match
      case None => Seq.empty
      case Some(parent) => parents(parent) :+ parent

    val pathFull: Seq[Page] = parents(page)
    val path: Seq[Page] = if pathFull.isEmpty then pathFull else pathFull.tail
    span(className := "post-path", path.map(page => span("/", page.ref(withIcon = false))))

  private def articleMeta(page: FullMarkupPage): Html.Element =
    div(className := "post-meta",
      join(
        join(
          join(
            timeHtml(Option.when(page.dateModified.nonEmpty)("Published:"), page.date, "dt-published", "datePublished"),
            "•",
            timeHtml(Some("Updated:"), page.dateModified, "dt-modified", "dateModified")
          ),
          "•",
          page.author.fold(Seq.empty): author =>
            Seq(
              span(className := "post-authors",
                span(className := "post-author", itemProp := "author", itemScope := true, itemType := "http://schema.org/Person",
                  span(className := "p-author h-card", itemProp := "name", author)
                )
              )
            )
        ),
        "|",
        page.tags.map(page.site.tags.tagRef)
      )
    )

  private def join(left: Seq[Html.Element], text: String, right: Seq[Html.Element]): Seq[Html.Element] =
    if left.nonEmpty && right.nonEmpty
    then left ++ Seq(span(className := "bullet-divider", text)) ++ right
    else left ++ right

  private def timeHtml(label: Option[String], date: Option[Date], cls: String, itemprop: String): Seq[Html.Element] =
    date.fold(Seq.empty): date =>
      label.fold(Seq.empty)(label => Seq(span(className := "meta-label", label))) ++
        Seq(time(className := cls, datetime := date.toString, itemProp := itemprop, date.toShortString))

  def collectorPageHeader(page: FullMarkupPage): Html.Element =
    val index: StoreIndex = page.content.flatMap(_.storeIndex).get
    Xml2Html.fromXml(collectorHeaderXml(index))

  // Live collector: `div.store-header` / `tei-head` with name, ": ", title, then abstract.
  // Parent selector labels (`архив`) are not in this store file.
  private def collectorHeaderXml(index: StoreIndex): Xml.Element =
    val names: Xml.Nodes = Chunk.from(
      index.names.find(_.lang.contains("ru")).orElse(index.names.headOption).toSeq.map(storeNameXml)
    )
    val titleInner: Xml.Nodes = index.title.fold(Chunk.empty[Xml.Node]): title =>
      TeiMarkup.convertFragment(title).getChildren
    val colon: Xml.Nodes =
      if names.nonEmpty && titleInner.nonEmpty then Chunk(Xml.text(": ")) else Chunk.empty
    val head: Xml.Element = Xml.element("tei-head").setChildren(names ++ colon ++ titleInner)
    val description: Xml.Nodes = Chunk.from(index.description.toSeq.map(TeiMarkup.convertFragment))
    Xml.element("header").addClass("store-header").setChildren(Chunk(head: Xml.Node) ++ description)

  private def storeNameXml(name: StoreIndex.Name): Xml.Element =
    var result: Xml.Element = Xml.element("span").addClass("store-name").setText(name.n)
    name.lang.foreach(lang => result = result.set("lang", lang))
    result
