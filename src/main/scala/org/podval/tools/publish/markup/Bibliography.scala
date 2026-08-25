package org.podval.tools.publish.markup

import de.undercouch.citeproc.CSL
import de.undercouch.citeproc.bibtex.{BibTeXConverter, BibTeXItemDataProvider}
import de.undercouch.citeproc.csl.{CSLCitation, CSLCitationItem, CSLCitationItemBuilder}
import org.podval.xml.{HtmlElement, Xml, XmlParser}
import zio.blocks.chunk.Chunk
import scala.jdk.CollectionConverters.CollectionHasAsScala
import java.io.{File, FileInputStream}

final class Bibliography(
  private val provider: Option[BibTeXItemDataProvider],
  val style: String,
  val lang: String
):
  def contains(key: String): Boolean =
    provider.exists(_.getIds.contains(key))

  def format(
    stubs: Seq[Xml.Element]
  ): (Map[Xml.Element, Xml.Element], Option[Xml.Element]) =
    provider match
      case None =>
        (stubs.map(stub => stub -> Bibliography.unresolved(stub)).toMap, None)
      case Some(provider) =>
        val csl: CSL = CSL(provider, style, lang)
        csl.setOutputFormat("html")
        val replacements: Map[Xml.Element, Xml.Element] = stubs.map: stub =>
          val items: Seq[Citation.Item] = Citation.itemsOf(stub)
          val mode: Citation.Mode = Citation.modeOf(stub)
          val missing: Seq[String] = items.map(_.key).filterNot(contains)
          stub -> (
            if missing.nonEmpty || items.isEmpty then Bibliography.unresolved(stub)
            else
              val cslItems: Array[CSLCitationItem] = items.map(item =>
                var builder: CSLCitationItemBuilder = CSLCitationItemBuilder(item.key)
                item.locator.foreach(loc => builder = builder.locator(loc))
                mode match
                  case Citation.Mode.Narrative => builder = builder.authorOnly(true)
                  case Citation.Mode.SuppressAuthor => builder = builder.suppressAuthor(true)
                  case Citation.Mode.Parenthetical => ()
                builder.build()
              ).toArray
              val html: String = csl.makeCitation(CSLCitation(cslItems*)).get(0).getText
              Bibliography.wrapCite(html, items.map(_.key))
          )
        .toMap
        val formattedList: Option[Xml.Element] =
          Option.when(stubs.exists(stub => Citation.itemsOf(stub).exists(item => contains(item.key)))):
            val ids: Seq[String] = csl.getRegisteredItems.asScala.toSeq.map(_.getId).filter(_ != null)
            Bibliography.wrapList(csl.makeBibliography(), ids)
        (replacements, formattedList)

  private def unknownLabels(stubs: Seq[Xml.Element]): Seq[String] =
    stubs.flatMap: stub =>
      val items: Seq[Citation.Item] = Citation.itemsOf(stub)
      val missing: Seq[String] = items.map(_.key).filterNot(contains)
      Option.when(missing.nonEmpty || items.isEmpty)(
        if missing.nonEmpty then missing.mkString(", ") else ""
      )

  def resolve(
    xml: Xml.Element
  ): (Xml.Element, Seq[String]) =
    val stubs: Seq[Xml.Element] = Citation.gather(xml)
    if stubs.isEmpty then (xml, Seq.empty)
    else
      val (replacements: Map[Xml.Element, Xml.Element], list: Option[Xml.Element]) = format(stubs)
      var replacedList: Boolean = false
      var result: Xml.Element = xml.transform(element =>
        if Citation.isCite(element) then replacements.getOrElse(element, element)
        else if Citation.isList(element) then
          replacedList = true
          list.getOrElse(element)
        else element
      )
      list.filter(_ => !replacedList).foreach: bibliography =>
        result = result.setChildren(result.getChildren :+ bibliography)
      (result, unknownLabels(stubs))

object Bibliography:
  def load(
    documentDirectory: File,
    bibliography: Option[String],
    csl: Option[String],
    lang: Option[String]
  ): Bibliography =
    val locale: String = lang.getOrElse("en-US")
    (bibliography, csl) match
      case (Some(name), Some(style)) =>
        val file = File(documentDirectory, name)
        if !file.isFile then Bibliography(None, style, locale)
        else
          val provider: BibTeXItemDataProvider = BibTeXItemDataProvider()
          val in: FileInputStream = FileInputStream(file)
          try provider.addDatabase(BibTeXConverter().loadDatabase(in))
          finally in.close()
          Bibliography(Some(provider), style, locale)
      case _ => Bibliography(None, csl.getOrElse(""), locale)

  private def unresolved(stub: Xml.Element): Xml.Element =
    val keys: String = Citation.itemsOf(stub).map(_.key).mkString("; ")
    stub.add(Citation.UnresolvedClass).setText(if keys.nonEmpty then keys else "?")

  private def wrapCite(html: String, keys: Seq[String]): Xml.Element =
    val cite: Xml.Element = Xml
      .element(HtmlElement.A)
      .add(Citation.CiteClass)
      .setChildren(parseFragment(html))
    keys.headOption.fold(cite)(key => cite.setHref(Citation.entryHref(key)))

  private def wrapList(
    bibl: de.undercouch.citeproc.output.Bibliography,
    ids: Seq[String]
  ): Xml.Element =
    val entries: String = Option(bibl.getEntries).toSeq.flatten.mkString
    val html: String =
      Option(bibl.getBibStart).getOrElse("""<div class="csl-bib-body">""") +
        entries +
        Option(bibl.getBibEnd).getOrElse("</div>")
    Citation.listPlaceholder.setChildren(assignEntryIds(parseFragment(html), ids))

  private def assignEntryIds(nodes: Xml.Nodes, ids: Seq[String]): Xml.Nodes =
    var i: Int = 0
    def walk(nodes: Xml.Nodes): Xml.Nodes = nodes.map: node =>
      node.asElement match
        case Some(element) if element.hasClass("csl-entry") && i < ids.length =>
          val keyed: Xml.Element = element.setId(Citation.entryId(ids(i)))
          i += 1
          keyed
        case Some(element) => element.setChildren(walk(element.getChildren))
        case None => node
    walk(nodes)

  private def parseFragment(html: String): Chunk[Xml.Node] =
    XmlParser.parse(s"<div>$html</div>", isXml = false) match
      case Right(wrapper) => wrapper.getChildren
      case Left(_) => Chunk(Xml.text(html))
