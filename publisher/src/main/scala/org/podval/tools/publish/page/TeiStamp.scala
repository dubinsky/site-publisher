package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{DocumentHeader, TeiXmlWriterConfig}
import org.podval.tools.publish.site.Config
import org.podval.xml.{Xml, XmlAttribute, XmlDocument, XmlName}
import java.time.Instant

/** Fills `publicationStmt`, `sourceDesc`, `langUsage`, `calendarDesc`, and `revisionDesc`
  * on a re-read of the source TEI. Does not rewrite the git file.
  */
object TeiStamp:
  def render(page: FullMarkupPage): String =
    TeiXmlWriterConfig.render(XmlDocument.xml(stamp(page)))

  private def stamp(page: FullMarkupPage): Xml.Element =
    val raw: Xml.Element = page.source.get.readRaw()
    val (pre, rest) = raw.getChildren.span(node => !node.asElement.exists(_.isNamed("teiHeader")))
    rest match
      case head +: tail =>
        val header: Xml.Element = head.asElement.getOrElse(elem(raw, "teiHeader"))
        raw.setChildren(pre ++ Seq(stampedHeader(header, raw, page)) ++ tail)
      case Seq() =>
        raw.setChildren(pre ++ Seq(stampedHeader(elem(raw, "teiHeader"), raw, page)))

  private def stampedHeader(header: Xml.Element, root: Xml.Element, page: FullMarkupPage): Xml.Element =
    val lang: Option[String] = DocumentHeader.harvest(root).flatMap(_.lang)
    val profile: Option[Xml.Element] = stampedProfile(header, root, page, lang)
    val rest: Seq[Xml.Node] = without(header, Set("fileDesc", "profileDesc", "revisionDesc"))
    header.setChildren(
      Seq(stampedFileDesc(header, root, page)) ++
        rest ++
        profile.toSeq ++
        Seq(revisionDesc(root, page))
    )

  private def stampedFileDesc(header: Xml.Element, root: Xml.Element, page: FullMarkupPage): Xml.Element =
    val existing: Xml.Element = child(header, "fileDesc").getOrElse(elem(root, "fileDesc"))
    val kept: Seq[Xml.Node] = without(existing, Set("publicationStmt", "sourceDesc"))
    val stamped: Seq[Xml.Node] =
      Seq(publicationStmt(root, page)) ++ sourceDesc(root, page).toSeq
    existing.setChildren(insertAfter(kept, "titleStmt", stamped))

  private def stampedProfile(
    header: Xml.Element,
    root: Xml.Element,
    page: FullMarkupPage,
    lang: Option[String]
  ): Option[Xml.Element] =
    val additions: Seq[Xml.Element] =
      langUsage(root, lang).toSeq ++ calendarDesc(root, page.site.config.teiDefaultCalendar).toSeq
    child(header, "profileDesc") match
      case Some(existing) =>
        Some(existing.setChildren(without(existing, Set("langUsage", "calendarDesc")) ++ additions))
      case None if additions.nonEmpty =>
        Some(elem(root, "profileDesc", additions))
      case None =>
        None

  private def publicationStmt(root: Xml.Element, page: FullMarkupPage): Xml.Element =
    val publisher: Xml.Element = elem(root, "publisher", Seq(
      elem(root, "ptr").set("target", page.site.config.url.trim)
    ))
    val availability: Option[Xml.Element] = page.site.config.license.map(availabilityOf(root, _))
    val date: Option[Xml.Element] = page.site.git.head.map: head =>
      elem(root, "date").set("when", publishedWhen(page, head.time))
    elem(root, "publicationStmt", Seq(publisher) ++ availability.toSeq ++ date.toSeq)

  private def availabilityOf(root: Xml.Element, license: Config.License): Xml.Element =
    val name: String = license.name.trim
    val base: Xml.Element = elem(root, "licence", Seq(Xml.text(name)))
    val licence: Xml.Element =
      license.link.map(_.trim).filter(_.nonEmpty).fold(base)(link => base.set("target", link))
    val holder: Option[Xml.Element] =
      license.holder.map(_.trim).filter(_.nonEmpty).map(text => para(root, text))
    elem(root, "availability", Seq(licence) ++ holder.toSeq).set("status", "free")

  private def sourceDesc(root: Xml.Element, page: FullMarkupPage): Option[Xml.Element] =
    val path: Option[Xml.Element] =
      Option(PageHeader.addressLabel(page)).filter(_.nonEmpty).map(para(root, _))
    val facsimiles: Option[Xml.Element] =
      page.site.config.facsimilesUrl.map(_.trim).filter(_.nonEmpty).map(para(root, _))
    val paras: Seq[Xml.Element] = path.toSeq ++ facsimiles.toSeq
    Option.when(paras.nonEmpty)(elem(root, "sourceDesc", paras))

  private def langUsage(root: Xml.Element, lang: Option[String]): Option[Xml.Element] =
    lang.map(_.trim).filter(_.nonEmpty).map: code =>
      elem(root, "langUsage", Seq(
        elem(root, "language", Seq(Xml.text(code))).set("ident", code)
      ))

  private def calendarDesc(root: Xml.Element, calendar: Option[String]): Option[Xml.Element] =
    calendar.map(_.trim).filter(_.nonEmpty).map: id =>
      val calendarEl: Xml.Element =
        elem(root, "calendar", Seq(para(root, calendarProse(id)))).set(XmlAttribute.XmlId, id)
      elem(root, "calendarDesc", Seq(calendarEl))

  private def calendarProse(id: String): String =
    val word: String = id.trim
    val titled: String = word.take(1).toUpperCase + word.drop(1)
    s"$titled calendar"

  private def revisionDesc(root: Xml.Element, page: FullMarkupPage): Xml.Element =
    val sha: Option[String] = page.site.git.head.map(_.sha)
    val when: Option[String] = page.site.git.head.map(head => publishedWhen(page, head.time))
    val base: Xml.Element = elem(root, "change", Seq(para(root, revisionSentence(sha))))
    val change: Xml.Element = when.fold(base)(value => base.set("when", value))
    elem(root, "revisionDesc", Seq(change))

  private def revisionSentence(sha: Option[String]): String =
    val sentence: String = "This published file is generated. The file in git is the source."
    sha.fold(sentence)(id => s"This published file is generated. The file in git is the source ($id).")

  private def publishedWhen(page: Page, time: Instant): String =
    page.site.toOffsetDateTime(time).toLocalDate.toString

  /** New elements carry the root namespace so the writer does not clear a default `xmlns`. */
  private def elem(root: Xml.Element, local: String, children: Seq[Xml.Node] = Seq.empty): Xml.Element =
    Xml.element(XmlName(local, root.getName.namespace), Seq.empty, children)

  private def para(root: Xml.Element, text: String): Xml.Element =
    elem(root, "p", Seq(Xml.text(text)))

  private def child(element: Xml.Element, name: String): Option[Xml.Element] =
    element.childElements.find(_.isNamed(name))

  private def without(element: Xml.Element, names: Set[String]): Seq[Xml.Node] =
    element.getChildren.filterNot(node => node.asElement.exists(el => names.contains(el.getName.localName)))

  private def insertAfter(children: Seq[Xml.Node], afterLocal: String, insert: Seq[Xml.Node]): Seq[Xml.Node] =
    val (pre, rest) = children.span(node => !node.asElement.exists(_.isNamed(afterLocal)))
    rest match
      case head +: tail => pre ++ Seq(head) ++ insert ++ tail
      case Seq() => pre ++ insert
