package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{CollectionIndex, FacsimilePage, FullMarkupPage, NamedWindows, Page}
import org.podval.tools.publish.site.Path
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}
import Xml.given

/** Per-document facsimile viewer: JPEG URLs and scroller XML. `pb` hrefs are filled at render. */
object Facsimile:
  object ScrollerClass extends CssClass("facsimile-scroller")

  val fileName: String = "facsimile"
  val textTarget: String = "text"
  val facsimileTarget: String = "facsimile"

  def needed(page: FullMarkupPage): Boolean =
    page.site.config.facsimilesUrl.map(_.trim).exists(_.nonEmpty) &&
    !CollectionIndex.isTranslation(page) &&
    page.doc.flatMap(_.documentHeader).toSeq.flatMap(_.pbs).exists(pb => !pb.isMissing)

  def path(document: Page): Path = document.path.add(fileName).html

  /** Collector inbound remainder `facsimile, P` → `P, facsimile` (`/rgada/facsimile/029`). */
  def inboundRemainder(remainder: Seq[String]): Option[Seq[String]] =
    remainder match
      case Seq(`fileName`, document) if document.nonEmpty => Some(Seq(document, fileName))
      case _ => None

  def imageUrl(
    base: String,
    sourceDirectory: Seq[String],
    n: String,
    facs: Option[String]
  ): String =
    facs.map(_.trim).filter(_.nonEmpty).getOrElse:
      val slash: String = if base.endsWith("/") then base else s"$base/"
      val dir: String = sourceDirectory.mkString("/")
      val mid: String = if dir.isEmpty then "" else s"$dir/"
      s"$slash$mid$n.jpg"

  def original(page: Page): Page =
    if !CollectionIndex.isTranslation(page) then page
    else
      val base: String = CollectionIndex.baseName(page)
      page.parent.toSeq.flatMap: directory =>
        directory.site.pages.pages.filter: sibling =>
          sibling.isInstanceOf[FullMarkupPage] &&
          !sibling.isDirectory &&
          sibling.parent.contains(directory) &&
          !CollectionIndex.isTranslation(sibling) &&
          CollectionIndex.baseName(sibling) == base
      .headOption.getOrElse(page)

  def resolveLink(element: Xml.Element, page: Page): Xml.Element =
    if !Pb.is(element) then element
    else page.site.pages.facsimilePage(page) match
      case None => element
      case Some(viewer) =>
        element.getId.filter(_.nonEmpty).fold(element): id =>
          val linked: Xml.Element =
            element.setHref(s"${viewer.publishedPath}#$id")
          NamedWindows.targetAttr(viewer).fold(
            linked.set(XmlAttribute.Target, facsimileTarget)
          )(name => linked.set(XmlAttribute.Target, name))

  def scroller(viewer: FacsimilePage): Xml.Element =
    val document: FullMarkupPage = viewer.document
    val pageType: PageType =
      document.parent.flatMap(_.store).map(_.pageType).getOrElse(PageType.Manuscript)
    val base: String = document.site.config.facsimilesUrl.map(_.trim).filter(_.nonEmpty).get
    val sourceDir: Seq[String] = document.sourcePath.map(_.path.init).getOrElse(Seq.empty)
    val pbs: Seq[Pb] =
      document.doc.flatMap(_.documentHeader).toSeq.flatMap(_.pbs).filterNot(_.isMissing)
    val figures: Xml.Nodes =
      pbs.map(pb => pageFigure(document, pageType, base, sourceDir, pb))

    Xml.element(XmlElement.Div).add(ScrollerClass).setChildren(figures)

  private def pageFigure(
    document: FullMarkupPage,
    pageType: PageType,
    base: String,
    sourceDir: Seq[String],
    pb: Pb
  ): Xml.Element =
    val id: String = Pb.pageId(pb.n)
    val display: String = pageType.displayName(pb.n)
    val img: Xml.Element = Xml.element(XmlElement.Img)
      .setId(id)
      .set(XmlAttribute.Alt, s"facsimile for page $display")
      .set(XmlAttribute.Src, imageUrl(base, sourceDir, pb.n, pb.facs))
    val link: Xml.Element =
      val a: Xml.Element = Xml.element(XmlElement.A)
        .setHref(s"${document.publishedPath}#$id")
        .setChildren(Seq(img: Xml.Node))
      NamedWindows.targetAttr(document).fold(a.set(XmlAttribute.Target, textTarget))(name =>
        a.set(XmlAttribute.Target, name)
      )
    Figure.make(Seq(Xml.text(display)), Seq(link: Xml.Node))
