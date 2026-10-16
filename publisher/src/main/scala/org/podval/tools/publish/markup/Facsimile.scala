package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{CollectionIndex, FacsimilePage, FullMarkupPage, NamedWindows, Page}
import org.podval.tools.publish.site.Path
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

/** Per-document facsimile viewer: JPEG URLs and scroller XML. `pb` hrefs are filled at render. */
object Facsimile:
  object ScrollerClass extends CssClass("facsimile-scroller")

  val fileName: String = "facsimile"
  val textTarget: String = "text"
  val facsimileTarget: String = "facsimile"

  def needed(page: FullMarkupPage): Boolean =
    page.site.config.facsimilesUrl.map(_.trim).exists(_.nonEmpty) &&
    !CollectionIndex.isTranslation(page) &&
    present(page).nonEmpty

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

  def imageFor(document: FullMarkupPage, pb: Pb): (String, String) =
    val base: String = document.site.config.facsimilesUrl.map(_.trim).filter(_.nonEmpty).get
    val sourceDir: Seq[String] = document.sourcePath.map(_.path.init).getOrElse(Seq.empty)
    val url: String = imageUrl(base, sourceDir, pb.n, pb.facs)
    val alt: String = s"facsimile for page ${displayName(document, pb.n)}"
    (url, alt)

  def resolveLink(element: Xml.Element, page: Page, isChunked: Boolean): Xml.Element =
    if !Pb.is(element) then element
    else page.site.pages.facsimilePage(page) match
      case None => element
      case Some(viewer) =>
        element.getId.filter(_.nonEmpty).fold(element): id =>
          val linked: Xml.Element = element.setHref(s"${viewer.publishedPath}#$id")
          val targeted: Xml.Element = NamedWindows.targetAttr(viewer).fold(
            linked.set(XmlAttribute.Target, facsimileTarget)
          )(name => linked.set(XmlAttribute.Target, name))
          // Chunk anchors keep today's href. The companion is the unchunked page only.
          if isChunked then targeted
          else page match
            case full: FullMarkupPage if needed(full) =>
              val pb: Option[Pb] = present(full).find(candidate => Pb.pageId(candidate.n) == id)
              pb.fold(targeted): found =>
                val (url, alt) = imageFor(full, found)
                targeted.set("data-facsimile-src", url).set("data-facsimile-alt", alt)
            case _ => targeted

  /** Primary `img` has no `src`: `display: none` would not cancel that fetch. */
  def aside(document: FullMarkupPage): Xml.Element =
    val pb: Pb = present(document).head
    val (url, alt) = imageFor(document, pb)
    val img: Xml.Element = Xml.element(XmlElement.Img)
      .addClass("facsimile-aside-img")
      .set(XmlAttribute.Alt, alt)
      .set("decoding", "async")
      .set("data-facsimile-src", url)
      .set("data-facsimile-current", Pb.pageId(pb.n))
    val status: Xml.Element = Xml.element(XmlElement.P)
      .addClass("facsimile-aside-status")
      .set(XmlAttribute.Hidden, "hidden")
    val fallback: Xml.Element = Xml.element(XmlElement.Img)
      .set(XmlAttribute.Alt, alt)
      .set("decoding", "async")
    val source: Xml.Element = Xml.element(XmlElement.Source)
      .set("media", "screen and (min-width: 1312px)")
      .set("srcset", url)
    val picture: Xml.Element = Xml.element("picture").setChildren(Seq(source, fallback: Xml.Node))
    val noscript: Xml.Element = Xml.element("noscript").setChildren(Seq(picture: Xml.Node))
    val frame: Xml.Element = Xml.element(XmlElement.Div)
      .addClass("facsimile-aside-frame")
      .setChildren(Seq(img, status, noscript: Xml.Node))
    document.withInterfaceLang(
      Xml.element(XmlElement.Aside)
        .addClass("facsimile-aside")
        .set("aria-label", "Facsimile")
        .setChildren(Seq(frame))
    )

  def scroller(viewer: FacsimilePage): Xml.Element =
    val document: FullMarkupPage = viewer.document
    val figures: Xml.Nodes = present(document).map(pb => pageFigure(document, pb))
    Xml.element(XmlElement.Div).add(ScrollerClass).setChildren(figures)

  private def present(document: FullMarkupPage): Seq[Pb] =
    document.doc.flatMap(_.documentHeader).toSeq.flatMap(_.pbs).filterNot(_.isMissing)

  private def displayName(document: FullMarkupPage, n: String): String =
    document.parent.flatMap(_.store).map(_.pageType).getOrElse(PageType.Manuscript).displayName(n)

  private def pageFigure(document: FullMarkupPage, pb: Pb): Xml.Element =
    val id: String = Pb.pageId(pb.n)
    val (url, alt) = imageFor(document, pb)
    val display: String = displayName(document, pb.n)
    val img: Xml.Element = Xml.element(XmlElement.Img)
      .setId(id)
      .set(XmlAttribute.Alt, alt)
      .set(XmlAttribute.Src, url)
    val link: Xml.Element =
      val a: Xml.Element = Xml.element(XmlElement.A)
        .setHref(s"${document.publishedPath}#$id")
        .setChildren(Seq(img: Xml.Node))
      NamedWindows.targetAttr(document).fold(a.set(XmlAttribute.Target, textTarget))(name =>
        a.set(XmlAttribute.Target, name)
      )
    Figure.make(Seq(Xml.text(display)), Seq(link: Xml.Node))
