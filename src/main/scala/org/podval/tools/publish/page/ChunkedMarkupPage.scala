package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Section
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html

// Note: TOC - no sectionId, isTerminal = false
final class ChunkedMarkupPage(
  markupPage: FullMarkupPage,
  val sectionId: Option[String],
  isTerminal: Boolean,
  name: String
) extends MarkupPage(
  markupPage.site,
  markupPage.path.add(name).html
):
  override def markupContent: Option[Html.Element] = markupPage.markupContent(
    sectionId = sectionId,
    isTerminal = isTerminal
  )
  
  override def isDirectory: Boolean = false

  override def source: Option[PageSource] = None

  // Chunks live under /P/; do not synthesize a DirectoryPage at /P/index.html
  // (that path is the TOC chunk). Same parent as the unchunked document.
  override lazy val parent: Option[DirectoryPage] = markupPage.parent

  override def titleFromPath: String =
    if sectionId.isEmpty then markupPage.titleFromPath else path.fileName

  override def titleDefault: String =
    if sectionId.isEmpty then markupPage.title else path.fileName

  override def hasSyntheticContent: Boolean = false

  override protected def syntheticContentOpt: Option[Html.Element] = None

  override protected def iconDefault: Icon = Icon.note // TODO page/document...

  override def pageHeader: Option[Html.Element] = markupPage.pageHeader

  override protected def formatSourcePage: Option[FullMarkupPage] = Some(markupPage)
  override protected def formatIsChunked: Boolean = true

  override def up: Option[Page] = sectionId match
    case None => super.up
    case Some(id) =>
      val parentSection: Option[Section] =
        markupPage.content.map(_.toc.getSection(id)).flatMap(_.parent)
      parentSection.flatMap(section => markupPage.chunks.find(_.sectionId.contains(section.id)))
        .orElse(markupPage.chunks.find(_.sectionId.isEmpty))

  override def prev: Option[Page] =
    val all: Seq[ChunkedMarkupPage] = markupPage.chunks
    val i: Int = all.indexWhere(_.path == path)
    Option.when(i > 0)(all(i - 1))

  override def next: Option[Page] =
    val all: Seq[ChunkedMarkupPage] = markupPage.chunks
    val i: Int = all.indexWhere(_.path == path)
    Option.when(i >= 0 && i < all.length - 1)(all(i + 1))

  
