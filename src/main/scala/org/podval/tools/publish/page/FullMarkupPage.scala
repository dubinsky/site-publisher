package org.podval.tools.publish.page

import org.podval.tools.publish.markup.PageHeader
import org.podval.tools.publish.site.{PageError, Path, Posts, Site}
import org.podval.xml.Html

abstract class FullMarkupPage(site: Site, path: Path) extends MarkupPage(site, path):
  final override def prev: Option[Page] = parent.flatMap(_.prev(this))
  override def next: Option[Page] = parent.flatMap(_.next(this))

  final override def markupContent: Option[Html.Element] =
    markupContent(sectionId = None, isTerminal = true)
  final override def pageHeader: Option[Html.Element] = Option.when(source.isDefined)(PageHeader.of(this))
  final def chunks: Seq[ChunkedMarkupPage] = content.map(_.toc.chunks(this)).getOrElse(Seq.empty)
  override protected def formatSourcePage: Option[FullMarkupPage] = Some(this)

  // TODO permalink must be absolute
  final def aliases: Seq[Alias] = (postPath.toSeq ++ frontMatter.permalink.toSeq ++ frontMatter.aliases)
    .map(Alias(site, this, _))

  private def postPath: Option[String] = if !frontMatter.post then None else date match
    case None =>
      site.error(path, PageError.NoDate, s"No date for an automatic blog post")
      None
    case Some(date) =>
      val title: String = frontMatter.postTitle.getOrElse(path.fileName) // TODO titleFromPath?
      Some(Posts.path(date.localDate, title).html.withoutHtml.toString)

  final def tags: List[String] = frontMatter.tags

  final def author: Option[String] = content(_.frontMatter.author)

  final def tocDepth: Int = frontMatter.tocDepth.getOrElse(2)
  final def hasToc: Boolean = chunk || frontMatter.tocDepth.isDefined
  final def chunk: Boolean = frontMatter.chunk
  final def chunkDepth: Int = frontMatter.chunkDepth.getOrElse(2)
  final def pdf: Boolean = frontMatter.pdf
