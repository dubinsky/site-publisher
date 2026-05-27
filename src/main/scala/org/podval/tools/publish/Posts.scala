package org.podval.tools.publish

import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*
import java.time.LocalDate
import java.time.format.DateTimeParseException

final class Posts(
  site: Site,
  postsDirectoryName: String,
  draftsDirectoryName: Option[String],
  dailyNotesDirectoryName: Option[String]
) extends MarkupPage.WithSyntheticContent(site, Path("posts").html) with Page.NonDirectory:
  override def titleDefault: String = "Posts"
  override protected def descriptionDefault: Option[String] = Some("All posts")
  override protected def iconDefault: Icon = Icon.envelope
  override protected def headerPagePriorityDefault: Int = 1
  override protected def langDefault: Option[String] = Some("en")

  def posts: List[Page] = site
    .pages
    .filter(_.isPost)
    .filterNot(page => page.isDirectory && page.source.isEmpty)
    .sortBy(_.date)
    .reverse

  override protected def syntheticContent: Html.Element =
    div(className := "home",
      //      h1(className := "page-heading", page.title)
      h2(className := "post-list-heading", "Posts"),
      ul(className := "post-list", posts.map(post =>
        li(
          span(className := "post-meta", post.date.map(_.toShortString).getOrElse("")),
          h3(className := "post-link", post.ref())
          // {%- if site.minima.show_excerpts -%} {{ post.excerpt }} {%- endif -%} // TODO unify with feed.xml
        )
      ))
    )

  def path(sourcePath: Path): Option[Path] =
    def inDirectory(name: String): Boolean = sourcePath.path.head == name

    val isPost: Boolean = inDirectory(postsDirectoryName) || draftsDirectoryName.exists(inDirectory)
    val isDaily: Boolean = dailyNotesDirectoryName.exists(inDirectory)

    if !isPost && !isDaily then None else
      val fileName: String = sourcePath.fileName
      val errorReporter: PageError.Reporter = PageError.SiteReporter(sourcePath, site)

      for
        date: LocalDate <-
          try
            if fileName.length < 10 then throw DateTimeParseException("Date is too short", fileName, 0)
            Some(LocalDate.parse(fileName.substring(0, 10)))
          catch case e: DateTimeParseException =>
            errorReporter.error(PageError.FileName, s"Post and daily note names must start with date: $fileName", None, Some(e))

        title: String <-
          val titleString: String = if fileName.length <= 11 then "" else fileName.substring(11).trim
          val title: String = if titleString.nonEmpty then titleString else Directory.fileName
          val dailiesMixedWithPosts: Boolean = dailyNotesDirectoryName.contains(postsDirectoryName)
          if dailiesMixedWithPosts then Some(title) else if isPost && titleString.isEmpty
          then errorReporter.error(PageError.FileName, s"Post must have title: $fileName", None)
          else if isDaily && titleString.nonEmpty
          then errorReporter.error(PageError.FileName, s"Daily note can not have title: $fileName", None)
          else Some(title)
      yield
        Posts.path(date, title)

object Posts:
  def path(date: LocalDate, title: String): Path = Path(
    f"${date.getYear}%04d",
    f"${date.getMonthValue}%02d",
    f"${date.getDayOfMonth}%02d",
    title
  )

  def date(path: Path): Option[LocalDate] = if path.path.length != 4 then None else
    val dateString = s"${path.path(0)}-${path.path(1)}-${path.path(2)}"
    try Some(LocalDate.parse(dateString))
    catch case e: DateTimeParseException => None
