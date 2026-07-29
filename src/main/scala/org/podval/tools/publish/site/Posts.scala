package org.podval.tools.publish.site

import org.podval.tools.publish.page.{DirectoryPage, NonDirectoryPage, Page, SyntheticMarkupPage}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*
import java.time.LocalDate
import java.time.format.DateTimeParseException

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

final class Posts(site: Site) extends SyntheticMarkupPage(site, Path("posts").html) with NonDirectoryPage:
  override def titleDefault: String = "Posts"
  override protected def descriptionDefault: Option[String] = Some("All posts")
  override protected def iconDefault: Icon = Icon.envelope
  override protected def headerPagePriorityDefault: Int = 1
  override protected def langDefault: Option[String] = Some("en")

  def posts: List[Page] = site
    .pages
    .pages
    .filter(_.postDate.isDefined)
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

  private def postsDirectoryName: String = site.postsDirectoryName
  private def draftsDirectoryName: Option[String] = site.draftsDirectoryName
  private def dailyNotesDirectoryName: Option[String] = site.dailyNotesDirectoryName
  
  def isDirectoryEmptiedOut(directoryPath: Seq[String]): Boolean = directoryPath.length == 1 && {
    val name: String = directoryPath.head
    postsDirectoryName == name ||
    draftsDirectoryName.contains(name) ||
    dailyNotesDirectoryName.contains(name)
  }

  def path(sourcePath: Path): Option[Path] =
    def inDirectory(name: String): Boolean = sourcePath.path.head == name

    val isPost: Boolean = inDirectory(postsDirectoryName) || draftsDirectoryName.exists(inDirectory)
    val isDaily: Boolean = dailyNotesDirectoryName.exists(inDirectory)

    if !isPost && !isDaily then None else
      val fileName: String = sourcePath.fileName

      for
        date: LocalDate <-
          try
            // TODO record error, do not throw|!
            if fileName.length < 10 then throw DateTimeParseException("Date is too short", fileName, 0)
            Some(LocalDate.parse(fileName.substring(0, 10)))
          catch case e: DateTimeParseException =>
            site.error(
              sourcePath, 
              PageError.FileName, 
              s"Post and daily note names must start with date: $fileName", 
              Some(e)
            )
            None

        title: String <-
          val titleString: String = if fileName.length <= 11 then "" else fileName.substring(11).trim
          val title: String = if titleString.nonEmpty then titleString else DirectoryPage.fileName
          val dailiesMixedWithPosts: Boolean = dailyNotesDirectoryName.contains(postsDirectoryName)
          if dailiesMixedWithPosts
          then Some(title) else
            if isPost && titleString.isEmpty
            then
              site.error(
                sourcePath, 
                PageError.FileName, 
                s"Post must have title: $fileName"
              )
              None
            else
              if isDaily && titleString.nonEmpty
              then
                site.error(
                  sourcePath, 
                  PageError.FileName, 
                  s"Daily note can not have title: $fileName"
                )
                None
              else
                Some(title)
      yield
        Posts.path(date, title)
