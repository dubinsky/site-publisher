package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

/** Collector `/report` apparatus. Bodies other than the index are filled when harvest exists. */
final class ReportPage(
  site: Site,
  val kind: ReportPage.Kind
) extends SyntheticMarkupPage(site, kind.path):
  override def titleDefault: String = kind.title

  override protected def iconDefault: Icon = Icon.list

  override def parent: Option[DirectoryPage] = None

  override def pageHeader: Option[Html.Element] = Some(
    header(className := "post-header",
      h1(className := "post-title p-name", itemProp := "name headline", title)
    )
  )

  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override protected def syntheticContent: Html.Element =
    kind match
      case ReportPage.Kind.Index =>
        Page.pageList(
          site.pages.pages.collect:
            case page: ReportPage if page.kind != ReportPage.Kind.Index => page
        )
      case _ =>
        div(className := "report")

object ReportPage:
  val segment: String = "report"

  enum Kind derives CanEqual:
    case Index, NoRefs, Unclears, Misnamed

    def id: Option[String] = this match
      case Index => None
      case NoRefs => Some("no-refs")
      case Unclears => Some("unclears")
      case Misnamed => Some("misnamed-entities")

    def title: String = this match
      case Index => "Reports"
      case NoRefs => "Names without @ref"
      case Unclears => "Unclear"
      case Misnamed => "Misnamed entities"

    def path: Path = id.fold(Path(segment).html)(name => Path(segment, name).html)

  def pages(site: Site): Seq[ReportPage] = Kind.values.toSeq.map(ReportPage(site, _))
