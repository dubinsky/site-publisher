package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{MarkupPage, Page}
import org.podval.xml.Html
import zio.blocks.html.*

final class BackLinks:
  private var backLinks: List[BackLink] = List.empty

  def addBackLinks(backLink: Seq[BackLink]): Unit = backLinks = backLinks.appendedAll(backLink)

  def backLinks(page: Page): Seq[(Page, List[BackLink])] = backLinks
    .filter(_.to.page == page)
    .filterNot(_.from == page)
    .groupBy(_.from)
    .toSeq
    .sortBy(_._1.title)

  def html(page: MarkupPage): Option[Html.Element] =
    val backLinks: Seq[(Page, List[BackLink])] = this.backLinks(page)
    if page.hasSyntheticContent && !page.isDirectory || backLinks.isEmpty then None else Some:
      div(className := "backlinks",
        h3("Backlinks"),
        ul(backLinks.map((from, links) =>
          li(
            details(
              summary(
                from.ref(),
                span(className := "backlinks-count", links.length)
              ),
              ul(className := "backlinks-list", links.map(link =>
                val context = link.context
                li(
                  a(
                    href := context.url,
                    context.before,
                    span(className := "backlink", context.element),
                    context.after
                  )
                )
              ))
            )
          )
        ))
      )
