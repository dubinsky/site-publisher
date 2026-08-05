package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{MarkupPage, OriginalMarkupPage}
import org.podval.xml.Html
import zio.blocks.html.*

// TODO move to site
final class BackLinks:
  private var backLinks: List[BackLink] = List.empty

  def addBackLinks(backLink: Seq[BackLink]): Unit = backLinks = backLinks.appendedAll(backLink)

  def html(page: MarkupPage): Option[Html.Element] =
    val pageBackLinks: Seq[(OriginalMarkupPage, List[BackLink])] = backLinks
      .filter(_.to.page == page)
      .filterNot(_.from == page)
      .groupBy(_.from)
      .toSeq
      .sortBy(_._1.title)

    if page.hasSyntheticContent && !page.isDirectory || pageBackLinks.isEmpty then None else Some:
      div(className := "backlinks",
        h3("Backlinks"),
        ul(pageBackLinks.map((from, links) =>
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
