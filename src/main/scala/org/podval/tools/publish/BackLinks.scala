package org.podval.tools.publish

import org.podval.xml.Html
import zio.blocks.html.*
import BackLinks.BackLink

final class BackLinks:
  private var backLinks: List[BackLink] = List.empty

  def addBackLinks(backLink: Seq[BackLink]): Unit = backLinks = backLinks.appendedAll(backLink)

  def backLinks(page: Page): Seq[(MarkupPage, List[BackLink])] = backLinks
    .filter(_.to.page == page)
    .filterNot(_.from == page)
    .groupBy(_.from)
    .toSeq
    .sortBy(_._1.title)

  def html(page: MarkupPage): Option[Html.Element] =
    val backLinks: Seq[(MarkupPage, List[BackLinks.BackLink])] = this.backLinks(page)
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

object BackLinks:
  final class Context(
    val url: String,
    val before: String,
    val element: String,
    val after: String
  )

  final class BackLink(
    val to: Link,
    val from: MarkupPage,
    val transclude: Boolean,
    val kind: Option[Link.Kind],
    val context: Context
  )
