package org.podval.tools.publish

import org.podval.xml.{Html, HtmlAttribute, HtmlElement, Xml, XmlAttribute}
import zio.blocks.html.*
import BackLinks.BackLink
import org.podval.tools.publish.features.{InternalLinksFeature, WikiLinksFeature}

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
    val backLinks: Seq[(Page, List[BackLinks.BackLink])] = this.backLinks(page)
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

// Note: Obsidian expands the context to the source level, which is good for searching - but doesn't look great
// when there are non-wiki links in there;
// I am going with just text, so the non-wiki links are not going to be visible...
// Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
object BackLinks:
  final class Context(
    val url: String,
    val before: String,
    val element: String,
    val after: String
  )

  final class BackLink(
    val to: Link,
    val from: Page,
    val transclude: Boolean,
    val kind: Option[Link.Kind],
    val context: Context
  )

  def backLink(
    element: Xml.Element,
    parents: Seq[Xml.Element],
    from: Page,
    toc: Toc
  ): Option[BackLink] =
    if !element.isElement(HtmlElement.A) || !element.has(InternalLinksFeature.InternalLinkClass) then None else
      for
        ref <- element.get(HtmlAttribute.Href)
        to <- Link.resolve(ref, kind = None, from)
        id <- element.get(XmlAttribute.Id)
      yield
        val toId: Option[Link.ToId] = toc.resolveId(id)
        val toFrom: Link = Link(from, fragment = toId, intrapage = false)
        val parent: Xml.Element = parents.head
        // TODO go back to `ne`
        val (before: Xml.Nodes, tail) = parent.getChildren.span(
          _.asElement.fold(true)(element => !element.get(HtmlAttribute.Href).contains(ref))
        )
        val it: Xml.Element = tail.head.asElement.get
        val after: Xml.Nodes = tail.tail

        BackLink(
          to = to,
          from = from,
          transclude = element.has(WikiLinksFeature.TranscludeClass),
          kind = Link.Kind.of(element),
          context = Context(
            url = toFrom.url,
            before = shortenContext(isBefore = true, Xml.toString(before)),
            element = it.getText,
            after = shortenContext(isBefore = false, Xml.toString(after))
          )
        )


  private val contextLengthHalf: Int = 60

  private def shortenContext(isBefore: Boolean, string: String): String =
    if string.length <= contextLengthHalf then string else if isBefore then
      val result = string.substring(string.length - contextLengthHalf)
      val prefix = /*if result.startsWith(" ") then "" else*/ "..."
      prefix + result.trim
    else
      val result = string.substring(0, contextLengthHalf)
      val suffix = /*if result.endsWith(" ") then "" else*/ "..."
      result.trim + suffix
