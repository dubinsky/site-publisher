package org.podval.tools.publish.link

import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.page.{Page, PageContent}
import org.podval.xml.Xml

final class BackLink private(
  val to: Link,
  val from: Page,
  val transclude: Boolean,
  val kind: Option[LinkKind],
  val context: LinkContext
)

object BackLink:
  def apply(
    element: Xml.Element,
    parents: Seq[Xml.Element],
    from: Page,
    content: PageContent,
  ): Option[BackLink] =
    if !element.isA || !Links.isInternalLink(element) then None else
      for
        ref <- element.getHref
        to <- Link.resolve(ref, kind = None, from)
        id <- element.getId
      yield
        val toId: Option[Link.ToId] = content.resolveId(id)
        val toFrom: Link = Link(from, fragment = toId, intrapage = false)
        val parent: Xml.Element = parents.head
        // TODO go back to `ne`
        val (before: Xml.Nodes, tail) = parent.getChildren.span(
          _.asElement.fold(true)(element => !element.getHref.contains(ref))
        )
        val it: Xml.Element = tail.head.asElement.get
        val after: Xml.Nodes = tail.tail

        new BackLink(
          to = to,
          from = from,
          transclude = Links.isTranscluded(element),
          kind = LinkKind.of(element),
          context = LinkContext(
            toFrom = toFrom,
            before = before,
            element = it,
            after = after
          )
        )
