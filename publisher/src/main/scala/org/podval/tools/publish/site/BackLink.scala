package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{Ids, Link, LinkKind, WikiLink}
import org.podval.tools.publish.page.FullMarkupPage
import org.podval.xml.Xml
import org.podval.xml.Xml.given

final class BackLink private(
  val to: Link,
  val from: FullMarkupPage,
  val kind: Option[LinkKind],
  val context: LinkContext
)

object BackLink:
  def apply(
    element: Xml.Element, 
    parent: Xml.Element,
    from: FullMarkupPage,
    ids: Ids
  ): Option[BackLink] =
    val kind: Option[LinkKind] = LinkKind.of(element)
    if WikiLink.isTranscluded(element) then None
    else
      for
        ref <- element.getHref
        to <- from.site.pages.resolve(ref, kind, from)
        id <- element.getId
      yield
        val toFrom: Link = Link(from, fragment = ids.resolve(id), isIntrapage = false)
        val (before: Xml.Nodes, tail: Xml.Nodes) = parent.getChildren.span(_ ne element)

        new BackLink(
          to = to,
          from = from,
          kind = kind,
          context = LinkContext(
            toFrom = toFrom,
            before = before,
            element = element,
            after = tail.tail
          )
        )

