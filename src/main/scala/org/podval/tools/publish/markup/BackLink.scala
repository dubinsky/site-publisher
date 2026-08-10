package org.podval.tools.publish.markup

import org.podval.tools.publish.page.FullMarkupPage
import org.podval.xml.Xml

final class BackLink private(
  val to: Link,
  val from: FullMarkupPage,
  val transclude: Boolean,
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
    for
      ref <- element.getHref
      to <- Link.resolve(ref, kind = None, from)
      id <- element.getId
    yield
      val toFrom: Link = Link(from, fragment = ids.resolve(id), isIntrapage = false)
      val (before: Xml.Nodes, tail: Xml.Nodes) = parent.getChildren.span(_ ne element)

      new BackLink(
        to = to,
        from = from,
        transclude = WikiLink.isTranscluded(element),
        kind = LinkKind.of(element),
        context = LinkContext(
          toFrom = toFrom,
          before = before,
          element = element,
          after = tail.tail
        )
      )

