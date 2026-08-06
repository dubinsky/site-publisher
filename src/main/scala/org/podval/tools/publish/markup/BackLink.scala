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
      val toId: Option[Link.ToId] = ids.resolve(id)
      val toFrom: Link = Link(from, fragment = toId, isIntrapage = false)
      // TODO go back to `ne`
      val (before: Xml.Nodes, tail) = parent.getChildren.span(
        _.asElement.fold(true)(element => !element.getHref.contains(ref))
      )
      val it: Xml.Element = tail.head.asElement.get
      val after: Xml.Nodes = tail.tail

      new BackLink(
        to = to,
        from = from,
        transclude = WikiLink.isTranscluded(element),
        kind = LinkKind.of(element),
        context = LinkContext(
          toFrom = toFrom,
          before = before,
          element = it,
          after = after
        )
      )

