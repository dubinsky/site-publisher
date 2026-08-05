package org.podval.tools.publish.markup

import org.podval.tools.publish.page.OriginalMarkupPage
import org.podval.xml.{Xml, XmlDialect}

final class BackLink(
  val to: Link,
  val from: OriginalMarkupPage,
  val transclude: Boolean,
  val kind: Option[LinkKind],
  val context: LinkContext
)

object BackLink:
  def backLinks(
    xml: Xml.Element,
    xmlDialect: XmlDialect,
    from: OriginalMarkupPage,
    ids: Ids
  ): Seq[BackLink] =
    def backLink(element: Xml.Element, parentOpt: Option[Xml.Element]): Option[BackLink] =
      if !(element.isA && element.has(Links.InternalLinkClass)) then None else
        val parent: Xml.Element = parentOpt.get
        for
          ref <- element.getHref
          to <- Link.resolve(ref, kind = None, from)
          id <- element.getId
        yield
          val toId: Option[Link.ToId] = ids.resolve(id)
          val toFrom: Link = Link(from, fragment = toId, intrapage = false)
          // TODO go back to `ne`
          val (before: Xml.Nodes, tail) = parent.getChildren.span(
            _.asElement.fold(true)(element => !element.getHref.contains(ref))
          )
          val it: Xml.Element = tail.head.asElement.get
          val after: Xml.Nodes = tail.tail

          BackLink(
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

    xmlDialect.gatherWithParent(element = xml, gatherElement = backLink)
