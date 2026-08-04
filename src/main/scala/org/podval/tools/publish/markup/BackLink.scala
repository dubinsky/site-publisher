package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{OriginalMarkupPage, Page, PageContent}
import org.podval.xml.{Xml, XmlDialect}

final class BackLink private(
  val to: Link,
  val from: Page, // TODO OriginalMarkupPage
  val transclude: Boolean,
  val kind: Option[LinkKind],
  val context: LinkContext
)

object BackLink:
  def backLinks(
    xml: Xml.Element,
    xmlDialect: XmlDialect,
    page: OriginalMarkupPage,
    ids: Ids
  ): Seq[BackLink] = xmlDialect.gatherWithParents(
    element = xml,
    gatherElement = (element, parents) => BackLink(
      element = element,
      parents = parents,
      from = page,
      ids = ids
    )
  )
  
  def apply(
    element: Xml.Element,
    parents: Seq[Xml.Element],
    from: Page,
    ids: Ids,
  ): Option[BackLink] =
    if !(element.isA && element.has(Links.InternalLinkClass)) then None else
      for
        ref <- element.getHref
        to <- Link.resolve(ref, kind = None, from)
        id <- element.getId
      yield
        val toId: Option[Link.ToId] = ids.resolve(id)
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
