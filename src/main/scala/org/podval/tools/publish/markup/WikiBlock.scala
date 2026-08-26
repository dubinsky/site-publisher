package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{HtmlClass, Xml}

final class WikiBlock(
  val id: String
)

object WikiBlock:
  private object BlockClass extends HtmlClass("wiki-block")

  def is(element: Xml.Element): Boolean = element.has(BlockClass)

  def mark(element: Xml.Element, id: String, errorReporter: PageErrorReporter): Xml.Element =
    element.getId match
      case Some(idExisting) =>
        errorReporter.error(
          kind = PageError.NoId,
          message = s"Block id '$id' conflicts with existing id '$idExisting'"
        )
        element
      case None =>
        element.add(BlockClass).setId(id)
