package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Xml

// Remove body stubs
final class FootnoteBodiesTransformer(source: PageSource) extends Transformer:
  override def transform(element: Xml.Element): Xml.Element =
    source.xmlDialect.transform(element, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          val remove: Boolean =
            child.has(Footnotes.BodyClass) ||
              source.markup.isSpuriousFootnotesDiv(element)
          // TODO AsciiDoc footnotes div does not get removed!
          if remove then
            val x = 0
          remove
        ))
      )
    )
    