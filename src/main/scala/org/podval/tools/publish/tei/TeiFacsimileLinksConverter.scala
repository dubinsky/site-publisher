package org.podval.tools.publish.tei

import org.podval.tools.publish.processor.Converter
import org.podval.xml.Xml

private object TeiFacsimileLinksConverter:
  private val facsimileSymbol: String = "⎙"

final class TeiFacsimileLinksConverter extends Converter:
  override protected def convert(element: Xml.Element): Option[Xml.Element] =
    // TODO convert 'n' attribute?
    Option.when(element.getName == "pb")(
      renameElement("a", element.setText(TeiFacsimileLinksConverter.facsimileSymbol))
    )
