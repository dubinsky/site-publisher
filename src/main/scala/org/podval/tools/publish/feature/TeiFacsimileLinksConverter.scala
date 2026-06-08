package org.podval.tools.publish.feature

import org.podval.tools.publish.processor.ConverterSimple
import org.podval.xml.Xml

private object TeiFacsimileLinksConverter:
  private val facsimileSymbol: String = "⎙"

final class TeiFacsimileLinksConverter extends ConverterSimple:
  override def convert(element: Xml.Element): Xml.Element =
    if element.getName != "pb"
    then element
      // TODO convert 'n' attribute?
    else renameElement("a", element.setText(TeiFacsimileLinksConverter.facsimileSymbol))
