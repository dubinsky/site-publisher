package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** HTML-shaped leftovers → shared IR.
  * Dialect soup (`quoteblock`, `[!tip]`, TEI `cit`, …) is converted on `XxxMarkup` first.
  * This is the common tail for HTML, Markdown, and AsciiDoc (`HtmlMarkup.process`).
  * TEI does not run it yet: leftovers are still TEI names, not HTML. */
object HtmlIr:
  private val passes: List[Xml.Element => Xml.Element] = List(
    Aside.normalize,
    Quote.normalize,
    Strike.normalize,
    Figure.normalize,
    PdfEmbed.normalize
  )

  def normalize(xml: Xml.Element): Xml.Element =
    xml.transform(element => passes.foldLeft(element)((el, pass) => pass(el)))
