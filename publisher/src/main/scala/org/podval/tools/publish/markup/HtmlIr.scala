package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** HTML-shaped leftovers → shared IR.
  * Dialect soup (`quoteblock`, `[!tip]`, TEI `cit`, `videoblock`, …) is converted on `XxxMarkup` first.
  * Markdown, AsciiDoc, and HTML run this from `HtmlMarkup.process`.
  * TEI and DocBook emit the same IR in their converters and do not run this pass.
  * TEI `<s>` is a sentence; `Strike.normalize` would rename it to `<del>`. */
object HtmlIr:
  private val passes: List[Xml.Element => Xml.Element] = List(
    Aside.normalize,
    Quote.normalize,
    Strike.normalize,
    Figure.normalize,
    PdfEmbed.normalize,
    Video.normalize
  )

  def normalize(xml: Xml.Element): Xml.Element =
    xml.transform(element => passes.foldLeft(element)((el, pass) => pass(el)))
