package org.podval.tools.publish

// A bare package clause is not a definition, so Scala 3 drops its doc.
// This empty package object is what attaches the comment to `markup`.
/** Dialects: `HtmlMarkup`, `MarkdownMarkup`, `AsciiDocMarkup`, `TeiMarkup`, `DocBookMarkup`
  * (`XmlMarkup` is the shared `.xml` extension).
  * Shared IR stays in this package (`Footnote`, `Section`, `Citation`, `Figure`, …).
  * Pipeline: dub.podval.org note `Publishing/Site Publisher`, Design / Pipeline.
  * TEI and DocBook emit that IR in their converters.
  * Markdown, AsciiDoc, and HTML finish in `HtmlIr.normalize`.
  * `Strike.normalize` must not see a TEI `s`. TEI `s` is a sentence.
  */
package object markup
