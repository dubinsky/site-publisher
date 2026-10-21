package org.podval.tools.publish.site

import org.podval.metadata.{Term, TermCatalog}
import org.podval.xml.XmlParser

/** Built-in axes and interface strings (`Terms.xml`). */
object Terms:
  val catalog: TermCatalog = TermCatalog(XmlParser.loadCatalog(this, Term.codec))
