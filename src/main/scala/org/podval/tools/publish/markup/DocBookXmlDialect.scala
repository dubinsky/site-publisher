package org.podval.tools.publish.markup

import org.podval.xml.XmlDialect

object DocBookXmlDialect extends XmlDialect(
  preformat = Set("programlisting", "screen", "literallayout"),
  nest = Set("para", "simpara", "title"),
  cling = Set("footnote", "xref", "co")
)
