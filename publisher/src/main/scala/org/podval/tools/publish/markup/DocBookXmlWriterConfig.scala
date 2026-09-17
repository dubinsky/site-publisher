package org.podval.tools.publish.markup

import org.podval.xml.XmlWriterConfig

object DocBookXmlWriterConfig extends XmlWriterConfig(
  preformat = Set("programlisting", "screen", "literallayout"),
  nest = Set("para", "simpara", "title"),
  cling = Set("footnote", "xref", "co")
)
