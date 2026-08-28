package org.podval.tei

// TODO @xmlAttribute("id") and @xmlNamespace() annotations mentioned in the documentation
// do not work! Raw @Modifier.config("xml.attribute", "") does...
// File the issue.
// TODO working around ZIO Blocks XML bug where attributes are discarded if the element does not have sub-elements;
// report it!

//@Modifier.config("xml.namespace.uri", "http://www.tei-c.org/ns/1.0")
//@Modifier.config("xml.namespace.prefix", "tei")
//final case class TEI(
//  text: Text
//)
//
//final case class Text(
//  @Modifier.config("xml.attribute", "") lang: Option[String] = None,
//  body: RawXml
//)
