package org.podval.xml

import org.ccil.cowan.tagsoup.Parser as TagSoupParser
import org.xml.sax.{Attributes, XMLFilter, XMLReader}
import org.xml.sax.helpers.XMLFilterImpl

// Note: TagSoup is not suitable for case-sensitive or namespaced XML dialects such as TEI, since it:
// - does not support namespaces at all;
// - lower-cases element and attribute names.
object TagSoup:
  def reader: XMLReader =
    val tagSoup: XMLReader = TagSoupParser()
    tagSoup.setFeature(TagSoupParser.rootBogonsFeature, true)
    // Do not invent HTML default attributes (e.g. <br clear="none">).
    tagSoup.setFeature(TagSoupParser.defaultAttributesFeature, false)

    val tagSoupHtmlBodyRemover: XMLFilter = TagSoupHtmlBodyRemover()
    tagSoupHtmlBodyRemover.setParent(tagSoup)
    tagSoupHtmlBodyRemover
    
  // TagSoup always wraps fragments in <html><body>...</body></html>;
  // this filter removes the wrapper.
  private final class TagSoupHtmlBodyRemover extends XMLFilterImpl:
    private def suppress(localName: String): Boolean =
      "html".equalsIgnoreCase(localName) || "body".equalsIgnoreCase(localName)

    override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit =
      if !suppress(localName) then super.startElement(uri, localName, qName, attributes)

    override def endElement(uri: String, localName: String, qName: String): Unit =
      if !suppress(localName) then super.endElement(uri, localName, qName)
