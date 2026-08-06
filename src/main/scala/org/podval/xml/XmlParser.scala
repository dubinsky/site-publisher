package org.podval.xml

import zio.blocks.schema.xml.Xml
import javax.xml.parsers.SAXParserFactory

object XmlParser:
  def parse(content: String, isXml: Boolean): Either[Throwable, Xml.Element] = 
    if isXml then parseXml(content) else parseHtml(content)
  
  def parseXml(content: String): Either[Throwable, Xml.Element] = parseStax(content)

  def parseHtml(content: String): Either[Throwable, Xml.Element] = parseSaxHtml(content)

  private def parseStax(content: String): Either[Throwable, Xml.Element] =
    XmlParserStAX.parse(content)
  
  private def parseSaxXml(content: String): Either[Throwable, Xml.Element] =
    XmlParserSax.parse(content = content, reader = SAXParserFactory.newInstance.newSAXParser.getXMLReader)

  private def parseSaxHtml(content: String): Either[Throwable, Xml.Element] =
    XmlParserSax.parse(content = content, reader = TagSoup.reader)
