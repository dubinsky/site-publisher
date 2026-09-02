package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import org.xml.sax.{Attributes, InputSource, XMLReader}
import org.xml.sax.ext.LexicalHandler
import org.xml.sax.helpers.DefaultHandler
import java.io.StringReader

// Note: written by Grok, re-written by me ;)
object XmlParserSax:
  def parse(reader: XMLReader, content: String): Either[Throwable, Xml.Element] =
    try
      reader.setFeature("http://xml.org/sax/features/namespaces", true)
      // Include xmlns:* in the attribute list so namespace declarations become attributes
      reader.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
      reader.setFeature("http://xml.org/sax/features/external-general-entities", false)
      reader.setFeature("http://xml.org/sax/features/external-parameter-entities", false)

      val builder: XmlBuilder = XmlBuilder()
      val handler: XmlParserSax = XmlParserSax(builder)
    
      reader.setContentHandler(handler)
      reader.setProperty("http://xml.org/sax/properties/lexical-handler", handler)
      reader.parse(InputSource(StringReader(content)))

      Right(builder.result)
    catch
      case e: Throwable => Left(e)  

private final class XmlParserSax(builder: XmlBuilder) extends DefaultHandler with LexicalHandler:
  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    attributes: Attributes
  ): Unit =
    builder.startElement(Xml.Element(
      name = fromName(uri, localName, qName),
      children = Chunk.empty,
      attributes = fromAttributes(attributes)
    ))

  override def endElement(uri: String, localName: String, qName: String): Unit =
    builder.endElement()

  override def characters(characters: Array[Char], start: Int, length: Int): Unit =
    builder.addCharacters(String(characters, start, length))

  override def ignorableWhitespace(characters: Array[Char], start: Int, length: Int): Unit =
    builder.addCharacters(String(characters, start, length))

  override def processingInstruction(target: String, data: String): Unit =
    builder.processingInstruction(target = target, data = data)

  // LexicalHandler

  override def startDTD(name: String, publicId: String, systemId: String): Unit = ()

  override def endDTD(): Unit = ()

  override def startEntity(name: String): Unit = ()

  override def endEntity(name: String): Unit = ()

  override def startCDATA(): Unit =
    builder.flushCharacters()
    builder.setCData()

  override def endCDATA(): Unit =
    builder.flushCharacters()

  override def comment(characters: Array[Char], start: Int, length: Int): Unit =
    builder.comment(String(characters, start, length))

private def fromName(uri: String, localName: String, qName: String): XmlName =
  val (prefix: Option[String], local: String) =
    if localName.nonEmpty then
      val colon: Int = qName.indexOf(':')
      val p: Option[String] = Option.when(colon >= 0)(qName.substring(0, colon)).filter(_.nonEmpty)
      (p, localName)
    else
      val colon: Int = qName.indexOf(':')
      if colon >= 0 then (Some(qName.substring(0, colon)), qName.substring(colon + 1))
      else (None, qName)

  XmlName(
    localName = local,
    prefix = prefix,
    // TagSoup puts the XHTML namespace on every HTML element; drop it.
    namespace = noneIfEmpty(uri).filterNot(_ == XmlNamespace.xhtml)
  )

private def fromAttributes(attributes: Attributes): Chunk[(XmlName, String)] =
  Chunk.from((0 until attributes.getLength).map: i =>
    (
      fromName(
        uri = attributes.getURI(i),
        localName = attributes.getLocalName(i),
        qName = attributes.getQName(i)
      ),
      attributes.getValue(i)
    )
  )

private def noneIfEmpty(string: String): Option[String] =
  Option.when(string.nonEmpty)(string)

