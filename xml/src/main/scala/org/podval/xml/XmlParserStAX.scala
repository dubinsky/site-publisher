package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import java.io.{InputStream, Reader, StringReader}
import javax.xml.namespace.QName
import javax.xml.stream.{XMLEventReader, XMLInputFactory, XMLStreamException}
import javax.xml.stream.events.{Attribute, Characters, Comment, EndElement, EntityReference, ProcessingInstruction,
  StartElement}

object XmlParserStAX:
  def parse(content: String): Either[XMLStreamException, Xml.Element] =
    parse(StringReader(content))

  def parse(reader: Reader): Either[XMLStreamException, Xml.Element] =
    try parseEventReader(factory.createXMLEventReader(reader))
    catch case e: XMLStreamException => Left(e)

  def parse(stream: InputStream): Either[XMLStreamException, Xml.Element] =
    try parseEventReader(factory.createXMLEventReader(stream))
    catch case e: XMLStreamException => Left(e)

  private def factory: XMLInputFactory =
    // Built-in: com.sun.xml.internal.stream.XMLInputFactoryImp
    val result: XMLInputFactory = XMLInputFactory.newInstance
    // Store `xi:include` is a page reference. Do not expand XInclude or load a DTD/external
    // subset (there is no standard StAX XInclude switch; the JDK factory leaves it off).
    result.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
    result.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    result.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    result

  private def parseEventReader(reader: XMLEventReader): Either[XMLStreamException, Xml.Element] =
    try Right(read(reader))
    catch case e: XMLStreamException => Left(e)
    finally reader.close()

  private def read(reader: XMLEventReader): Xml.Element =
    val builder: XmlBuilder = XmlBuilder()

    while reader.hasNext do reader.nextEvent match
      case startElement: StartElement =>
        builder.startElement(Xml.Element(
          name = fromQName(startElement.getName),
          children = Chunk.empty,
          attributes = Chunk.from(
            startElement.getAttributes.asScala.map(fromAttribute) ++
            startElement.getNamespaces.asScala.map(fromAttribute)
          ),
        ))

      case endElement: EndElement =>
        builder.endElement()

      case characters: Characters =>
        val text: String = characters.getData
        if characters.isCData then
          builder.flushCharacters()
          builder.setCData()
          builder.addCharacters(text)
          builder.flushCharacters()
        else
          builder.addCharacters(text)

      case entityReference: EntityReference =>
        builder.addCharacters(s"&${entityReference.getName};")

      case comment: Comment =>
        builder.comment(comment.getText)

      case processingInstruction: ProcessingInstruction =>
        builder.processingInstruction(
          target = processingInstruction.getTarget,
          data = processingInstruction.getData
        )

      case _ => ()

    builder.result

  private def fromQName(qName: QName): XmlName = XmlName(
    localName = qName.getLocalPart,
    prefix = noneIfEmpty(qName.getPrefix),
    namespace = noneIfEmpty(qName.getNamespaceURI)
  )

  private def noneIfEmpty(string: String): Option[String] =
    Option.when(string.nonEmpty)(string)

  // Note: this takes care of the namespaces too - `Namespace` is derived from `Attribute`,
  // and `NamespaceImpl` handles the `xmlns:` prefix.
  private def fromAttribute(attribute: Attribute): (XmlName, String) = (
    fromQName(attribute.getName),
    attribute.getValue
  )
