package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import java.io.StringReader
import javax.xml.namespace.QName
import javax.xml.stream.{XMLEventReader, XMLInputFactory, XMLStreamException}
import javax.xml.stream.events.{Attribute, Characters, Comment, EndElement, EntityReference, ProcessingInstruction,
  StartElement}

object XmlParserStAX:
  def parse(content: String): Either[XMLStreamException, Xml.Element] =
    try Right(parseInternal(content))
    catch case e: XMLStreamException => Left(e)

  private def parseInternal(content: String): Xml.Element =
    // Built-in: com.sun.xml.internal.stream.XMLInputFactoryImp
    val factory: XMLInputFactory = XMLInputFactory.newInstance
    factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
    val reader: XMLEventReader = factory.createXMLEventReader(new StringReader(content))

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

      case xmlEvent => ()

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
