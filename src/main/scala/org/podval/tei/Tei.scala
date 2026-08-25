package org.podval.tei

import org.podval.xml.{RawXml, WithRawXml, Xml, XmlParser}
import zio.blocks.schema.derive.Deriver
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.schema.xml.{XmlCodec, XmlCodecError, XmlFormat}
import zio.blocks.typeid.TypeId

// TODO @xmlAttribute("id") and @xmlNamespace() annotations mentioned in the documentation
// do not work! Raw @Modifier.config("xml.attribute", "") does...
// File te issue.

//@Modifier.config("xml.namespace.uri", "http://www.tei-c.org/ns/1.0")
//@Modifier.config("xml.namespace.prefix", "tei")
final case class TEI(
  teiHeader: TeiHeader,
  text: Text
)

final case class Text(
  @Modifier.config("xml.attribute", "") lang: Option[String] = None,
  body: RawXml
)

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[RawXml],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[RawXml],
  revisionDesc: Option[RawXml]
)

// PublicationStmt and SourceDesc are mandatory (TEI Guidelines),
// but I made them optional so that they can be removed from the editable pre-TEI files
// (and added programmatically to the version published on the site).
final case class FileDesc(
  titleStmt: TitleStmt,
  editionStmt: Option[RawXml],
  extent: Option[RawXml],
  publicationStmt: Option[PublicationStmt],
  seriesStmt: Option[RawXml],
  notesStmt: Option[RawXml],
  sourceDesc: Option[RawXml]
)

// TODO how do I bind to all of them being possibly multiple?
final case class TitleStmt(
  title: Option[RawXml] = None,
  author: Option[RawXml] = None,
  editor: Option[Editor] = None,
  sponsor: Option[RawXml] = None,
  funder: Option[RawXml] = None,
  principal: Option[RawXml] = None,
  respStmt: Option[RawXml] = None
)

final case class Editor(
  @Modifier.config("xml.attribute", "") role: Option[String],
  persName: Option[EntityReference] // TODO does not parse; probably should not be optional
)

final case class PublicationStmt(
  publisher: Option[RawXml],
  availability: Option[Availability]
)

final case class Availability(
  @Modifier.config("xml.attribute", "") status: Option[String]
) extends WithRawXml

// TODO deal with choices here - using a variant?
final case class ProfileDesc(
  documentAbstract: Option[RawXml],
  creation: Option[Creation],
  langUsage: Option[LangUsage],
  textClass: Option[RawXml],
  correspDesc: Option[RawXml],
  calendarDesc: Option[RawXml],
  handNotes: Option[HandNotes],
  listTranspose: Option[RawXml]
)

final case class Creation(
  date: Date
) extends WithRawXml

final case class Date(
  @Modifier.config("xml.attribute", "") when: String,
  @Modifier.config("xml.attribute", "") calendar: Option[String]
) extends WithRawXml

final case class LangUsage(
  languages: Seq[Language]
)

final case class Language(
  @Modifier.config("xml.attribute", "") ident: String,
  @Modifier.config("xml.attribute", "") usage: Option[Int],
  text: Option[String] // TODO Text
)

final case class HandNotes(
  handNotes: Seq[HandNote]
)

final case class HandNote(
  handNote: RawXml
)

object Tei:
  val mimeType = "application/tei+xml"
  val dtdId: String = "-//TEI P5"

  given schema: Schema[TEI] = Schema.derived

  val codecDeriver: Deriver[XmlCodec] = XmlFormat.deriver
    .withInstance(RawXml.codec)
    // TODO I would like this to be less hand-made ;)
    .withInstance(WithRawXml.codec(Schema.derived[Availability]))
    .withInstance(WithRawXml.codec(Schema.derived[Creation], codecDeriver = Some(
      XmlFormat.deriver
        .withInstance(WithRawXml.codec(Schema.derived[Date]))
    )))
    .withInstance(WithRawXml.codec(Schema.derived[Date]))
    .withInstance(WithRawXml.codec(Schema.derived[Entity]))
    .withInstance(WithRawXml.codec(Schema.derived[EntityReference]))

  private val codec: XmlCodec[TEI] = schema.deriving(codecDeriver)
    .instance(TypeId.of[Date], WithRawXml.codec(Schema.derived[Date]))
    .instance(TypeId.of[Creation], WithRawXml.codec(Schema.derived[Creation]))
    .derive

  def parse(content: String): Either[Throwable, TEI] = XmlParser.parseXml(content).flatMap: xml =>
    // TODO working around ZIO Blocks XML bug where attributes are discarded if the element does not have sub-elements;
    // report it!
    val xmlEffective = xml.transform(
      element =>
        if element.attributes.isEmpty || element.children.exists(_.isInstanceOf[Xml.Element]) // TODO compiler warning!
        then element
        else element.copy(children = element.children :+ Xml.element(WithRawXml.dummyElement)),
      stopAtCode = false
    )
    try Right(codec.decodeValue(xmlEffective))
    catch case error: XmlCodecError => Left(error)

  def main(args: Array[String]): Unit =
    val content = org.podval.tools.publish.util.Files.read(java.io.File(
      "/home/dub/OpenTorah/alter-rebbe.org/archive/rgada/category/VII/inventory/2/case/3140/029.xml"
    ))
    Tei.parse(content) match
      case Left(error) => println(error)
      case Right(tei) =>
        println(tei)
        val t = tei.teiHeader.fileDesc.titleStmt
        val x = 0

//    println(codec.encodeToString(tei, WriterConfig.pretty)) // TODO I will, of course, encode it to XML AST and write it myself ;)
