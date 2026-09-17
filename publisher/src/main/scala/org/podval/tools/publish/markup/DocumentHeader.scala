package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlAttribute}

/** TEI `teiHeader` fields for the collector `document-header` table. Harvested from the raw tree. */
final class DocumentHeader(
  val description: Option[Xml.Element],
  val date: Option[Xml.Element],
  val authors: Seq[Xml.Element],
  val addressee: Option[Xml.Element],
  val transcribers: Seq[Xml.Element],
  val lang: Option[String],
  val pbs: Seq[Pb]
):
  def isEmpty: Boolean =
    description.isEmpty && date.isEmpty && authors.isEmpty && addressee.isEmpty && transcribers.isEmpty

object DocumentHeader:
  def harvest(xml: Xml.Element): Option[DocumentHeader] =
    Option.when(xml.isNamed("TEI")):
      val header: Option[Xml.Element] = child(xml, "teiHeader")
      val titleStmt: Option[Xml.Element] = header.flatMap(child(_, "fileDesc")).flatMap(child(_, "titleStmt"))
      val profileDesc: Option[Xml.Element] = header.flatMap(child(_, "profileDesc"))
      new DocumentHeader(
        description = profileDesc.flatMap(child(_, "abstract")),
        date = profileDesc.flatMap(child(_, "creation")).flatMap(child(_, "date")),
        authors = titleStmt.toSeq.flatMap(children(_, "author")),
        addressee = profileDesc.flatMap(addresseeOf),
        transcribers = titleStmt.toSeq.flatMap(children(_, "editor")).filter(_.get(XmlAttribute.Role).contains("transcriber")),
        lang = textLang(xml),
        pbs = Pb.harvest(xml)
      )

  private def textLang(xml: Xml.Element): Option[String] =
    child(xml, "text").flatMap: text =>
      text.get(XmlAttribute.XmlLang).orElse(text.get(XmlAttribute.Lang)).map(_.trim).filter(_.nonEmpty)

  private def addresseeOf(profileDesc: Xml.Element): Option[Xml.Element] =
    profileDesc.gather(el =>
      Option.when(el.isNamed("persName") && el.get(XmlAttribute.Role).contains("addressee"))(el)
    ).headOption

  private def children(element: Xml.Element, name: String): Seq[Xml.Element] =
    element.getChildren.flatMap(_.asElement).filter(_.isNamed(name)).toSeq

  private def child(element: Xml.Element, name: String): Option[Xml.Element] =
    children(element, name).headOption
