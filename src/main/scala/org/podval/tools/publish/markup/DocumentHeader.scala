package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** TEI `teiHeader` fields for the collector `document-header` table. Harvested from the raw tree. */
final class DocumentHeader(
  val description: Option[Xml.Element],
  val date: Option[Xml.Element],
  val authors: Seq[Xml.Element],
  val addressee: Option[Xml.Element],
  val transcribers: Seq[Xml.Element]
):
  def isEmpty: Boolean =
    description.isEmpty && date.isEmpty && authors.isEmpty && addressee.isEmpty && transcribers.isEmpty

object DocumentHeader:
  def harvest(xml: Xml.Element): Option[DocumentHeader] =
    Option.when(xml.localName == "TEI"):
      val header: Option[Xml.Element] = child(xml, "teiHeader")
      val titleStmt: Option[Xml.Element] = header.flatMap(child(_, "fileDesc")).flatMap(child(_, "titleStmt"))
      val profileDesc: Option[Xml.Element] = header.flatMap(child(_, "profileDesc"))
      new DocumentHeader(
        description = profileDesc.flatMap(child(_, "abstract")),
        date = profileDesc.flatMap(child(_, "creation")).flatMap(child(_, "date")),
        authors = titleStmt.toSeq.flatMap(children(_, "author")),
        addressee = profileDesc.flatMap(addresseeOf),
        transcribers = titleStmt.toSeq.flatMap(children(_, "editor")).filter(_.get("role").contains("transcriber"))
      )

  private def addresseeOf(profileDesc: Xml.Element): Option[Xml.Element] =
    profileDesc.gather(el =>
      Option.when(el.localName == "persName" && el.get("role").contains("addressee"))(el)
    ).headOption

  private def children(element: Xml.Element, name: String): Seq[Xml.Element] =
    element.getChildren.flatMap(_.asElement).filter(_.localName == name).toSeq

  private def child(element: Xml.Element, name: String): Option[Xml.Element] =
    children(element, name).headOption
