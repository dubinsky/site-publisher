package org.podval.tools.publish.page

import org.podval.tools.publish.site.Config
import org.podval.xml.{Xml, XmlAttribute}
import Xml.given

/** Collector four named browsing contexts. Opt-in (`named-windows`); default off. */
object NamedWindows:
  val hierarchy: String = "hierarchyViewer"
  val apparatus: String = "apparatusViewer"
  val text: String = "textViewer"
  val facsimile: String = "facsimileViewer"

  def of(page: Page): String =
    page match
      case _: FacsimilePage => facsimile
      case _: EntityListPage => apparatus
      case _ if page.entityKind.isDefined => apparatus
      case _ if page.doc.flatMap(_.asEntityLists).isDefined => apparatus
      case _ if PageHeader.isCollectionDocument(page) && page.doc.flatMap(_.documentHeader).isDefined =>
        text
      case _ => hierarchy

  def targetAttr(dest: Page): Option[String] =
    Option.when(dest.site.config.namedWindows)(of(dest))

  def hierarchyTarget(config: Config): Option[String] =
    Option.when(config.namedWindows)(hierarchy)

  def setXmlTarget(element: Xml.Element, dest: Page): Xml.Element =
    targetAttr(dest).fold(element)(name => element.set(XmlAttribute.Target, name))
