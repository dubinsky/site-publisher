package org.podval.tools.publish.page

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.interactive.action.PDActionGoTo
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotationLink
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.{
  PDDestination, PDNamedDestination, PDPageDestination
}
import java.io.File
import scala.jdk.CollectionConverters.ListHasAsScala

/** Named destinations as Chromium wrote them from HTML `id`s (1-based page numbers). */
object PdfNamedDestinations:
  def pageByName(pdf: File): Map[String, Int] =
    val document: PDDocument = Loader.loadPDF(pdf)
    try
      val result = scala.collection.mutable.Map.empty[String, Int]
      val pages = document.getPages
      var i: Int = 0
      while i < pages.getCount do
        val annotations = pages.get(i).getAnnotations
        if annotations != null then
          annotations.asScala.foreach:
            case link: PDAnnotationLink =>
              destinationOf(link).foreach: dest =>
                nameOf(dest).foreach: name =>
                  pageNumber(document, dest).foreach: number =>
                    result(name) = number
            case _ =>
        i += 1
      result.toMap
    finally
      document.close()

  private def destinationOf(link: PDAnnotationLink): Option[PDDestination] =
    Option(link.getDestination).orElse:
      Option(link.getAction).collect:
        case go: PDActionGoTo => go.getDestination

  private def nameOf(dest: PDDestination): Option[String] = dest match
    case named: PDNamedDestination => Option(named.getNamedDestination)
    case _ => None

  private def pageNumber(document: PDDocument, dest: PDDestination): Option[Int] = dest match
    case named: PDNamedDestination =>
      Option(document.getDocumentCatalog.findNamedDestinationPage(named)).flatMap(pageNumber(document, _))
    case pageDest: PDPageDestination =>
      val number: Int = pageDest.retrievePageNumber()
      if number >= 0 then Some(number + 1)
      else Option(pageDest.getPage).map(page => document.getPages.indexOf(page) + 1).filter(_ > 0)
    case _ => None
