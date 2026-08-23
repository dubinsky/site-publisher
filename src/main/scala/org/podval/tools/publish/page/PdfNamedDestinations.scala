package org.podval.tools.publish.page

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.{PDDocument, PDPageTree}
import org.apache.pdfbox.pdmodel.interactive.action.PDActionGoTo
import org.apache.pdfbox.pdmodel.interactive.annotation.{PDAnnotation, PDAnnotationLink}
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.{PDDestination, PDNamedDestination, PDPageDestination}
import scala.jdk.CollectionConverters.ListHasAsScala
import java.io.File

/** Named destinations as Chromium wrote them from HTML `id`s (1-based page numbers). */
object PdfNamedDestinations:
  def pageByName(pdf: File): Map[String, Int] =
    val document: PDDocument = Loader.loadPDF(pdf)

    def pageNumber(destination: PDDestination): Option[Int] = destination match
      case named: PDNamedDestination =>
        Option(document.getDocumentCatalog.findNamedDestinationPage(named)).flatMap(pageNumber)
      case pageDest: PDPageDestination =>
        val number: Int = pageDest.retrievePageNumber()
        if number >= 0 then Some(number + 1)
        else Option(pageDest.getPage).map(page => document.getPages.indexOf(page) + 1).filter(_ > 0)
      case _ => None

    try
      val pages: PDPageTree = document.getPages
      val pairs: Seq[(String, Int)] = for
        i <- 0 until pages.getCount
        annotations <- Option(pages.get(i).getAnnotations).toSeq
        annotation: PDAnnotation <- annotations.asScala.toSeq
        link: PDAnnotationLink <- annotation match
          case link: PDAnnotationLink => Some(link)
          case _ => None
        destination <- Option(link.getDestination).orElse:
          Option(link.getAction).collect:
            case go: PDActionGoTo => go.getDestination
        name <- destination match
          case named: PDNamedDestination => Option(named.getNamedDestination)
          case _ => None
        number <- pageNumber(destination)
      yield name -> number
      pairs.toMap
    finally
      document.close()
