package org.podval.tools.publish.page

import org.apache.pdfbox.pdmodel.{PDDestinationNameTreeNode, PDDocument, PDDocumentNameDictionary, PDPage}
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.interactive.action.PDActionGoTo
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotationLink
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.{PDNamedDestination, PDPageXYZDestination}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

/** PDFBox round-trip of named dests on link annotations. Does not cover Chromium/Skia PDFs. */
final class PdfNamedDestinationsSpec extends AnyFunSuite:
  test("pageByName reads named destinations from link annotations") {
    val pdf: File = File.createTempFile("named-dests-", ".pdf")
    pdf.deleteOnExit()
    val document = PDDocument()
    try
      val page0 = PDPage()
      val page1 = PDPage()
      document.addPage(page0)
      document.addPage(page1)

      val dest = PDPageXYZDestination()
      dest.setPage(page1)
      dest.setTop(100)

      val dests = PDDestinationNameTreeNode()
      dests.setNames(java.util.Map.of("colophon", dest))
      val names = PDDocumentNameDictionary(document.getDocumentCatalog)
      names.setDests(dests)
      document.getDocumentCatalog.setNames(names)

      val action = PDActionGoTo()
      action.setDestination(PDNamedDestination("colophon"))
      val link = PDAnnotationLink()
      link.setRectangle(PDRectangle(10, 10, 50, 20))
      link.setAction(action)
      page0.getAnnotations.add(link)

      document.save(pdf)
    finally
      document.close()

    assert(PdfNamedDestinations.pageByName(pdf).get("colophon").contains(2))
  }
