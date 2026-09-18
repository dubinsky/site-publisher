package org.podval.tools.publish.page

import scala.annotation.tailrec

object GraphOwner:
  @tailrec
  def of(page: Page): Option[FullMarkupPage] = page match
    case a: Alias => of(a.real)
    case f: FullMarkupPage => Some(f)
    case c: ChunkedMarkupPage => Some(c.markupPage)
    case fac: FacsimilePage => Some(fac.document)
    case pdf: PdfPage => Some(pdf.markupPage)
    case _ => None
