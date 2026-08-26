package org.podval.tools.publish.markup

import org.podval.xml.{Html, HtmlXmlDialect}
import org.scalatest.funsuite.AnyFunSuite

final class PagedListSpec extends AnyFunSuite:
  test("batchCount and slice") {
    assert(PagedList.batchCount(25, 10) == 3)
    assert(PagedList.slice(1 to 5, 2, 2) == Seq(3, 4))
  }

  test("nav: page 1 is newest, so Older goes forward and Newer back") {
    val first: String = HtmlXmlDialect.render(PagedList.nav(1, 3, i => s"/$i"))
    assert(first.contains("""class="pagination""""), first)
    assert(!first.contains("Newer"), first)
    assert(first.contains("Older"), first)
    assert(first.contains("""aria-current="page""""), first)
    val last: String = HtmlXmlDialect.render(PagedList.nav(3, 3, i => s"/$i"))
    assert(last.contains("Newer"), last)
    assert(!last.contains("Older"), last)
  }