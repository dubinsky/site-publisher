package org.podval.tools.publish.site

import org.scalatest.funsuite.AnyFunSuite

final class PathSpec extends AnyFunSuite:
  test("toString, html, withoutHtml, add") {
    val notes: Path = Path("notes").html
    assert(notes.toString == "/notes.html")
    assert(notes.fileName == "notes")
    assert(notes.withoutHtml.toString == "/notes")
    assert(notes.withoutHtml.withoutHtml.toString == "/notes")
    assert(Path("dir", "page").html.toString == "/dir/page.html")
    assert(Path("dir").add("page").toString == "/dir/page")
    assert(Path.root.toString == "/")
  }

  test("relativize joins relative aliases from the parent directory") {
    val page: Path = Path("dir", "page").html
    assert(page.relativize("sib").toString == "/dir/sib")
    assert(page.relativize("sub/page").toString == "/dir/sub/page")
    val notes: Path = Path("notes").html
    assert(notes.relativize("notes-alias").toString == "/notes-alias")
  }

  test("relativize absolute aliases start at site root") {
    val page: Path = Path("dir", "page").html
    assert(page.relativize("/other").toString == "/other")
    assert(page.relativize("/a/b").toString == "/a/b")
  }

  test("relativize drops . segments") {
    val page: Path = Path("dir", "page").html
    assert(page.relativize("./sib").toString == "/dir/sib")
    assert(page.relativize("/a/./b").toString == "/a/b")
  }

  test("relativize resolves .. and clamps at site root") {
    val page: Path = Path("dir", "page").html
    assert(page.relativize("../sib").toString == "/sib")
    assert(page.relativize("../../outside").toString == "/outside")
    assert(page.relativize("/a/b/../c").toString == "/a/c")
    val notes: Path = Path("notes").html
    assert(notes.relativize("../other").toString == "/other")
    assert(notes.relativize("../../escape").toString == "/escape")
  }

  test("fromHref keeps the last-segment extension") {
    val toc: Path = Path.fromHref("/book/book/index.html")
    assert(toc.toString == "/book/book/index.html")
    assert(toc.fileName == "index")
    assert(toc.extension.contains("html"))
    assert(Path.fromHref("index.html").toString == "/index.html")
    assert(Path.fromHref("/book/").toString == "/book")
    assert(Path.fromHref("/a/./b/../c.html").toString == "/a/./b/../c.html")
  }

  test("resolveFrom joins relative html hrefs to the page directory") {
    val chunk: Path = Path("book", "book", "Alpha").html
    assert(chunk.resolveFrom("index.html").toString == "/book/book/index.html")
    assert(chunk.resolveFrom("./Beta.html").toString == "/book/book/Beta.html")
    assert(chunk.resolveFrom("../index.html").toString == "/book/index.html")
    assert(chunk.resolveFrom("/book/book/index.html").toString == "/book/book/index.html")
    assert(chunk.resolveFrom("/a/./b/../c.html").toString == "/a/c.html")
    val toc: Path = Path("chunked", "index").html
    assert(toc.resolveFrom("Alpha.html").toString == "/chunked/Alpha.html")
  }

  test("isRelativeFileHref is html files and ./ ../, not wiki names") {
    assert(Path.isRelativeFileHref("index.html"))
    assert(Path.isRelativeFileHref("./Alpha.html"))
    assert(Path.isRelativeFileHref("../index.html"))
    assert(!Path.isRelativeFileHref("notes"))
    assert(!Path.isRelativeFileHref("Nested Book"))
    assert(!Path.isRelativeFileHref("book/book"))
    assert(!Path.isRelativeFileHref("/book/book/index.html"))
  }
