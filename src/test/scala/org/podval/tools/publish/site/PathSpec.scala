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
