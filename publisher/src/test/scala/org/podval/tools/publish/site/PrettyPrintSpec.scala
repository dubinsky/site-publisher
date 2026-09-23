package org.podval.tools.publish.site

import org.podval.tools.publish.markup.TeiXmlWriterConfig
import org.podval.tools.publish.util.SiteOptions
import org.podval.xml.Xml
import org.podval.xml.Xml.given
import org.scalatest.funsuite.AnyFunSuite
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class PrettyPrintSpec extends AnyFunSuite:
  private val siteConfig: String =
    """title: Pretty
      |description: Pretty-print tests
      |url: http://example.test
      |author: A
      |email: a@example.test
      |""".stripMargin

  private def withSite(files: Map[String, String])(body: (Site, NioPath) => Unit): Unit =
    val dir: NioPath = NioFiles.createTempDirectory("site-publisher-pretty")
    try
      (files + ("_site_config.yml" -> siteConfig)).foreach: (name, text) =>
        val path: NioPath = dir.resolve(name)
        Option(path.getParent).foreach(parent => NioFiles.createDirectories(parent))
        NioFiles.writeString(path, text)
      val site: Site = Site(SiteOptions(sourceDirectoryPath = dir.toString, prettyPrint = true))
      body(site, dir)
    finally
      deleteTree(dir)

  private def deleteTree(dir: NioPath): Unit =
    if NioFiles.exists(dir) then
      NioFiles.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete)

  private def text(dir: NioPath, name: String): String =
    NioFiles.readString(dir.resolve(name))

  private def xmlnsCount(value: String): Int =
    Iterator.unfold(0) { from =>
      val at: Int = value.indexOf("xmlns=", from)
      if at < 0 then None else Some((at, at + "xmlns=".length))
    }.size

  test("a long verse line keeps the words on the l tags") {
    val words: String = "вступившія въ " + "слово " * 20 + "означенна"
    val verse: Xml.Element = Xml.element("p").setChildren(Seq(
      Xml.element("l").setText("short"),
      Xml.element("l").setText(words),
      Xml.element("l").setChildren(Seq(
        Xml.element("date").setText("Ноября 18 го дня " * 8),
        Xml.text(".")
      ))
    ))
    val dumped: String = TeiXmlWriterConfig.render(verse)
    val lines: Array[String] = dumped.split("\n")
    assert(lines.exists(line => line.contains("<l>short</l>")), dumped)
    assert(lines.exists(line => line.contains("<l>вступившія")), dumped)
    assert(lines.exists(line => line.contains("означенна</l>")), dumped)
    assert(lines.exists(line => line.contains("<l><date")), dumped)
    assert(!lines.exists(_.trim == "<l>"), dumped)
    assert(!lines.exists(_.trim == "</l>"), dumped)
  }

  test("TEI document keeps one xmlns, clung note, and date attribute") {
    val source: String =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<TEI xmlns="http://www.tei-c.org/ns/1.0">
        |<p xml:lang="ru">word<note>n</note> <date when="1800-01-01">x</date></p>
        |</TEI>
        |""".stripMargin
    withSite(Map("doc.xml" -> source)): (site, dir) =>
      site.prettyPrint()
      val once: String = text(dir, "doc.xml")
      assert(xmlnsCount(once) == 1, once)
      assert(once.contains("word<note>"), once)
      assert(once.contains("""when="1800-01-01""""), once)
      assert(once.contains("""xml:lang="ru""""), once)
      val mtime = NioFiles.getLastModifiedTime(dir.resolve("doc.xml"))
      site.prettyPrint()
      assert(text(dir, "doc.xml") == once)
      assert(NioFiles.getLastModifiedTime(dir.resolve("doc.xml")).equals(mtime))
  }

  test("person names stack and a store keeps includes, a comment, and an empty name") {
    withSite(Map(
      "alter.xml" ->
        """<person xmlns="http://www.tei-c.org/ns/1.0" role="jew">
          |<persName>One</persName><persName>Two</persName>
          |<p>text</p>
          |</person>
          |""".stripMargin,
      "archive.xml" ->
        """<store xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xi="http://www.w3.org/2001/XInclude">
          |<by selector="archive">
          |<xi:include href="books.xml"/>
          |<xi:include href="other.xml"/>
          |<!-- <xi:include href"broken.xml"/> -->
          |<name n="книги"/>
          |</by>
          |</store>
          |""".stripMargin
    )): (site, dir) =>
      site.prettyPrint()
      val person: String = text(dir, "alter.xml")
      assert(person.contains("<persName>One</persName>"), person)
      assert(person.contains("<persName>Two</persName>"), person)
      assert(xmlnsCount(person) == 1, person)
      assert(person.contains("""role="jew""""), person)
      val store: String = text(dir, "archive.xml")
      assert(store.contains("""href="books.xml""""), store)
      assert(store.contains("""href="other.xml""""), store)
      assert(!store.contains("<xi:include href=\"books.xml\">"), store)
      assert(store.contains("broken.xml"), store)
      assert(store.contains("""<name n="книги"/>"""), store)
      assert(store.contains("<by"), store)
      assert(store.contains("xmlns:tei="), store)
      assert(store.contains("xmlns:xi="), store)
      assert(store.indexOf("xmlns:tei=") == store.lastIndexOf("xmlns:tei="), store)
      assert(store.indexOf("xmlns:xi=") == store.lastIndexOf("xmlns:xi="), store)
  }

  test("glued lb stays on one line and a following line stays a break") {
    withSite(Map(
      "glued.xml" ->
        """<TEI xmlns="http://www.tei-c.org/ns/1.0"><l>бѣлорус-<lb/>ский</l></TEI>
          |""".stripMargin,
      "broken.xml" ->
        """<TEI xmlns="http://www.tei-c.org/ns/1.0"><l>бѣлорус-<lb/>
          |кого</l></TEI>
          |""".stripMargin
    )): (site, dir) =>
      site.prettyPrint()
      val glued: String = text(dir, "glued.xml")
      assert(glued.contains("бѣлорус-<lb/>ский"), glued)
      assert(!glued.contains("<lb></lb>"), glued)
      val broken: String = text(dir, "broken.xml")
      assert(broken.contains("<lb/>"), broken)
      assert(broken.contains("кого"), broken)
      assert(!broken.contains("ский"), broken)
  }

  test("non-xml, ignored paths, assets, unknown roots, and unsafe files stay byte for byte") {
    val html: String = "<input disabled>\n"
    val md: String = "Hello.\n"
    val project: String = "<project/>\n"
    val spaced: String = """<TEI xml:space="preserve"><p> a </p></TEI>""" + "\n"
    val subset: String = """<!DOCTYPE TEI [<!ENTITY e "x">]><TEI/>""" + "\n"
    val unclosed: String = "<TEI><p>no end\n"
    withSite(Map(
      "note.md" -> md,
      "about.html" -> html,
      "project.xml" -> project,
      "space.xml" -> spaced,
      "subset.xml" -> subset,
      "open.xml" -> unclosed,
      "kept.xml" ->
        """<TEI xmlns="http://www.tei-c.org/ns/1.0"><p>ok</p></TEI>
          |""".stripMargin,
      "kept.yml" -> "asset: true\n",
      "_site/generated.xml" -> "<generated/>\n",
      ".idea/misc.xml" -> "<project/>\n"
    )): (site, dir) =>
      val bad: NioPath = dir.resolve("bad.xml")
      NioFiles.write(bad, Array[Byte](0xff.toByte))
      val beforeBad: Array[Byte] = NioFiles.readAllBytes(bad)
      site.prettyPrint()
      assert(text(dir, "note.md") == md)
      assert(text(dir, "about.html") == html)
      assert(text(dir, "project.xml") == project)
      assert(text(dir, "space.xml") == spaced)
      assert(text(dir, "subset.xml") == subset)
      assert(text(dir, "open.xml") == unclosed)
      assert(text(dir, "kept.xml").contains("<TEI"), text(dir, "kept.xml"))
      assert(!text(dir, "kept.xml").contains("<?xml"), text(dir, "kept.xml"))
      assert(text(dir, "_site/generated.xml") == "<generated/>\n")
      assert(text(dir, ".idea/misc.xml") == "<project/>\n")
      assert(NioFiles.readAllBytes(bad).sameElements(beforeBad))
      assert(!new String(NioFiles.readAllBytes(bad), StandardCharsets.ISO_8859_1).contains("\uFFFD"))
  }

  test("DocBook keeps a YAML prefix and self-closes an empty xref") {
    val source: String =
      """---
        |title: Hello
        |---
        |<article>
        |<para>word<footnote>n</footnote></para>
        |<xref linkend="x"/>
        |</article>
        |""".stripMargin
    withSite(Map("book.xml" -> source)): (site, dir) =>
      site.prettyPrint()
      val printed: String = text(dir, "book.xml")
      assert(printed.startsWith("---\ntitle: Hello\n---\n"), printed)
      assert(printed.contains("""<xref linkend="x"/>"""), printed)
      assert(printed.contains("word<footnote>"), printed)
      assert(!printed.contains("word <footnote>"), printed)
  }
