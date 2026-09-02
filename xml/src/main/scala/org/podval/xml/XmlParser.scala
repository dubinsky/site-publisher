package org.podval.xml

import zio.blocks.schema.xml.Xml
import org.xml.sax.InputSource
import scala.util.Using
import java.io.File
import java.net.URL

/** Load XML (and HTML) into the ZIO Blocks XML AST.
  *
  * XInclude is off by default: `xi:include/@href` stays in the tree. Publisher
  * `store`/`collection` indexes treat it as a child page, not an inlined
  * document. Pass `xinclude = true` on URL, file, or classpath load to expand
  * includes. `xml:base` on included roots is relative to the *initial*
  * document, so nested includes do not hit
  * [[https://issues.apache.org/jira/browse/XERCESJ-1102 XERCESJ-1102]].
  * String parse never expands (there is no base URL). HTML uses TagSoup
  * (`parseHtml`) from a string, URL, or file; it does not expand XInclude.
  */
object XmlParser:
  def parse(content: String, isXml: Boolean): Either[Throwable, Xml.Element] =
    if isXml then parseXml(content) else parseHtml(content)

  def parseXml(content: String): Either[Throwable, Xml.Element] =
    XmlParserStAX.parse(content)

  def parseXml(file: File): Either[Throwable, Xml.Element] =
    parseXml(file, xinclude = false)

  def parseXml(file: File, xinclude: Boolean): Either[Throwable, Xml.Element] =
    parseXml(file.toURI.toURL, xinclude)

  def parseXml(url: URL): Either[Throwable, Xml.Element] =
    parseXml(url, xinclude = false)

  def parseXml(url: URL, xinclude: Boolean): Either[Throwable, Xml.Element] =
    val loaded: Either[Throwable, Xml.Element] =
      Using(url.openStream())(XmlParserStAX.parse).fold(Left(_), identity)
    if xinclude then loaded.flatMap(XmlXInclude.expand(_, url)) else loaded

  /** Classpath resource; `name` is `Class.getResource` style (`/org/.../Foo.xml`
    * is from the classpath root). */
  def parseResource(name: String): Either[Throwable, Xml.Element] =
    parseResource(name, xinclude = false)

  def parseResource(name: String, xinclude: Boolean): Either[Throwable, Xml.Element] =
    val absolute: String = if name.startsWith("/") then name else s"/$name"
    parseResource(XmlParser.getClass, absolute, xinclude)

  def parseResource(loader: Class[?], name: String): Either[Throwable, Xml.Element] =
    parseResource(loader, name, xinclude = false)

  def parseResource(
    loader: Class[?],
    name: String,
    xinclude: Boolean
  ): Either[Throwable, Xml.Element] =
    Option(loader.getResource(name)) match
      case None => Left(XmlError(s"Resource not found: $name"))
      case Some(url) => parseXml(url, xinclude)

  /** Parse a catalog resource: wrapper `name`, each child decoded with `codec`. */
  def parseCatalog[A](resource: String, name: String, codec: XmlCodec[A]): Either[Throwable, Seq[A]] =
    parseResource(resource).flatMap: root =>
      codec.decodeCatalog(root, name).left.map(e => e: Throwable)

  def parseCatalog[A](
    loader: Class[?],
    resource: String,
    name: String,
    codec: XmlCodec[A]
  ): Either[Throwable, Seq[A]] =
    parseResource(loader, resource).flatMap: root =>
      codec.decodeCatalog(root, name).left.map(e => e: Throwable)

  def parseHtml(content: String): Either[Throwable, Xml.Element] =
    XmlParserSax.parse(reader = HtmlTagSoup.reader, content = content)

  def parseHtml(file: File): Either[Throwable, Xml.Element] =
    parseHtml(file.toURI.toURL)

  def parseHtml(url: URL): Either[Throwable, Xml.Element] =
    Using(url.openStream()): stream =>
      val source: InputSource = InputSource(stream)
      source.setSystemId(url.toString)
      XmlParserSax.parse(reader = HtmlTagSoup.reader, source = source)
    .fold(Left(_), identity)
