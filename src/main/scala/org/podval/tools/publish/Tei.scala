package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlAst, XmlParser, XmlWriter}

/*
I tried to define CSS namespaces like this:
@namespace tei   url("http://www.tei-c.org/ns/1.0");
@namespace db    url("http://docbook.org/ns/docbook");
@namespace xhtml url("http://www.w3.org/1999/xhtml");
and use them in CSS rules like this: tei|div, docbook|title.

It seems that in browser DOM all elements are in the HTML5 xhtml namespace
unless xmlns attribute is present on that element;
why are the namespace declarations not inherited is not clear.

So, I prefix the names of the elements from non-HTML namespaces with the namespace prefix
if their names clash with the HTML namespace in a way that makes CSS styling difficult.
For instance, I use <div> to structure the layout, but need to be able to style TEI
depending on the level of nesting of TEI divs.
Also, HTML disallows tables within paragraphs, so to have a tooltip inside a TEI paragraph,
it needs to not be an HTML <p> (and of course, namespace is ignored...)
*/
object Tei extends Markup:
  override val extension: String = "xml"
  override val additionalExtensions: Set[String] = Set.empty
  override protected def recognizeMarkdownWikiLinks: Boolean = false
  override protected def recognizeMarkdownFootnotes: Boolean = false
  override protected def recognizeMarkdownBlocks: Boolean = false
  override protected def stop(xml: XmlAst)(element: xml.Element): Boolean = false

  private object Cols extends Xml.Attribute("cols")
  private object Colspan extends Xml.Attribute("colspan")
  private object Url extends Xml.Attribute("url")
  private object Src extends Xml.Attribute("src")

  private def withPrefix(name: String): String = s"tei-$name"

  private val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

  override protected def toHtml(element: Xml.Element): Xml.Element =
    val name: String = Xml.name(element)

    val result: Xml.Element = Xml.setAttributes(element, Xml.attributes(element).map((name, value) =>
      val nameNew =
        if !reservedAttributes.contains(name)
        then name
        else withPrefix(name)
      (nameNew, value)
    ))

    name match
      case "head" | "body" | "title" | "div" | "p" =>
        renameElement(withPrefix(name), result)

      case "row" =>
        renameElement("tr", result)

      case "cell" =>
        renameElement("td", copyAttribute("cols", "colspan", result))

      case "graphic" =>
        renameElement("image", copyAttribute("url", "src", result))

      case "ref" | "ptr" =>
        renameElement("a", copyAttribute("target", "href", result))

      case "persName" | "placeName" | "orgName" =>
        renameElement("a", copyAttribute("ref", "href", result))

      case "pb" =>
        renameElement("a", Xml.setText(result, facsimileSymbol))

      // TODO tooltips on dates and gaps

      case _ => element

  private val facsimileSymbol: String = "⎙"

  override protected def linkKind(element: Xml.Element): Option[Link.Kind] =
    if Xml.ClassName.has(element, "persName") then Some(Link.Kind.Person) else
    if Xml.ClassName.has(element, "placeName") then Some(Link.Kind.Place) else
    if Xml.ClassName.has(element, "orgName") then Some(Link.Kind.Organization) else
      None

  private def renameElement(
    name: String,
    element: Xml.Element
  ): Xml.Element =
    Xml.rename(Xml.ClassName.add(element, Xml.name(element)), name)

  private def copyAttribute(
    from: String,
    to: String,
    element: Xml.Element
  ): Xml.Element =
    Xml.getAttribute(element, from).fold(element)(Xml.setAttribute(element, to, _))

  override protected def isSectionElement(element: Xml.Element): Boolean =
    Xml.name(element) == "div"

  override protected def sectionTitle(element: Xml.Element): Option[String] = Xml
    .children(element)
    .flatMap(Xml.asElement)
    .find(element => Xml.name(element) == "head")
    .flatMap(Xml.toStringOpt)

  override protected def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty // TODO

  override protected def setFootnoteCorrelationIds(element: Xml.Element): Xml.Element =
    val correlationIds: IdGenerator = IdGenerator("")

    Xml.transform(element, stop(Xml), element =>
      var result: Xml.Element = element
      val isFootnote = Xml.name(element) == "note" && Xml.getAttribute(element, "place").contains("end")
      if isFootnote then
        result = Footnotes.CorrelationId.set(element, correlationIds.generate())
        result = Footnotes.LinkClass.add(result)
        result = Footnotes.BodyClass.add(result)
      result
    )

  override protected def isFootnotesContainer(element: Xml.Element): Boolean =
    Xml.name(element) == "text"

  override def parse(
    content: String,
    errorReporter: PageError.Reporter
  ): Xml.Element = XmlParser.parse(content) match
    case Right(xml) => Xml.asElement(xml).get
    case Left(error) =>
      errorReporter.error(PageError.Parsing, "TEI parsing error", Some(error))
      malformedTei(error)

  private def malformedTei(error: Throwable): Xml.Element =
    var result = Xml.element("TEI")
    result = Xml.ClassName.add(result, "malformed-xml")
    result = Xml.setText(result, s"Malformed TEI: $error")
    result

  def teiWriterConfig: XmlWriter.Config = new XmlWriter.Config:
    override def selfClose(name: String): Boolean = false
    override def stack(name: String): Boolean = false
    override def unStack(name: String): Boolean = name == "choice"
    override def nest(name: String): Boolean = Set("p", /*"abstract",*/ "head", "salute", "dateline").contains(name)
    override def cling(name: String): Boolean = Set("note", "lb", "sic", "corr").contains(name)
    override def break(name: String): Boolean = false // TODO TEI: lb; HTML: br?!
    override def preformat(name: String): Boolean = false
