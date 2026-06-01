package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlClass, Xml, XmlAttribute, XmlElement, XmlParser}

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

  private object Cols extends XmlAttribute("cols")
  private object Colspan extends XmlAttribute("colspan")
  private object Url extends XmlAttribute("url")
  private object Src extends XmlAttribute("src")

  private def withPrefix(name: String): String = s"tei-$name"

  private val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

  override protected def toHtml(element: Xml.Element): Xml.Element =
    val name: String = element.getName

    val result: Xml.Element = element.setAttributes(element.getAttributes.map((name, value) =>
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
        renameElement("a", result.setText(facsimileSymbol))

      // TODO tooltips on dates and gaps

      case _ => element

  private val facsimileSymbol: String = "⎙"

  override protected def linkKind(element: Xml.Element): Option[Link.Kind] =
    if element.has(HtmlClass("persName")) then Some(Link.Kind.Person) else
    if element.has(HtmlClass("placeName")) then Some(Link.Kind.Place) else
    if element.has(HtmlClass("orgName")) then Some(Link.Kind.Organization) else
      None

  private def renameElement(
    name: String,
    element: Xml.Element
  ): Xml.Element = element
    .add(HtmlClass(element.getName))
    .rename(name)

  private def copyAttribute(
    from: String,
    to: String,
    element: Xml.Element
  ): Xml.Element =
    element.get(XmlAttribute(from)).fold(element)(element.set(XmlAttribute(to), _))

  override protected def isSectionElement(element: Xml.Element): Boolean =
    element.getName == "div"

  override protected def sectionTitle(element: Xml.Element): Option[String] = element
    .getChildren
    .flatMap(_.asElement)
    .find(element => element.getName == "head")
    .flatMap(_.getTextOpt)

  override protected def sections(
    element: Xml.Element,
    errorReporter: PageError.Reporter
  ): Seq[Fragment.Section] = Seq.empty // TODO

  override protected def setFootnoteCorrelationIds(element: Xml.Element): Xml.Element =
    val correlationIds: IdGenerator = IdGenerator("")

    transform(element, element =>
      val isFootnote = element.getName == "note" && element.get(XmlAttribute("place")).contains("end")
      if !isFootnote then element else element
        .set(Footnotes.CorrelationId, correlationIds.generate())
        .add(Footnotes.LinkClass)
        .add(Footnotes.BodyClass)
    )

  override protected def isFootnotesContainer(element: Xml.Element): Boolean =
    element.getName == "text"

  override def parse(
    content: String,
    errorReporter: PageError.Reporter
  ): Xml.Element = XmlParser.parse(content) match
    case Right(xml) => xml.asElement.get
    case Left(error) =>
      errorReporter.error(PageError.Parsing, "TEI parsing error", Some(error))
      malformedTei(error)

  private def malformedTei(error: Throwable): Xml.Element = Xml
    .element(XmlElement("TEI"))
    .add(HtmlClass("malformed-xml"))
    .setText(s"Malformed TEI: $error")
