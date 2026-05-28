package org.podval.xml

import org.podval.xml.XmlAst
import org.typelevel.paiges.Doc
import zio.blocks.chunk.Chunk

final class XmlWriter[X <: XmlAst](val xml: X)(
  val config: XmlWriter.Config,
  val width: Int = 120,
  val indent: Int = 2
):
  def render(element: xml.Element): String =
    fromElement(
      element,
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    )
      .render(width)
      .replace(XmlWriter.hiddenNewline, "\n")
      .appended('\n')

  private def fromElement(
    element: xml.Element,
    parent: Option[xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    val attributeValues: Chunk[(String, String)] = xml.attributes(element, parent)
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map((name, value) =>
        Doc.text(s"$name=") + Doc.lineOrEmpty + Doc.text(Strings.quote(encodeXmlSpecials(value)))
      ))

    val nodes: List[xml.Xml] =
      atomize(List.empty, xml.children(element).toList)
//      xml.children(element).toList

    val chunks: Seq[Seq[xml.Xml]] = chunkify(Seq.empty, List.empty, nodes, flush = false)
    val noText: Boolean = chunks.forall(_.forall(node => xml.asAtom(node).isEmpty))
    val whitespaceLeft: Boolean = nodes.headOption.exists(isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(isCharacters)
    val charactersRight: Boolean = nodes.lastOption.exists(isCharacters)
    
    val children: Seq[Doc] = if chunks.isEmpty then Seq.empty else
      val canBreakLeft1 = canBreakLeft || whitespaceLeft
      val canBreakRight1 = canBreakRight || whitespaceRight

       if chunks.length == 1 then Seq(
        fromChunk(chunks.head, Some(element), canBreakLeft1, canBreakRight1)
      ) else
        fromChunk(chunks.head, Some(element), canBreakLeft = canBreakLeft1, canBreakRight = true) +:
        chunks.tail.init.map(chunk => fromChunk(chunk, Some(element), canBreakLeft = true, canBreakRight = true)) :+
        fromChunk(chunks.last, Some(element), canBreakLeft = true, canBreakRight = canBreakRight1)

    val name: String = xml.name(element)

    if children.isEmpty then
      Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + (
        if config.selfClose(name)
        then Doc.text("/>")
        else Doc.text(s"></$name>")
      )
    else
      val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$name>")

      val stack: Boolean =
        noText &&
        !config.unStack(name) &&
        ((children.length >= 2) || ((children.length == 1) && config.stack(name)))

      if stack then
        // If this is clearly a bunch of elements - stack 'em with an indent:
        Doc.cat(Seq(
          start,
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(indent))),
          Doc.hardLine,
          end
        ))
      else if config.nest(name) then
        // If this is forced-nested element - nest it:
        Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, indent)
      else
        // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
        // character content should stick to the opening and closing tags:
        Doc.cat(Seq(
          start,
          if canBreakLeft && !charactersLeft then Doc.lineOrEmpty else Doc.empty,
          Doc.intercalate(Doc.lineOrSpace, children),
          if canBreakRight && !charactersRight then Doc.lineOrEmpty else Doc.empty,
          end
        ))
  
  @scala.annotation.tailrec
  private def atomize(result: List[xml.Xml], nodes: List[xml.Xml]): List[xml.Xml] = if nodes.isEmpty then result else
    val (atoms: List[xml.Xml], tail: List[xml.Xml]) = nodes.span(node => xml.asAtom(node).isDefined)

    val resultNew: List[xml.Xml] =
      if atoms.isEmpty
      then result
      else result ++ processText(Seq.empty, squashBigWhitespace(atoms.map(atom => xml.asAtom(atom).get).mkString("")))

    tail match 
      case Nil => resultNew
      case n :: ns => atomize(resultNew :+ n, ns)

  private def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  @scala.annotation.tailrec
  private def processText(result: Seq[xml.Xml], text: String): Seq[xml.Xml] = if text.isEmpty then result else
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val resultNew: Seq[xml.Xml] = if spaces.isEmpty then result else result :+ space
    val (word: String, tail2: String) = tail.span(_ != ' ')
    if word.isEmpty
    then resultNew
    else processText(resultNew :+ xml.mkText(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(
    result: Seq[Seq[xml.Xml]],
    current: List[xml.Xml],
    nodes: List[xml.Xml],
    flush: Boolean
  ): Seq[Seq[xml.Xml]] =
    if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else
      if nodes.isEmpty then
        if current.isEmpty
        then result
        else chunkify(result, current, nodes, flush = true)
      else
        val node = nodes.head
        val tail = nodes.tail
        if isWhitespace(node)
        then chunkify(result, current, tail, flush = current.nonEmpty)
        else
          if current.isEmpty
          then chunkify(result, node :: current, tail, flush = false)
          else
            val c = current.head
            if isWhitespace(c)
            then chunkify(result, current, nodes, flush = true)
            else
              val cling: Boolean =
                xml.asElement(c).isEmpty ||
                xml.asElement(c).nonEmpty && xml.asElement(node).isEmpty && !isWhitespace(node) ||
                xml.asElement(node).isDefined && config.cling(xml.name(xml.asElement(node).get))
              if cling
              then chunkify(result, node :: current, tail, flush = false)
              else chunkify(result, current, nodes, flush = true)

  private def fromChunk(
    nodes: Seq[xml.Xml],
    parent: Option[xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    require(nodes.nonEmpty)
    if nodes.length == 1 then
      fromNode(nodes.head, parent, canBreakLeft, canBreakRight)
    else Doc.cat(
      fromNode(nodes.head, parent, canBreakLeft, canBreakRight = false) +:
      nodes.tail.init.map(node => fromNode(node, parent, canBreakLeft = false, canBreakRight = false)) :+
      fromNode(nodes.last, parent, canBreakLeft = false, canBreakRight)
    )
  
  private def fromNode(
    node: xml.Xml,
    parent: Option[xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    xml.asElement(node).map: (element: xml.Element) =>
      val name: String = xml.name(element)
      if config.preformat(name) then
        Doc.text(preformatElement(element, parent).mkString(XmlWriter.hiddenNewline))
      else
        val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
        if canBreakRight && config.break(name) then result + Doc.hardLine else result
    .orElse(xml.asAtom(node).map(text => Doc.text(encodeXmlSpecials(text))))
    .getOrElse(Doc.paragraph(toString(node)))

  private def preformatElement(element: xml.Element, parent: Option[xml.Element]): Seq[String] =
    val attributeValues: Chunk[(String, String)] = xml.attributes(element, parent)
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map((name, value) => s"$name=${Strings.quote(value)}") // TODO escapeSpecials?
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      xml.children(element).flatMap(node => preformat(node, Some(element)))

    val name: String = xml.name(element)
    if children.isEmpty then Seq(s"<$name$attributes/>")
    else if children.length == 1 then Seq(s"<$name$attributes>${children.head}</$name>")
    else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")

  private def preformat(node: xml.Xml, parent: Option[xml.Element]): Seq[String] =
    xml.asElement(node).map(preformatElement(_, parent))
    .orElse(xml.asAtom(node).map(preformat))
    .getOrElse(preformat(toString(node)))

  private def preformat(string: String): Seq[String] =
    Strings.encodeXmlSpecials(string)
    /* encodeXmlSpecials(string) */.split("\n").toSeq
  
  private def encodeXmlSpecials(string: String): String =
    if config.encodeXmlSpecials then Strings.encodeXmlSpecials(string) else string

  private def space: xml.Xml = xml.mkText(" ")

  private def toString(node: xml.Xml): String = xml.toString(node)

  private def isCharacters(node: xml.Xml): Boolean = xml.asAtom(node).fold(false)(_.trim.nonEmpty)
  private def isWhitespace(node: xml.Xml): Boolean = xml.asAtom(node).fold(false)(_.trim.isEmpty)

object XmlWriter:
  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  private val hiddenNewline: String = "\\n"

  abstract class Config:
    def encodeXmlSpecials: Boolean = false // TODO do not double-encode what you did not decode ;)

    //  if allowEmptyElements || keepEmptyElements.contains(name.localName)
    //  Some elements are mis-processed when they are empty, e.g. <script .../> ...
    //  ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    //  and in general, it's weird to expand the elements that are always empty...
    def selfClose(name: String): Boolean
    def stack(name: String): Boolean
    def unStack(name: String): Boolean
    def nest(name: String): Boolean
    def cling(name: String): Boolean
    def break(name: String): Boolean
    def preformat(name: String): Boolean

  def htmlWriterConfig: Config = new Config:
    override def selfClose(name: String): Boolean = Set("br", "hr", "meta", "link", "img", "input").contains(name)
    override def stack(name: String): Boolean = Set("nav", "header", "main", "div").contains(name)
    override def unStack(name: String): Boolean = false
    override def nest(name: String): Boolean = false
    override def cling(name: String): Boolean = false //Set("span").contains(name)
    override def break(name: String): Boolean = false // TODO TEI: lb; HTML: br?!
    override def preformat(name: String): Boolean = name == "pre"
