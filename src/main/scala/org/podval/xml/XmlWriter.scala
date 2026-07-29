package org.podval.xml

import org.typelevel.paiges.Doc
import zio.blocks.chunk.Chunk

object XmlWriter:
  private val indent: Int = 2

  val widthDefault: Int = 120

  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  private val hiddenNewline: String = "\\n"

  def render[Element: XmlAst](dialect: XmlDialect, element: Element, width: Int): String =
    fromElement(
      element,
      canBreakLeft = true,
      canBreakRight = true
    )(using dialect)
      .render(width)
      .replace(XmlWriter.hiddenNewline, "\n")
      .appended('\n')

  private def fromElement[Element](
    element: Element,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  )(using dialect: XmlDialect)(using ast: XmlAst[Element]): Doc =
    val attributeValues: Chunk[(String, String)] = element.getAttributes
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map((name, value) =>
        Doc.text(s"$name=") + Doc.lineOrEmpty + Doc.text(XmlEncode.quote(encodeXmlSpecials(value)))
      ))

    val nodes: List[ast.Node] =
      atomize(List.empty, element.getChildren.toList)
//      xml.children(element).toList

    val chunks: Seq[Seq[ast.Node]] = chunkify(Seq.empty, List.empty, nodes, flush = false)
    val noText: Boolean = chunks.forall(_.forall(_.asAtom.isEmpty))
    val whitespaceLeft: Boolean = nodes.headOption.exists(isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(isCharacters)
    val charactersRight: Boolean = nodes.lastOption.exists(isCharacters)
    
    val children: Seq[Doc] = if chunks.isEmpty then Seq.empty else
      val canBreakLeft1 = canBreakLeft || whitespaceLeft
      val canBreakRight1 = canBreakRight || whitespaceRight

       if chunks.length == 1 then Seq(
        fromChunk(chunks.head, canBreakLeft1, canBreakRight1)
      ) else
        fromChunk(chunks.head, canBreakLeft = canBreakLeft1, canBreakRight = true) +:
        chunks.tail.init.map(chunk => fromChunk(chunk, canBreakLeft = true, canBreakRight = true)) :+
        fromChunk(chunks.last, canBreakLeft = true, canBreakRight = canBreakRight1)

    val name: String = element.getName

    if children.isEmpty then
      Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + (
        if dialect.selfClose.contains(name)
        then Doc.text("/>")
        else Doc.text(s"></$name>")
      )
    else
      val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$name>")

      val stack: Boolean =
        noText &&
        !dialect.unStack.contains(name) &&
        ((children.length >= 2) || ((children.length == 1) && dialect.stack.contains(name)))

      if stack then
        // If this is clearly a bunch of elements - stack 'em with an indent:
        Doc.cat(Seq(
          start,
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(XmlWriter.indent))),
          Doc.hardLine,
          end
        ))
      else if dialect.nest.contains(name) then
        // If this is forced-nested element - nest it:
        Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, XmlWriter.indent)
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
  private def atomize(
    using ast: XmlAst[?]
  )(
    result: List[ast.Node],
    nodes: List[ast.Node]
  ): List[ast.Node] = if nodes.isEmpty then result else
    val (atoms: List[ast.Node], tail: List[ast.Node]) = nodes.span(_.asAtom.isDefined)

    val resultNew: List[ast.Node] =
      if atoms.isEmpty
      then result
      else result ++ processText(Seq.empty, squashBigWhitespace(atoms.map(_.asAtom.get).mkString("")))

    tail match 
      case Nil => resultNew
      case n :: ns => atomize(resultNew :+ n, ns)

  private def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  @scala.annotation.tailrec
  private def processText(
    using ast: XmlAst[?]
  )(
    result: Seq[ast.Node],
    text: String
  ): Seq[ast.Node] = if text.isEmpty then result else
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val resultNew: Seq[ast.Node] = if spaces.isEmpty then result else result :+ space
    val (word: String, tail2: String) = tail.span(_ != ' ')
    if word.isEmpty
    then resultNew
    else processText(resultNew :+ ast.text(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(using dialect: XmlDialect, ast: XmlAst[?])(
    result: Seq[Seq[ast.Node]],
    current: List[ast.Node],
    nodes: List[ast.Node],
    flush: Boolean
  ): Seq[Seq[ast.Node]] =
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
                c.asElement.isEmpty ||
                c.asElement.nonEmpty && node.asElement.isEmpty && !isWhitespace(node) ||
                node.asElement.isDefined && dialect.cling.contains(node.asElement.get.getName)
              if cling
              then chunkify(result, node :: current, tail, flush = false)
              else chunkify(result, current, nodes, flush = true)

  private def fromChunk(using dialect: XmlDialect, ast: XmlAst[?])(
    nodes: Seq[ast.Node],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    require(nodes.nonEmpty)
    if nodes.length == 1 then
      fromNode(nodes.head, canBreakLeft, canBreakRight)
    else Doc.cat(
      fromNode(nodes.head, canBreakLeft, canBreakRight = false) +:
      nodes.tail.init.map(node => fromNode(node, canBreakLeft = false, canBreakRight = false)) :+
      fromNode(nodes.last, canBreakLeft = false, canBreakRight)
    )
  
  private def fromNode(using dialect: XmlDialect, ast: XmlAst[?])(
    node: ast.Node,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    node.asElement.map: (element: ast.Element) =>
      val name: String = element.getName
      if dialect.preformat.contains(name) then
        Doc.text(preformatElement(element).mkString(XmlWriter.hiddenNewline))
      else
        val result: Doc = fromElement(element, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
        if canBreakRight && dialect.break.contains(name) then result + Doc.hardLine else result
    .orElse(node.asAtom.map(text => Doc.text(encodeXmlSpecials(text))))
    .getOrElse(Doc.paragraph(toString(node)))

  private def preformatElement[Element: XmlAst](element: Element): Seq[String] =
    val attributeValues: Chunk[(String, String)] = element.getAttributes
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map((name, value) => s"$name=${XmlEncode.quote(value)}") // TODO escapeSpecials?
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      element.getChildren.flatMap(preformat)

    val name: String = element.getName
    if children.isEmpty then Seq(s"<$name$attributes/>")
    else if children.length == 1 then Seq(s"<$name$attributes>${children.head}</$name>")
    else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")

  private def preformat(using ast: XmlAst[?])(node: ast.Node): Seq[String] = node
    .asElement
    .map(preformatElement)
    .orElse(node.asAtom.map(preformat))
    .getOrElse(preformat(toString(node)))

  private def preformat(string: String): Seq[String] =
    XmlEncode.encodeXmlSpecials(string).split("\n").toSeq

  // TODO from Grok:
  //- Description: For HTML dialect, `encodeXmlSpecials` is disabled (`HtmlXmlDialect` default).
  // Titles, tags, authors, and other front-matter strings rendered via the HTML DSL are written without escaping text nodes.
  // Attribute quoting (`Strings.quote`) also does not escape `"`, so a title containing `"` can break attributes.
  // For untrusted or multi-author content this is XSS/HTML injection risk; even for trusted content it can corrupt markup.
  //- Suggestion: Escape text and attributes on render (use full `Strings.escape` for attributes).
  // Prefer encoding on output always; only skip for preformatted trusted raw HTML islands if needed.
  private def encodeXmlSpecials(using dialect: XmlDialect)(string: String): String =
    if dialect.encodeXmlSpecials then XmlEncode.encodeXmlSpecials(string) else string

  private def space(using ast: XmlAst[?]): ast.Node = ast.text(" ")

  private def toString(using ast: XmlAst[?])(node: ast.Node): String = node.getText

  private def isCharacters(using xml: XmlAst[?])(node: xml.Node): Boolean = node.asAtom.fold(false)(_.trim.nonEmpty)
  private def isWhitespace(using xml: XmlAst[?])(node: xml.Node): Boolean = node.asAtom.fold(false)(_.trim.isEmpty)
