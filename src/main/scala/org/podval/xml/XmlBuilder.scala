package org.podval.xml

import zio.blocks.schema.xml.Xml
import scala.collection.mutable

// TODO abstract this and XmlParser over XmlAst
final class XmlBuilder:
  private val elements: mutable.Stack[Xml.Element] = mutable.Stack.empty

  private var root: Option[Xml.Element] = None

  // TODO ignore events after done
  def done: Boolean = root.nonEmpty
  
  def result: Xml.Element = root.get
  
  def startElement(element: Xml.Element): Unit =
    flushCharacters()
    elements.push(element)

  def endElement(): Unit =
    flushCharacters()
    val element: Xml.Element = elements.pop()
    if elements.nonEmpty
    then addChild(element)
    else root = Some(element)
    
  private def addChild(child: Xml): Unit =
    val parent: Xml.Element = elements.pop()
    elements.push(parent.copy(children = parent.children :+ child))

  def processingInstruction(target: String, data: String): Unit =
    flushCharacters()
    addChild(Xml.ProcessingInstruction(target = target, data = data))

  def comment(text: String): Unit =
    flushCharacters()
    addChild(Xml.Comment(text))
    
  private val characters: mutable.StringBuilder = mutable.StringBuilder()
  private var isCData: Boolean = false

  def addCharacters(more: String): Unit = characters.addAll(more)

  def setCData(): Unit = isCData = true
  
  def flushCharacters(): Unit = if characters.nonEmpty then
    val text: String = characters.toString
    characters.clear()
    addChild(if isCData then Xml.CData(text) else Xml.Text(text))
    isCData = false
    
