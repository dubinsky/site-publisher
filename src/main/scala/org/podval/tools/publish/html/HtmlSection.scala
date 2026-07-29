package org.podval.tools.publish.html

import org.podval.xml.Xml

// Sections are represented by the HTML `h` elements and are not nested.
// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, AsciiDoc, and likely Re-Structured text;
// pure XML markup formats like TEI and DocBook are different.
// Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`;
// for AsciiDoc, by setting `sectids` attribute to `true` -
// but I do it manually and uniformly for HTML, Markdown, etc.
final class HtmlSection(
  val level: Int,
  val title: String,
  val id: String
)

object HtmlSection:
  def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = element.getName
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None
