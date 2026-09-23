package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** TEI and DocBook conversion walks into `code`.
  * HTML passes keep the library default, which stops at `code`.
  */
private[markup] object DialectWalk:
  def transform(xml: Xml.Element)(f: Xml.Element => Xml.Element): Xml.Element =
    xml.transform(f, stopAtCode = false)

  def rewrite(
    xml: Xml.Element
  )(
    f: (Xml.Element, Option[Xml.Element]) => Xml.Rewrite
  ): Xml.Element = xml.rewrite(f, stopAtCode = false)

  def gather[A](xml: Xml.Element)(f: Xml.Element => Option[A]): Seq[A] =
    xml.gather(f, stopAtCode = false)

  def elements(xml: Xml.Element)(predicate: Xml.Element => Boolean): Seq[Xml.Element] =
    xml.elements(predicate, stopAtCode = false)
