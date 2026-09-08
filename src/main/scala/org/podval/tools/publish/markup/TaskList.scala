package org.podval.tools.publish.markup

import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

/** Markup-neutral task-list IR. CSS styles only these classes. */
object TaskList:
  object ListClass extends CssClass("task-list")
  object ItemClass extends CssClass("task-list-item")
  object CheckboxClass extends CssClass("task-list-item-checkbox")

  def isList(element: Xml.Element): Boolean =
    (element.isElement(XmlElement.Ul) || element.isElement(XmlElement.Ol)) && element.has(ListClass)

  def isItem(element: Xml.Element): Boolean =
    element.isElement(XmlElement.Li) && element.has(ItemClass)

  def isCheckbox(element: Xml.Element): Boolean =
    element.qName == "input" && (
      element.get(XmlAttribute.Type).contains("checkbox") || element.has(CheckboxClass)
    )

  def checkbox(done: Boolean): Xml.Element =
    val box: Xml.Element = Xml.element("input").set(XmlAttribute.Type, "checkbox")
    normalizeCheckbox(if done then box.set("checked", "checked") else box)

  def asItem(li: Xml.Element, done: Boolean, rest: Xml.Nodes): Xml.Element =
    asItem(li, checkbox(done), rest)

  def asItem(li: Xml.Element, box: Xml.Element, rest: Xml.Nodes): Xml.Element =
    li.add(ItemClass).setChildren(normalizeCheckbox(box) +: rest)

  def asList(list: Xml.Element): Xml.Element =
    val hasTask: Boolean = list.getChildren.flatMap(_.asElement).exists(_.has(ItemClass))
    if !hasTask then list else list.add(ListClass)

  private def normalizeCheckbox(element: Xml.Element): Xml.Element =
    val done: Boolean = element.get("checked").isDefined
    var result: Xml.Element = element
      .set(XmlAttribute.Type, "checkbox")
      .add(CheckboxClass)
      .set("disabled", "disabled")
      .set("readonly", "")
    if done then result.set("checked", "checked")
    else result.set("checked", "")
