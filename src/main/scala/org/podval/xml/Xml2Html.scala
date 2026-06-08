package org.podval.xml

object Xml2Html:
  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def fromXml(element: Xml.Element): Html.Element = Html
    .element(element.getName)
    .setAttributes(element.getAttributes)
    .setChildren(element.getChildren.flatMap: child =>
      // ZIO Blocks HTML does not support comments nor processing instructions
      child.asElement.map(fromXml)
        .orElse(child.asAtom.map(Html.text))
    )
