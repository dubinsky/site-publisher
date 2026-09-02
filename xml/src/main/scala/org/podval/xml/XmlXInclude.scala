package org.podval.xml

import java.net.URL

/** Expand `xi:include` in a tree already parsed with XInclude off.
  *
  * Included document roots get `xml:base` relative to the initial document
  * URL, not to the including file. That is the XInclude result OpenTorah's
  * Xerces post-pass was approximating; doing it here avoids
  * [[https://issues.apache.org/jira/browse/XERCESJ-1102 XERCESJ-1102]].
  */
// This is mind-bogglingly weird, but:
// - Xerces has a bug in the handling of XIncludes;
// - starting at the third level of nested includes, the values of the xml:base attributes are wrong;
// - the bug https://issues.apache.org/jira/browse/XERCESJ-1102 was reported in October 2005!!!
// - a patch that allegedly fixes the issue is known for years
// - a comment from the Xerces maintainer says:
//   What Xerces needs most is new contributors / committers who can volunteer their time and help review these patches and get them committed.
//   We also need a new release. It's been 5 years. Long overdue.
//   If you or anyone else is interested in getting involved we'd be happy to have you join the project.
// - latest release of Xerces was in 2023, with the bug still there
// - many projects depend on Xerces, including Saxon, where the bug was also discussed: https://saxonica.plan.io/issues/4664
// - allegedly, the bug is fixed in "SaxonC 11.1" - although how can this be with Saxon not shipping its own Xerces is not clear.
//
// So, I need to process XIncludes myself instead of relying on the industry-standard Xerces!
// What a nightmare...
object XmlXInclude:
  val NamespaceUri: String = "http://www.w3.org/2001/XInclude"

  def isInclude(element: Xml.Element): Boolean =
    element.localName == "include" &&
      (element.name.namespace.contains(NamespaceUri) || element.name.prefix.contains("xi"))

  def expand(root: Xml.Element, documentUrl: URL): Either[Throwable, Xml.Element] =
    expand(root, documentUrl, documentUrl, Nil)

  private def expand(
    element: Xml.Element,
    documentUrl: URL,
    initialUrl: URL,
    stack: List[String]
  ): Either[Throwable, Xml.Element] =
    if isInclude(element) then include(element, documentUrl, initialUrl, stack)
    else
      element.getChildren.foldLeft(Right(Seq.empty): Either[Throwable, Xml.Nodes]): (acc, node) =>
        for
          nodes <- acc
          more <- node.asElement match
            case None => Right(Seq(node))
            case Some(child) if isInclude(child) =>
              include(child, documentUrl, initialUrl, stack).map(Seq(_))
            case Some(child) =>
              expand(child, documentUrl, initialUrl, stack).map(Seq(_))
        yield nodes ++ more
      .map(element.setChildren)

  private def include(
    element: Xml.Element,
    documentUrl: URL,
    initialUrl: URL,
    stack: List[String]
  ): Either[Throwable, Xml.Element] =
    element.get(XmlAttribute.Href).map(_.trim).filter(_.nonEmpty) match
      case None => Left(XmlError("XInclude missing href"))
      case Some(href) =>
        val includedUrl: URL = documentUrl.toURI.resolve(href).normalize.toURL
        val key: String = includedUrl.toURI.normalize.toString
        if stack.contains(key) then Left(XmlError(s"XInclude cycle: $href"))
        else
          XmlParser.parseXml(includedUrl, xinclude = false).flatMap: included =>
            expand(included, includedUrl, initialUrl, key :: stack)
          .map(_.set(XmlAttribute.XmlBase, xmlBase(initialUrl, includedUrl)))

  private def xmlBase(initial: URL, included: URL): String =
    val from: String = resourcePath(initial)
    val to: String = resourcePath(included)
    val slash: Int = from.lastIndexOf('/')
    val dir: String = if slash < 0 then "" else from.substring(0, slash + 1)
    if dir.nonEmpty && to.startsWith(dir) then to.substring(dir.length) else to

  private def resourcePath(url: URL): String =
    val s: String = url.toString
    val bang: Int = s.lastIndexOf('!')
    val path: String = if bang >= 0 then s.substring(bang + 1) else Option(url.getPath).getOrElse(s)
    if path.startsWith("/") then path else s"/$path"
