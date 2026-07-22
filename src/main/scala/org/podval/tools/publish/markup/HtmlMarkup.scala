package org.podval.tools.publish.markup

object HtmlMarkup extends HtmlLikeMarkup(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false
) with XmlParsableMarkup