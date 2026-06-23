package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.xml.{Html, XmlDialect}

// TODO eliminate
object XmlMarkup extends XmlLikeMarkup(XmlDialect.Plain, Seq.empty):
  override def pageHeader(content: PageContent): Html.Element = Markup.pageHeader(content)

  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty
