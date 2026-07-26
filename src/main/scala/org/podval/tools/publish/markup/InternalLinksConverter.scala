package org.podval.tools.publish.markup

import org.podval.tools.publish.PageError
import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Converter
import org.podval.xml.Xml
import java.net.{URI, URISyntaxException}

final class InternalLinksConverter extends Converter(convertLinks = true):
  override protected def convert(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element =
    if !element.isA then element else
      element.getHref.fold(element): href =>
        // TODO verify that external link is not broken if the Site is so configured
        val isInternal: Boolean =
          try
            val uri: URI = URI(href)
            if content.site.isSelf(uri) then content.error(PageError.SelfLink, href)
            uri.getScheme == null
          catch case e: URISyntaxException => true

        if !isInternal
        then element
        else Links.markInternalLink(element)
