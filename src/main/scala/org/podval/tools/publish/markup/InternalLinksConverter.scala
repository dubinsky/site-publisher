package org.podval.tools.publish.markup

import org.podval.tools.publish.PageError
import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.Converter
import org.podval.xml.Xml
import java.net.{URI, URISyntaxException}

final class InternalLinksConverter extends Converter:
  override def stage: Converter.Stage = Converter.Stage.Links

  override protected def convert(
    element: Xml.Element,
    source: PageSource
  ): Option[Xml.Element] =
    if element.isA then None else
      element.getHref.flatMap: href =>
        // TODO verify that external link is not broken if the Site is so configured
        val isInternal: Boolean =
          try
            val uri: URI = URI(href)
            if source.site.isSelf(uri) then source.error(PageError.SelfLink, href)
            uri.getScheme == null
          catch case e: URISyntaxException => true

        Option.when(isInternal)(
          Links.markInternalLink(element)
        )
