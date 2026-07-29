package org.podval.tools.publish

import org.podval.tei.EntityKind
import org.podval.tools.publish.asciidoc.AsciiDocMarkup
import org.podval.tools.publish.html.HtmlMarkup
import org.podval.tools.publish.markdown.MarkdownMarkup
import org.podval.tools.publish.markup.{MarkupKind, Markups}
import org.podval.tools.publish.tei.TeiMarkup

abstract class Configurer:
  def markups: Markups

object Configurer:
  def get(name: String): Configurer = Class
    .forName(if name.contains(".") then name else s"${Configurer.getClass.getName}$name")
    .getDeclaredConstructor()
    .newInstance()
    .asInstanceOf[Configurer]

  final class Default extends Configurer:
    override def markups: Markups =
      val result: Markups = new Markups

      result.add(
        markupKind = MarkdownMarkup,
        processors = MarkupKind.processors(processMarkdown = true, processAsciidoc = false) ++ HtmlMarkup.processors ++ MarkdownMarkup.processors
      )

      result.add(
        markupKind = AsciiDocMarkup,
        processors = MarkupKind.processors(processMarkdown = false, processAsciidoc = true) ++ HtmlMarkup.processors ++ AsciiDocMarkup.processors
      )
      
      result.add(
        markupKind = HtmlMarkup,
        processors = MarkupKind.processors(processMarkdown = false, processAsciidoc = false) ++ HtmlMarkup.processors
      )

      result.add(
        markupKind = TeiMarkup,
        processors = MarkupKind.processors(processMarkdown = false, processAsciidoc = false) ++ TeiMarkup.processors,
        elements = Set("TEI", "store", "collection") ++ EntityKind.values.map(_.element).toSet,
      )

      result
