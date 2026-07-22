package org.podval.tools.publish

import org.podval.tei.EntityKind
import org.podval.tools.publish.asciidoc.{AsciiDocFootnotesConverter, AsciiDocMarkup}
import org.podval.tools.publish.markdown.{BlocksConverter, FlexMarkFootnotesConverter, KramdownTocHtmlConverter,
  MarkdownFootnotesConverter, MarkdownMarkup, WikiLinksProcessor}
import org.podval.tools.publish.markup.{AnchorIdsConverter, FootnotesTransformer, HtmlMarkup, HtmlSectionIdsConverter,
  InternalLinksProcessor, Markups}
import org.podval.tools.publish.processor.Processor
import org.podval.tools.publish.tei.{Tei2HtmlConverter, TeiEntityNamesConverter, TeiFacsimileLinksConverter,
  TeiFootnotesConverter, TeiMarkup, TeiSectionIdsConverter}

abstract class Configurer:
  def markups: Markups

  protected def commonProcessors: Seq[Processor] = Seq(
    new AnchorIdsConverter,
    new InternalLinksProcessor,
    new FootnotesTransformer
  )

  protected def htmlLikeProcessors: Seq[Processor] = Seq(
    new HtmlSectionIdsConverter
  )

  protected def markdownProcessors: Seq[Processor] = Seq(
    new BlocksConverter,
    new WikiLinksProcessor,
    new MarkdownFootnotesConverter,
    new FlexMarkFootnotesConverter,
    new KramdownTocHtmlConverter
  )

  protected def asciiDocProcessors: Seq[Processor] = Seq(
    new AsciiDocFootnotesConverter  
  )
  
  protected def teiProcessors: Seq[Processor] = Seq(
    new Tei2HtmlConverter,
    new TeiEntityNamesConverter,
    new TeiFacsimileLinksConverter,
    new TeiFootnotesConverter,
    new TeiSectionIdsConverter
  )

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
        processors = commonProcessors ++ htmlLikeProcessors ++ markdownProcessors
      )

      result.add(
        markupKind = AsciiDocMarkup,
        processors = commonProcessors ++ htmlLikeProcessors ++ asciiDocProcessors
      )
      
      result.add(
        markupKind = HtmlMarkup,
        processors = commonProcessors ++ htmlLikeProcessors
      )

      result.add(
        markupKind = TeiMarkup,
        processors = commonProcessors ++ teiProcessors,
        elements = Set("TEI", "store", "collection") ++ EntityKind.values.map(_.element).toSet,
      )

      result
