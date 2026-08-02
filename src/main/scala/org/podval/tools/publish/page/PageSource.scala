package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{Footnotes, Links, Markup, Section}
import org.podval.tools.publish.site.{PageError, PageErrorReporter, Path}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlDialect}
import zio.blocks.chunk.Chunk
import scala.ref.SoftReference

import java.net.{URI, URISyntaxException}
final class PageSource(
  val page: OriginalMarkupPage,
  val markup: Markup,
  val sourcePath: Path,
  frontMatterStandAlone: Option[Path]
) extends PageErrorReporter:
  def xmlDialect: XmlDialect = markup.xmlDialect

  override def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = page.site.error(
    sourcePath,
    kind,
    message,
    cause
  )

  private var contentVar: Option[SoftReference[PageContent]] = None

  def cache(frontMatter: FrontMatter, xml: Xml.Element): PageContent =
    val result: PageContent = process(frontMatter, xml)
    contentVar = Some(SoftReference(result))
    result

  def content: PageContent = contentVar match
    case None => readParseAndCache("Reading", firstReading = true)
    case Some(reference) => reference.get match
      case None => readParseAndCache("Re-reading evicted", firstReading = false)
      case Some(cached) => cached

  private def readParseAndCache(message: String, firstReading: Boolean): PageContent =
    val (frontMatter: FrontMatter, xml: Xml.Element) = markup.readAndParse(
      site = page.site,
      sourcePath = sourcePath,
      frontMatterStandAlone = frontMatterStandAlone,
      message = message,
      firstReading = firstReading
    )
    
    cache(frontMatter, xml)

  private def process(
    frontMatter: FrontMatter,
    xml: Xml.Element
  ): PageContent =
    // Run markup-specific processors and extract title
    val (xmlProcessed: Xml.Element, title: Option[Xml.Element]) = markup.process(this, xml)

    // TODO error if both front matter and content titles are present.

    // After everything that was to become a link had.
    val ids: IdGenerator = IdGenerator("_generated_id")
    val xmlLinksProcessed: Xml.Element = xmlDialect.transform(xmlProcessed, element =>
      var result: Xml.Element = element
      result = setAnchorId(result, ids).getOrElse(result)
      result = setSectionId(result, ids).getOrElse(result)
      result = convertInternalLink(result, this).getOrElse(result)
      result
    )

    // Process Footnotes

    // Retrieve footnote bodies // TODO shove them into PageContent
    val footnoteBodies: Map[String, Chunk[Xml.Node]] = Footnotes.footnoteBodies(xmlLinksProcessed, xmlDialect)
    val xmlFootnoteBodiesProcesses: Xml.Element = Footnotes.removeFootnoteBodies(xmlLinksProcessed, markup)
    val xmlResult: Xml.Element = Footnotes.transformFootnotes(xmlFootnoteBodiesProcesses, footnoteBodies, xmlDialect) // TODO unfold!

    PageContent(
      source = this,
      frontMatter = frontMatter,
      title = title,
      xml = xmlResult,
    )

  private def setAnchorId(element: Xml.Element, ids: IdGenerator): Option[Xml.Element] =
    Option.when(element.isA && element.getId.isEmpty)(
      element.setId(ids.generate())
    )

  private def setSectionId(element: Xml.Element, ids: IdGenerator): Option[Xml.Element] =
    Option.when(Section.is(element) && element.getId.isEmpty)(
      element.setId(ids.generate())
    )

  private def convertInternalLink(element: Xml.Element, source: PageSource): Option[Xml.Element] =
    if !element.isA then None else
      element.getHref.flatMap: href =>
        // TODO verify that external link is not broken if the Site is so configured
        val isInternal: Boolean =
          try
            val uri: URI = URI(href)
            if source.page.site.isSelf(uri) then source.error(PageError.SelfLink, href)
            uri.getScheme == null
          catch case e: URISyntaxException => true

        Option.when(isInternal)(
          element.add(Links.InternalLinkClass)
        )
