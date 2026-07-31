package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{AnchorIdsConverter, Footnotes, FootnotesTransformer,
  InternalLinksConverter, Markup, Processor}
import org.podval.tools.publish.site.{PageError, Path}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlDialect}
import zio.blocks.chunk.Chunk
import scala.ref.SoftReference

final class PageSource(
  val page: OriginalMarkupPage,
  val markup: Markup,
  val sourcePath: Path,
  frontMatterStandAlone: Option[Path]
):
  def xmlDialect: XmlDialect = markup.xmlDialect

  def error(
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
    val ids: IdGenerator = IdGenerator()
    val (xmlProcessed: Xml.Element, title: Option[Xml.Element]) = markup.process(this, ids, xml)

    // TODO error if both front matter and content titles are present.

    // After everything that was to become a link had.
    val xmlLinksProcessed: Xml.Element = Processor.process(xmlDialect, xmlProcessed, Seq(
      AnchorIdsConverter(ids),
      InternalLinksConverter(this)
    ))

    // Process Footnotes

    // Retrieve footnote bodies // TODO shove them into PageContent
    val footnoteBodies: Map[String, Chunk[Xml.Node]] = Footnotes.footnoteBodies(xmlLinksProcessed, xmlDialect)
    val xmlFootnoteBodiesProcesses: Xml.Element = Footnotes.removeFootnoteBodies(xmlLinksProcessed, markup)
    val xmlResult: Xml.Element = Processor.process(xmlDialect, xmlFootnoteBodiesProcesses, Seq(
      FootnotesTransformer(footnoteBodies, ids, this) // TODO unfold!
    ))

    PageContent(
      source = this,
      frontMatter = frontMatter,
      title = title,
      xml = xmlResult,
    )
