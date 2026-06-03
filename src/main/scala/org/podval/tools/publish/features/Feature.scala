package org.podval.tools.publish.features

import org.podval.tools.publish.util.IdGenerator
import org.podval.tools.publish.{Page, PageError, Toc}
import org.podval.xml.{Html, Xml, XmlDialect}

// Natural unit of functionality - and extending it ;)
abstract class Feature(
  val processPriority: Int = 0,
  val transformPriority: Int = 0,
  val postProcessPriority: Int = 0,
  val postProcessHtmlPriority: Int = 0
):

  // Called on load, both initial and after cache eviction,
  // for each element from within a `transform()`.
  def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element = element

  // Called on load, both initial and after cache eviction,
  // and can run its own `transform()`s.
  def transform(
    element: Xml.Element,
    context: Feature.TransformContext
  ): Xml.Element = element

  // Called when preparing the final result,
  // for each element from within a `trancform()`.
  def postProcess(
    element: Xml.Element,
    context: Feature.PostProcessContext
  ): Xml.Element = element

  // TODO is this where I add date tooltip?
  def postProcessHtml(
    element: Html.Element,
    context: Feature.PostProcessHtmlContext
  ): Html.Element = element

  final protected def convertText(
    element: Xml.Element,
    converter: String => Seq[Xml.Node]
  ): Xml.Element =
    element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(converter)))

object Feature:
  final class ProcessContext(
    ids: IdGenerator,
    val siteUrl: String,
    val errorReporter: PageError.Reporter
  ):
    def generateId(): String = ids.generate()

  final class TransformContext(
    val xmlDialect: XmlDialect
  )

  final class PostProcessContext(
    val page: Page,
    val errorReporter: PageError.Reporter
  )

  final class PostProcessHtmlContext(
    val toc: Toc
  )
