package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Xml

object PostConverter:
  val id: PostConverter = new PostConverter {}

  def concat(postConverters: PostConverter*): PostConverter = postConverters.reduce(_.andThen(_))

  private final class AndThen(left: PostConverter, right: PostConverter) extends PostConverter:
    override protected def postConvert(
      element: Xml.Element,
      source: PageSource
    ): Option[Xml.Element] =
      val postConvertedByLeft: Xml.Element = left.doPostConvert(
        element,
        source
      )

      val result: Xml.Element = right.doPostConvert(
        postConvertedByLeft,
        source
      )
      
      Some(result)

// Converts individual XML elements.
abstract class PostConverter:
  def andThen(right: PostConverter): PostConverter = PostConverter.AndThen(this, right)

  final def doPostConvert(
    element: Xml.Element,
    source: PageSource
  ): Xml.Element =
    postConvert(
      element,
      source
    )
      .getOrElse(element)
      
  protected def postConvert(
    element: Xml.Element,
    source: PageSource
  ): Option[Xml.Element] = postConvert(
    element
  )

  protected def postConvert(element: Xml.Element): Option[Xml.Element] =
    None
