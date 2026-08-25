package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.asciidoctor.ast.{PhraseNode, StructuralNode}
import org.asciidoctor.extension.{BlockMacroProcessor, InlineMacroProcessor}
import scala.jdk.CollectionConverters.MapHasAsScala

object AsciiDocCiteExtension:
  def register(asciidoctor: Asciidoctor): Unit =
    val registry = asciidoctor.javaExtensionRegistry()
    registry.inlineMacro(CiteMacro("cite", Citation.Mode.Parenthetical))
    registry.inlineMacro(CiteMacro("citenp", Citation.Mode.Narrative))
    registry.blockMacro(BibliographyMacro())

/** Default Asciidoctor inline-macro regexp is `name:(\S+?)\[…\]`, so `cite:[key]` (empty
  * target, keys in the attrlist — the bibtex-gem form) never matches. Allow an empty target
  * so both `cite:[key]` and `cite:key[locator]` work. */
final class CiteMacro(name: String, mode: Citation.Mode) extends InlineMacroProcessor(
  name,
  CiteMacro.config(name)
):
  override def process(
    parent: StructuralNode,
    target: String,
    attributes: java.util.Map[String, Object]
  ): PhraseNode =
    val html: String = Citation.toHtmlString(
      Citation.cite(mode, Citation.parseAsciiDocTarget(CiteMacro.raw(target, attributes)))
    )
    val options = new java.util.HashMap[String, Object]()
    options.put("type", ":pass")
    createPhraseNode(parent, "quoted", html, java.util.Map.of(), options)

object CiteMacro:
  def config(name: String): java.util.Map[String, Object] =
    val result = new java.util.HashMap[String, Object]()
    result.put(InlineMacroProcessor.REGEXP, s"\\b$name:(\\S*?)\\[(.*?)\\]")
    result

  def raw(target: String, attributes: java.util.Map[String, Object]): String =
    val fromTarget: Option[String] = Option(target).map(_.trim).filter(_.nonEmpty)
    val fromAttributes: String = attributeText(attributes).trim
    (fromTarget, fromAttributes) match
      case (Some(key), locator) if locator.nonEmpty => s"$key, $locator"
      case (Some(key), _) => key
      case (None, locator) => locator

  def attributeText(attributes: java.util.Map[String, Object]): String =
    val map = attributes.asScala
    val positional: Seq[String] = map
      .toSeq
      .collect { case (k, v) if k.forall(_.isDigit) => k.toInt -> v.toString }
      .sortBy(_._1)
      .map(_._2)
    if positional.nonEmpty then positional.mkString(", ")
    else map.get("text").map(_.toString).getOrElse("")

final class BibliographyMacro extends BlockMacroProcessor("bibliography"):
  override def process(
    parent: StructuralNode,
    target: String,
    attributes: java.util.Map[String, Object]
  ): StructuralNode =
    val html: String = Citation.toHtmlString(Citation.listPlaceholder)
    createBlock(parent, "pass", html)
