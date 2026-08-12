package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

final class Highlights(languages: Set[String]) extends JSLibrary:
  val version: String = "11.11.1"

  override val stylesheet: Some[String] = Some("/styles/default.min.css")

  override def imports: List[String] =
    List("highlight.min.js") ++ languages
      .flatMap(Highlights.languageFile)
      .toList
      .sorted
      .map(language => s"languages/$language.min.js")

  override val inlineJs: Some[Js] = Some(js"hljs.highlightAll();")

  override def cdn: String =
    if JSLibrary.preferCloudFlare
    then s"${JSLibrary.cloudFlare}highlight.js/$version"
    else s"${JSLibrary.jsDelivr}@highlightjs/cdn-assets@$version"

object Highlights:
  /**
   * Map a fenced-code language tag to the highlight.js language file name.
   * Returns None when there is no grammar to load (unknown or handled elsewhere).
   */
  private def languageFile(language: String): Option[String] =
    val name: String = language.toLowerCase
    if unsupported.contains(name) then None
    else Some(aliases.getOrElse(name, name))

  // No CDN grammar (or handled by another library).
  private val unsupported: Set[String] = Set(
    "liquid",   // not in highlight.js
    "mermaid"   // rendered by Mermaid, not highlight.js
  )

  private val aliases: Map[String, String] = Map(
    "html" -> "xml",
    "xhtml" -> "xml",
    "svg" -> "xml",
    "rss" -> "xml",
    "text" -> "plaintext",
    "txt" -> "plaintext",
    "plain" -> "plaintext",
    "md" -> "markdown",
    "mkd" -> "markdown",
    "js" -> "javascript",
    "jsx" -> "javascript",
    "mjs" -> "javascript",
    "cjs" -> "javascript",
    "ts" -> "typescript",
    "tsx" -> "typescript",
    "yml" -> "yaml",
    "sh" -> "bash",
    "zsh" -> "bash",
    "shell" -> "bash",
    "rb" -> "ruby",
    "py" -> "python",
    "rs" -> "rust",
    "c++" -> "cpp",
    "h" -> "c",
    "hpp" -> "cpp",
    "cs" -> "csharp",
    "c#" -> "csharp",
    "kt" -> "kotlin",
    "fs" -> "fsharp",
    "docker" -> "dockerfile",
    "obj-c" -> "objectivec",
    "objc" -> "objectivec",
    "ps" -> "powershell",
    "ps1" -> "powershell"
  )
