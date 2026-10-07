package org.podval.tools.publish.prose

import org.eclipse.jgit.ignore.IgnoreNode
import org.eclipse.jgit.ignore.IgnoreNode.MatchResult
import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.page.FrontMatter
import org.podval.tools.publish.util.{Files, Options}
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NioFiles, Path as NioPath}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Pretty-print Markdown and AsciiDoc files. Does not build a `Site`. */
object ProsePrettyPrint:
  private val defaultHome: NioPath = NioPath.of(sys.props("user.home"))

  def main(args: Array[String]): Unit =
    val code: Int = run(args)
    if code != 0 then sys.exit(code)

  def run(args: Array[String], home: NioPath = defaultHome): Int =
    val options: Options = Options(args, environmentVariablesPrefix = "SITE_PUBLISHER")
    ProseOptions.from(options) match
      case Left(flag) =>
        Console.err.println(s"bad --$flag")
        2
      case Right(_) if options.positionals.isEmpty =>
        Console.err.println("usage: pretty-print [--width=N] [--sentence-per-line=true|false] FILE...")
        2
      case Right(prose) =>
        val skipped: Boolean = options.positionals.map(path => visit(new File(path), prose, home, None)).exists(identity)
        if skipped then 1 else 0

  /** @return true when a considered file was skipped. */
  private def visit(file: File, options: ProseOptions, home: NioPath, ignore: Option[IgnoreNode]): Boolean =
    if !file.exists then
      skip(file.getPath, "unreadable")
      true
    else if isMemory(file, home) then
      skip(relative(file), "memory")
      true
    else if file.isDirectory then
      val root: NioPath = Try(file.toPath.toRealPath()).getOrElse(file.toPath.toAbsolutePath)
      val rules: IgnoreNode = ignore.getOrElse(siteIgnore(file))
      val stream = NioFiles.list(file.toPath)
      try
        stream.iterator.asScala.foldLeft(false): (failed, child) =>
          val childFile: File = child.toFile
          val escaped: Boolean = !inside(root, childFile)
          if escaped then
            skip(childFile.getPath, "outside")
            true
          else if ignored(rules, root, childFile) then failed
          else failed || visit(childFile, options, home, Some(rules))
      finally stream.close()
    else
      val extension: Option[String] = Files.nameAndExtension(file.getName)._2
      extension.flatMap(Markup.forExtension).flatMap(_.proseFormatter) match
        case None =>
          if ignore.isEmpty then
            skip(file.getPath, "not-prose")
            true
          else false
        case Some(prose) =>
          formatFile(file, options, prose)

  private def formatFile(file: File, options: ProseOptions, prose: ProseFormatter): Boolean =
    val bytes: Array[Byte] = NioFiles.readAllBytes(file.toPath)
    ProseIO.decodeUtf8(bytes) match
      case Left(_) =>
        skip(file.getPath, "bad-utf8")
        true
      case Right(text) if assetPassThrough(file, text) =>
        skip(file.getPath, "asset")
        true
      case Right(text) =>
        prose.format(text, options) match
          case Left(failure) =>
            skip(file.getPath, failure.reason)
            true
          case Right(output) =>
            if output != text then ProseIO.replace(file, output)
            val verb: String = if output == text then "unchanged" else "wrote"
            println(s"$verb ${file.getPath}")
            false

  private def assetPassThrough(file: File, text: String): Boolean =
    val internal: Boolean = FrontMatter.split(text)._1.isDefined
    val sidecar: Option[File] = Seq("yml", "yaml")
      .map(ext => new File(file.getParentFile, Files.nameAndExtension(file.getName)._1 + "." + ext))
      .find(_.isFile)
    val asset: Boolean = sidecar.exists: side =>
      FrontMatter.parse(Some(NioFiles.readString(side.toPath, StandardCharsets.UTF_8))).exists(_.asset)
    asset && !internal

  private def siteIgnore(directory: File): IgnoreNode =
    val file: File = new File(directory, "_site_ignore")
    val rules: String = if file.isFile then NioFiles.readString(file.toPath) else ""
    val node = new IgnoreNode()
    node.parse(new ByteArrayInputStream(rules.getBytes(StandardCharsets.UTF_8)))
    node

  private def ignored(node: IgnoreNode, root: NioPath, file: File): Boolean =
    val path: String = "/" + root.relativize(file.toPath.toAbsolutePath).toString.replace('\\', '/')
    given CanEqual[MatchResult, MatchResult] = CanEqual.derived
    node.isIgnored(path, file.isDirectory) == MatchResult.IGNORED

  def isMemory(file: File, home: NioPath): Boolean =
    val root: NioPath = home.resolve(".grok").resolve("memory-v2")
    Try(file.toPath.toRealPath()).toOption.exists: real =>
      Try(root.toRealPath()).toOption.exists(real.startsWith)

  private def inside(root: NioPath, file: File): Boolean =
    Try(file.toPath.toRealPath()).toOption.exists(_.startsWith(root))

  private def relative(file: File): String = file.getPath

  private def skip(path: String, reason: String): Unit =
    Console.err.println(s"skipped $path: $reason")
