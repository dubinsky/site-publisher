package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{DocBookMarkup, EntityKind, Markup, TeiMarkup, XmlMarkup}
import org.podval.tools.publish.util.Files
import org.podval.xml.{Xml, XmlParser, XmlWriterConfig}
import Xml.given
import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files as NioFiles, Path as NioPath, StandardCopyOption, StandardOpenOption}

/** Rewrite authored TEI and DocBook sources in place. Does not load the page graph. */
object PrettyPrint:
  def run(site: Site): Unit =
    val root: NioPath = site.sourceDirectory.toPath.toRealPath()
    val counts: Counts = walk(site, root, Seq.empty, site.sourceDirectory, Counts())
    site.log.info(counts.summary)

  /** v1 returns the dialect config. `rootLocalName` is the switch for a later per-kind config. */
  def writerConfig(markup: Markup, rootLocalName: String): XmlWriterConfig =
    rootLocalName match
      case _ => markup.xmlWriterConfig

  private def walk(
    site: Site,
    root: NioPath,
    path: Seq[String],
    directory: File,
    counts: Counts
  ): Counts =
    if !inside(root, directory) then
      warn(site, directory, "path realpath is outside the source directory")
      counts.incOutside
    else
      val pathString: String = if path.isEmpty then "/" else path.mkString("/", "/", "/")
      Files.list(directory).foldLeft(counts): (acc, child) =>
        val ignored: Boolean = site.ignore.isIgnored(s"$pathString${child.getName}", child.isDirectory)
        if ignored then
          site.log.debug(s"Ignored: $child")
          acc
        else if child.isDirectory then
          walk(site, root, path :+ child.getName, child, acc)
        else
          acc.plus(file(site, root, path, child))

  private def file(site: Site, root: NioPath, path: Seq[String], child: File): Counts =
    val (name, extension) = Files.nameAndExtension(child.getName)
    if !extension.contains(XmlMarkup.extension) then Counts()
    else if !inside(root, child) then
      warn(site, child, "path realpath is outside the source directory")
      Counts().incConsidered.incOutside
    else
      val sourcePath: Path = Path(path :+ name, extension)
      if Pages.isAssetPassThrough(site, sourcePath, sidecar(site, sourcePath)) then
        site.log.debug(s"asset pass-through: ${relative(site, child)}")
        Counts()
      else
        format(site, child, sourcePath)

  private def sidecar(site: Site, markup: Path): Option[Path] =
    Seq("yml", "yaml")
      .map(markup.withExtension)
      .find(path => site.sourceFile(path).isFile)

  private def format(site: Site, file: File, sourcePath: Path): Counts =
    val relativePath: String = relative(site, file)
    val bytes: Array[Byte] = NioFiles.readAllBytes(file.toPath)
    decodeUtf8(bytes) match
      case Left(error) =>
        warn(site, file, error.getMessage)
        Counts().incConsidered.incBadUtf8
      case Right(text) =>
        val (rawPrefix, body) = peelFrontMatter(text)
        if body.contains("xml:space") then
          warn(site, file, "xml:space")
          Counts().incConsidered.incXmlSpace
        else if internalSubset(body) then
          warn(site, file, "doctype internal subset")
          Counts().incConsidered.incInternalSubset
        else XmlParser.parseXmlDocument[Xml.Element](body) match
          case Left(error) =>
            warn(site, file, error.toString)
            Counts().incConsidered.incMalformed
          case Right(document) =>
            Markup.forElement(document.root.getName.qName) match
              case None =>
                warn(site, file, s"unknown root ${document.root.getName.qName}")
                Counts().incConsidered.incUnknownRoot
              case Some(markup) =>
                val rendered: String = writerConfig(markup, document.root.getName.localName).render(document)
                val output: String = rawPrefix + rendered
                val kind: Counts = kindOf(markup, document.root)
                if output == text then
                  site.log.debug(s"unchanged $relativePath")
                  Counts().incConsidered.incUnchanged.plus(kind)
                else
                  replace(file, output)
                  site.log.info(s"pretty-print $relativePath")
                  Counts().incConsidered.incWrote.plus(kind)

  def peelFrontMatter(raw: String): (String, String) =
    if !raw.startsWith("---\n") then ("", raw)
    else raw.indexOf("\n---\n", 3) match
      case -1 => ("", raw)
      case end =>
        val cut: Int = end + "\n---\n".length
        (raw.take(cut), raw.drop(cut))

  private def internalSubset(body: String): Boolean =
    val start: Int = body.indexOf("<!DOCTYPE")
    if start < 0 then false
    else
      val end: Int = body.indexOf('>', start)
      end >= 0 && body.substring(start, end).contains('[')

  private def kindOf(markup: Markup, root: Xml.Element): Counts =
    if markup eq DocBookMarkup then Counts(docbook = 1)
    else if TeiMarkup.isStoreRoot(root) then Counts(store = 1)
    else if root.isNamed("entityLists") then Counts(entityList = 1)
    else if root.isNamed("TEI") then Counts(tei = 1)
    else if EntityKind.forElement(root.getName.localName).isDefined then Counts(entity = 1)
    else Counts(tei = 1)

  private def replace(file: File, output: String): Unit =
    val path: NioPath = file.toPath
    val temp: NioPath = path.resolveSibling(path.getFileName.toString + ".pretty-print.tmp")
    try
      NioFiles.writeString(temp, output, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      NioFiles.move(temp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case error: Exception =>
        NioFiles.deleteIfExists(temp)
        throw error

  private def decodeUtf8(bytes: Array[Byte]): Either[CharacterCodingException, String] =
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch case error: CharacterCodingException => Left(error)

  private def inside(root: NioPath, file: File): Boolean =
    try file.toPath.toRealPath().startsWith(root)
    catch case _: java.io.IOException => false

  private def relative(site: Site, file: File): String =
    site.sourceDirectory.toPath.relativize(file.toPath).toString

  private def warn(site: Site, file: File, cause: String): Unit =
    site.log.warn(s"skip ${relative(site, file)}: $cause")

  final case class Counts(
    considered: Int = 0,
    wrote: Int = 0,
    unchanged: Int = 0,
    malformed: Int = 0,
    unknownRoot: Int = 0,
    xmlSpace: Int = 0,
    internalSubset: Int = 0,
    badUtf8: Int = 0,
    outside: Int = 0,
    tei: Int = 0,
    store: Int = 0,
    entityList: Int = 0,
    entity: Int = 0,
    docbook: Int = 0
  ):
    def plus(other: Counts): Counts = Counts(
      considered = considered + other.considered,
      wrote = wrote + other.wrote,
      unchanged = unchanged + other.unchanged,
      malformed = malformed + other.malformed,
      unknownRoot = unknownRoot + other.unknownRoot,
      xmlSpace = xmlSpace + other.xmlSpace,
      internalSubset = internalSubset + other.internalSubset,
      badUtf8 = badUtf8 + other.badUtf8,
      outside = outside + other.outside,
      tei = tei + other.tei,
      store = store + other.store,
      entityList = entityList + other.entityList,
      entity = entity + other.entity,
      docbook = docbook + other.docbook
    )

    def incConsidered: Counts = copy(considered = considered + 1)
    def incWrote: Counts = copy(wrote = wrote + 1)
    def incUnchanged: Counts = copy(unchanged = unchanged + 1)
    def incMalformed: Counts = copy(malformed = malformed + 1)
    def incUnknownRoot: Counts = copy(unknownRoot = unknownRoot + 1)
    def incXmlSpace: Counts = copy(xmlSpace = xmlSpace + 1)
    def incInternalSubset: Counts = copy(internalSubset = internalSubset + 1)
    def incBadUtf8: Counts = copy(badUtf8 = badUtf8 + 1)
    def incOutside: Counts = copy(outside = outside + 1)

    def summary: String =
      s"pretty-print considered $considered, wrote $wrote, unchanged $unchanged, " +
        s"malformed $malformed, unknown-root $unknownRoot, xml-space $xmlSpace, " +
        s"internal-subset $internalSubset, bad-utf8 $badUtf8, outside $outside, " +
        s"tei $tei, store $store, entity-list $entityList, entity $entity, docbook $docbook"
