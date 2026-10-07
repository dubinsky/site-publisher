package org.podval.tools.publish.prose

import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files as NioFiles, Path as NioPath, StandardCopyOption, StandardOpenOption}

/** UTF-8 decode and atomic replace shared by XML and prose pretty-print. */
object ProseIO:
  def decodeUtf8(bytes: Array[Byte]): Either[CharacterCodingException, String] =
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch case error: CharacterCodingException => Left(error)

  def replace(file: File, output: String): Unit =
    val path: NioPath = file.toPath
    val temp: NioPath = path.resolveSibling(path.getFileName.toString + ".pretty-print.tmp")
    try
      NioFiles.writeString(
        temp,
        output,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      NioFiles.move(temp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case error: Exception =>
        NioFiles.deleteIfExists(temp)
        throw error
