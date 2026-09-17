package org.podval.tools.publish.util

import java.io.{File, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{StandardCopyOption, StandardOpenOption, Files as NFiles}

object Files:
  def requireExists(file: File): Unit = require(file.exists, s"File does not exist: $file")

  def requireDirectory(file: File): Unit = require(file.isDirectory, s"File is not a directory: $file")

  def requireFile(file: File): Unit = require(file.isFile, s"File is a directory: $file")

  def list(directory: File): List[File] = Option(directory.listFiles).getOrElse(Array.empty[File]).toList

  def nameAndExtension(fullName: String): (String, Option[String]) = Strings.split(fullName, '.')

  def read(file: File): String = new String(NFiles.readAllBytes(file.toPath))

  def write(toFile: File, content: String): Unit =
    toFile.getParentFile.mkdirs()
    NFiles.writeString(toFile.toPath, content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

  def deleteDirectory(file: File): Unit =
    if file.isDirectory then file.listFiles.foreach(deleteDirectory)
    file.delete

  def copy(fromFile: File, toFile: File): Unit =
    requireExists(fromFile)
    requireFile(fromFile)
    toFile.getParentFile.mkdirs()
    NFiles.copy(fromFile.toPath, toFile.toPath, StandardCopyOption.REPLACE_EXISTING)

  def readResource(name: String): String =
    val stream: InputStream = getClass.getResourceAsStream(name)
    try
      String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally
      stream.close()

