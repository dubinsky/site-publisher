package org.podval.tools.publish.util

import com.sun.net.httpserver.{HttpExchange, HttpServer, SimpleFileServer}
import java.io.{File, UncheckedIOException}
import java.net.{BindException, InetSocketAddress, URLConnection}
import java.nio.file.Files as NioFiles

object Http:
  val localhost: String = "127.0.0.1"
  val defaultPort: Int = 8000

  // Prefer 8000 so `serve()` has a stable URL. If it is taken (another serve,
  // CI sibling, …), bind an ephemeral port; callers read the real port from
  // the returned server / `httpServerPort` / `Page.uri`.
  def start(
    root: File,
    rewrite: String => Option[String] = _ => None
  ): HttpServer =
    val rootPath: java.nio.file.Path = root.getAbsoluteFile.toPath
    try
      startOn(rootPath, defaultPort, rewrite)
    catch
      case e: UncheckedIOException if e.getCause.isInstanceOf[BindException] =>
        startOn(rootPath, 0, rewrite)
      case e: java.io.IOException if e.getCause.isInstanceOf[BindException] =>
        startOn(rootPath, 0, rewrite)
      case e: BindException =>
        startOn(rootPath, 0, rewrite)

  private def startOn(
    root: java.nio.file.Path,
    port: Int,
    rewrite: String => Option[String]
  ): HttpServer =
    val address: InetSocketAddress = InetSocketAddress(localhost, port)
    val files = SimpleFileServer.createFileHandler(root)
    val result: HttpServer = HttpServer.create(address, 0)
    result.createContext("/", (ex: HttpExchange) =>
      val raw: String = Option(ex.getRequestURI.getPath).getOrElse("/")
      rewrite(raw) match
        case Some(target) =>
          val file: File = fileAt(root, target)
          if file.isFile then sendFile(ex, file)
          else
            ex.sendResponseHeaders(404, -1)
            ex.close()
        case None =>
          files.handle(ex)
    )
    result.setExecutor(null)
    result.start()
    result

  private def fileAt(root: java.nio.file.Path, urlPath: String): File =
    urlPath.split("/").filter(_.nonEmpty).foldLeft(root)(_.resolve(_)).toFile

  private def sendFile(ex: HttpExchange, file: File): Unit =
    val mime: String =
      Option(URLConnection.guessContentTypeFromName(file.getName)).getOrElse("application/octet-stream")
    val bytes: Array[Byte] = NioFiles.readAllBytes(file.toPath)
    ex.getResponseHeaders.set("Content-Type", mime)
    val writeBody: Boolean = !ex.getRequestMethod.equalsIgnoreCase("HEAD")
    if writeBody then
      ex.sendResponseHeaders(200, bytes.length.toLong)
      val out = ex.getResponseBody
      out.write(bytes)
      out.close()
    else
      ex.sendResponseHeaders(200, -1)
      ex.close()
