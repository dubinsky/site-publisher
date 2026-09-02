package org.podval.tools.publish.util

import com.sun.net.httpserver.{HttpServer, SimpleFileServer}
import org.scalatest.funsuite.AnyFunSuite
import java.io.{File, UncheckedIOException}
import java.net.{BindException, InetSocketAddress, URI}
import java.nio.file.Files as NFiles

final class HttpServerSpec extends AnyFunSuite:
  test("falls back to an ephemeral port when the default port is taken") {
    val dir: File = NFiles.createTempDirectory("site-publisher-http-").toFile
    dir.deleteOnExit()
    val blocker: Option[HttpServer] =
      try
        val server: HttpServer = SimpleFileServer.createFileServer(
          InetSocketAddress(Http.localhost, Http.defaultPort),
          dir.toPath,
          SimpleFileServer.OutputLevel.NONE
        )
        server.start()
        Some(server)
      catch
        case e: UncheckedIOException if e.getCause.isInstanceOf[BindException] => None
    try
      val server: HttpServer = Http.start(dir)
      try
        val port: Int = server.getAddress.getPort
        assert(port > 0, s"server did not bind: $port")
        assert(port != Http.defaultPort, s"expected ephemeral port, got $port")
      finally
        server.stop(0)
    finally
      blocker.foreach(_.stop(0))
  }

  test("rewrites an alias URL to the written file") {
    val dir: File = NFiles.createTempDirectory("site-publisher-http-alias-").toFile
    dir.deleteOnExit()
    val nested: File = File(dir, "archive/case")
    nested.mkdirs()
    Files.write(File(nested, "3140.html"), "collection-body")
    val doc: File = File(File(nested, "3140"), "003.html")
    doc.getParentFile.mkdirs()
    Files.write(doc, "doc-003")
    val rewrite: String => Option[String] =
      request =>
        val path: String = if request.endsWith(".html") then request.dropRight(5) else request
        if path == "/rgada" then Some("/archive/case/3140.html")
        else if path == "/rgada/003" then Some("/archive/case/3140/003.html")
        else None
    val server: HttpServer = Http.start(dir, rewrite)
    try
      val port: Int = server.getAddress.getPort
      def get(path: String): String =
        val in = URI.create(s"http://${Http.localhost}:$port$path").toURL.openStream()
        try String(in.readAllBytes())
        finally in.close()
      assert(get("/rgada.html").contains("collection-body"))
      assert(get("/rgada/003.html").contains("doc-003"))
      assert(get("/rgada/003").contains("doc-003"))
    finally
      server.stop(0)
  }
