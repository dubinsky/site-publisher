package org.podval.tools.publish.site

import com.sun.net.httpserver.{HttpServer, SimpleFileServer}
import org.scalatest.funsuite.AnyFunSuite
import java.io.{File, UncheckedIOException}
import java.net.{BindException, InetSocketAddress}
import java.nio.file.Files as NFiles

final class HttpServerSpec extends AnyFunSuite:
  test("falls back to an ephemeral port when the default port is taken") {
    val dir: File = NFiles.createTempDirectory("site-publisher-http-").toFile
    dir.deleteOnExit()
    val blocker: Option[HttpServer] =
      try
        val server: HttpServer = SimpleFileServer.createFileServer(
          InetSocketAddress(Site.localhost, Site.defaultHttpPort),
          dir.toPath,
          SimpleFileServer.OutputLevel.NONE
        )
        server.start()
        Some(server)
      catch
        case e: UncheckedIOException if e.getCause.isInstanceOf[BindException] => None
    try
      val server: HttpServer = Site.startHttpServer(dir)
      try
        val port: Int = server.getAddress.getPort
        assert(port > 0, s"server did not bind: $port")
        assert(port != Site.defaultHttpPort, s"expected ephemeral port, got $port")
      finally
        server.stop(0)
    finally
      blocker.foreach(_.stop(0))
  }
