package org.podval.tools.publish.util

import com.sun.net.httpserver.{SimpleFileServer, HttpServer}
import java.io.File
import java.net.InetSocketAddress

object HttpServerUtil:
  def httpServer(siteRoot: File): HttpServer =
    val result: HttpServer = SimpleFileServer.createFileServer(
      InetSocketAddress("127.0.0.1", 0),
      siteRoot.getAbsoluteFile.toPath,
      SimpleFileServer.OutputLevel.NONE
    )
    result.start()
    result
