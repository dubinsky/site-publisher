package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.io.File
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NioFiles, Path as NioPath}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

final class ExternalLinksSpec extends AnyFunSuite with BeforeAndAfterAll:
  private val requests: ConcurrentLinkedQueue[(String, String)] = ConcurrentLinkedQueue()
  private var server: Option[HttpServer] = None

  override def beforeAll(): Unit =
    super.beforeAll()
    val created: HttpServer = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
    created.createContext("/", (exchange: HttpExchange) =>
      val method: String = exchange.getRequestMethod
      val path: String = Option(exchange.getRequestURI.getPath).getOrElse("/")
      requests.add((method, path))
      val code: Int =
        if path == "/head-405" && method.equalsIgnoreCase("HEAD") then 405
        else if path == "/missing" || path == "/missing-note" then 404
        else 200
      if method.equalsIgnoreCase("HEAD") then
        exchange.sendResponseHeaders(code, -1)
        exchange.close()
      else
        val body: Array[Byte] = "ok".getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(code, body.length.toLong)
        val out = exchange.getResponseBody
        out.write(body)
        out.close()
    )
    created.start()
    server = Some(created)

  override def afterAll(): Unit =
    server.foreach(_.stop(0))
    super.afterAll()

  private def port: Int = server.get.getAddress.getPort

  private def base: String = s"http://127.0.0.1:$port"

  private def hits(path: String): Seq[String] =
    requests.asScala.toSeq.collect:
      case (method, hit) if hit == path => method

  private def config(check: Boolean): String =
    val flag: String = if check then "check-links: true\n" else ""
    s"""title: External Links
       |description: checker
       |url: http://links.test
       |author: Test
       |email: test@links.test
       |$flag""".stripMargin

  private def generate(files: Map[String, String]): String =
    requests.clear()
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-external-links")
    try
      val dir: File = path.toFile
      files.foreach: (relative, content) =>
        Files.write(File(dir, relative), content)
      val target: File = File(dir, "_site")
      Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(target.getAbsolutePath),
        treatErrorsAsWarnings = true,
        logLevelOpt = Some("WARN")
      )).generate()
      Files.read(File(target, "errors.html"))
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private def countBroken(errors: String): Int =
    "broken external link:".r.findAllIn(errors).size

  test("check-links off does not request a broken url") {
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = false),
      "index.md" -> s"[dead]($base/missing)\n"
    ))
    assert(!errors.contains("broken external"), errors)
    assert(requests.isEmpty, requests.toString)
  }

  test("a live anchor and image are requested once and not reported") {
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = true),
      "index.md" ->
        s"""[ok]($base/ok)
           |
           |![pic]($base/ok)
           |""".stripMargin
    ))
    assert(!errors.contains("broken external"), errors)
    assert(hits("/ok") == Seq("HEAD"), requests.toString)
  }

  test("duplicate urls are fetched once and reported once per page") {
    val missing: String = s"$base/missing"
    val note: String = s"$base/missing-note"
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = true),
      "index.md" -> "Home.\n",
      "_posts/2020-01-02-dead.md" ->
        s"""[a]($missing) and [b]($missing#frag).[^n]
           |
           |[^n]: [c]($note)
           |""".stripMargin,
      "other.md" -> s"[d]($missing)\n"
    ))
    assert(countBroken(errors) == 3, errors)
    assert(errors.contains("/_posts/2020-01-02-dead.md"), errors)
    assert(errors.contains("/other.md"), errors)
    assert(errors.contains(note), errors)
    assert(hits("/missing") == Seq("HEAD"), requests.toString)
    assert(hits("/missing-note") == Seq("HEAD"), requests.toString)
  }

  test("mailto and a same-host url are not requested") {
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = true),
      "index.md" ->
        """[mail](mailto:a@b.test)
          |[self](http://links.test/nope)
          |""".stripMargin
    ))
    assert(requests.isEmpty, requests.toString)
    assert(errors.contains("spurious external"), errors)
    assert(errors.contains("http://links.test/nope"), errors)
    assert(!errors.contains("broken external"), errors)
    assert(!errors.contains("mailto:a@b.test"), errors)
  }

  test("HEAD 405 is retried with GET") {
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = true),
      "index.md" -> s"[x]($base/head-405)\n"
    ))
    assert(!errors.contains("broken external"), errors)
    assert(hits("/head-405") == Seq("HEAD", "GET"), requests.toString)
  }

  test("a url inside a fenced code block is not requested") {
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = true),
      "index.md" ->
        s"""```
           |$base/missing
           |```
           |""".stripMargin
    ))
    assert(!errors.contains("broken external"), errors)
    assert(requests.isEmpty, requests.toString)
  }

  test("a transcluded url is reported on the source page") {
    val errors: String = generate(Map(
      "_site_config.yml" -> config(check = true),
      "index.md" -> "Home.\n",
      "target.md" ->
        s"""---
           |title: Target
           |---
           |[dead]($base/missing)
           |""".stripMargin,
      "host.md" ->
        """---
          |title: Host
          |---
          |![[target]]
          |""".stripMargin
    ))
    assert(countBroken(errors) == 1, errors)
    assert(errors.contains("/target.md"), errors)
    assert(!errors.contains("/host.md"), errors)
    assert(hits("/missing") == Seq("HEAD"), requests.toString)
  }
