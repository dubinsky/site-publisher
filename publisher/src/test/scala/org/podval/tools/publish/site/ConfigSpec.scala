package org.podval.tools.publish.site

import org.scalatest.funsuite.AnyFunSuite

final class ConfigSpec extends AnyFunSuite:
  private def decode(yaml: String): Config =
    Config.decode(yaml) match
      case Left(error) => fail(error.getMessage)
      case Right(config) => config

  private def reject(yaml: String): String =
    Config.decode(yaml) match
      case Left(error) => error.getMessage
      case Right(_) => fail("expected an unknown config key")

  private val required: String =
    """title: T
      |description: D
      |url: http://t.test
      |author: A
      |email: a@t.test
      |""".stripMargin

  test("facsimiles-url is optional") {
    assert(decode(required).facsimilesUrl.isEmpty)
  }

  test("facsimiles-url maps from kebab-case") {
    val config: Config = decode(
      required + "facsimiles-url: https://storage.googleapis.com/facsimiles.alter-rebbe.org/\n"
    )
    assert(config.facsimilesUrl.contains("https://storage.googleapis.com/facsimiles.alter-rebbe.org/"))
  }

  test("tei-default-calendar is optional") {
    assert(decode(required).teiDefaultCalendar.isEmpty)
  }

  test("tei-default-calendar maps from kebab-case") {
    val config: Config = decode(required + "tei-default-calendar: julian\n")
    assert(config.teiDefaultCalendar.contains("julian"))
  }

  test("named-windows defaults to false") {
    assert(!decode(required).namedWindows)
  }

  test("named-windows maps from kebab-case") {
    assert(decode(required + "named-windows: true\n").namedWindows)
  }

  test("check-links defaults to false") {
    assert(!decode(required).checkLinks)
  }

  test("check-links maps from kebab-case") {
    assert(decode(required + "check-links: true\n").checkLinks)
  }

  test("graph defaults to off") {
    val graph: Config.Graph = decode(required).graph
    assert(!graph.enabled)
    assert(graph.includeTransclusions)
    assert(graph.excludePathPrefixes.isEmpty)
  }

  test("unknown keys are rejected, including nested graph keys") {
    val message: String = reject(
      required +
        """facsimile-url: http://x
          |graph:
          |  mystery: days
          |""".stripMargin
    )
    assert(message.contains("facsimile-url"), message)
    assert(message.contains("graph.mystery"), message)
  }

  test("camelCase config keys are unknown") {
    val message: String = reject(required + "namedWindows: true\n")
    assert(message.contains("namedWindows"), message)
  }

  test("graph maps from kebab-case") {
    val graph: Config.Graph = decode(
      required +
        """graph:
          |  enabled: true
          |  include-transclusions: false
          |  exclude-path-prefixes:
          |    - days
          |""".stripMargin
    ).graph
    assert(graph.enabled)
    assert(!graph.includeTransclusions)
    assert(graph.excludePathPrefixes == List("days"))
  }
