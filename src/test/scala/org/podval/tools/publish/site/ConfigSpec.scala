package org.podval.tools.publish.site

import org.scalatest.funsuite.AnyFunSuite

final class ConfigSpec extends AnyFunSuite:
  private def decode(yaml: String): Config =
    Config.codec.decode(yaml) match
      case Left(error) => fail(error.getMessage)
      case Right(config) => config

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
