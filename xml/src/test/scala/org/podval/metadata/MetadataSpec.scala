package org.podval.metadata

import org.podval.xml.{XmlParser, Xml as ZioXml}
import org.scalatest.funsuite.AnyFunSuite

final class MetadataSpec extends AnyFunSuite:
  test("Language names") {
    assert(Language.English.names.names.length == 4)
    assert(Language.Hebrew.names.hasName("he"))
    assert(Language.Hebrew.names.hasName("иврит"))
  }

  test("Name codec accepts n or text but not both") {
    def decode(xml: String) =
      Name.codec.decode(XmlParser.parseXml(xml).toOption.get)(using ZioXml)

    val fromN = decode("""<name lang="en" n="English"/>""").toOption.get
    assert(fromN.name == "English")
    assert(fromN.languageSpec.language.contains(Language.English))

    val fromText = decode("""<name lang="ru">русский</name>""").toOption.get
    assert(fromText.name == "русский")
    assert(fromText.languageSpec.language.contains(Language.Russian))

    assert(decode("""<name n="x">y</name>""").isLeft)
    assert(decode("""<name lang="en"/>""").isLeft)
    assert(decode("""<name lang="en" n="" transliterated="yes"/>""").toOption.get.name == "")

    val encoded = Name.codec.encode(fromN)(using ZioXml)
    assert(encoded.get("n").contains("English"))
    assert(encoded.get("lang").contains("en"))
  }

  test("Hebrew.numberToString") {
    def check(number: Int, string: String): Unit =
      assert(Language.Hebrew.numberToString(number) == string)
      assert(Language.Hebrew.numberFromString(string).contains(number))

    check(  0, "")
    check(  5, "ה")
    check( 10, "י")
    check( 15, "טו")
    check( 20, "כ")
    check(100, "ק")
    check(116, "קטז")
    check(119, "קיט")
    check(555, "תקנה")
    check(999, "תתקצט")
    check(6000, "ו׳")

    assert(Language.Hebrew.numberFromString("הה").isEmpty)
    assert(Language.Hebrew.numberFromString("ק׳").isEmpty)
  }
