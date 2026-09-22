package org.podval.tools.publish.markup

import org.podval.metadata.Language
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import Xml.given
import org.scalatest.funsuite.AnyFunSuite

final class TeiDateSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def render(element: Xml.Element): String =
    HtmlXmlWriterConfig.render(element).replaceAll("\\s+", " ").replace("= ", "=")

  private final class Reporter(
    language: Language = Language.English,
    julian: Boolean = false
  ) extends PageErrorReporter:
    var errors: Seq[(PageError.Kind, String)] = Seq.empty
    override def languageSpec: Language.Spec = language.toSpec
    override def teiDefaultCalendarIsJulian: Boolean = julian
    override def error(
      kind: PageError.Kind,
      message: String,
      cause: Option[Throwable] = None
    ): Unit =
      errors = errors :+ (kind -> message)

  private def convert(
    xml: String,
    language: Language = Language.English,
    julian: Boolean = false
  ): (String, Reporter) =
    val reporter: Reporter = Reporter(language, julian)
    (render(TeiDate.convert(parse(xml), reporter)), reporter)

  test("defaultIsJulian: omitted and gregorian are false; julian is true") {
    assert(!TeiDate.defaultIsJulian(None))
    assert(!TeiDate.defaultIsJulian(Some("")))
    assert(!TeiDate.defaultIsJulian(Some("gregorian")))
    assert(TeiDate.defaultIsJulian(Some("julian")))
  }

  test("defaultIsJulian rejects unknown values") {
    val thrown: IllegalArgumentException =
      intercept[IllegalArgumentException](TeiDate.defaultIsJulian(Some("hebrew")))
    assert(thrown.getMessage.contains("hebrew"), thrown.getMessage)
  }

  test("point date with Gregorian default has no Julian row") {
    val (dumped, reporter) = convert("""<date when="1798-08-11">11 августа</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("""class="date-ref""""), dumped)
    assert(dumped.contains("""class="date-tip""""), dumped)
    assert(dumped.contains("11 августа"), dumped)
    assert(dumped.contains("Gregorian"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(dumped.contains("Jewish"), dumped)
    assert(!dumped.contains("Julian"), dumped)
  }

  test("Julian default and Russian names match collector for 1798-08-11") {
    val (dumped, reporter) = convert(
      """<date when="1798-08-11">11 августа</date>""",
      language = Language.Russian,
      julian = true
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("Julian"), dumped)
    assert(dumped.contains("1798 август 11"), dumped)
    assert(dumped.contains("1798 август 22"), dumped)
    assert(dumped.contains("5558 Элул 10"), dumped)
  }

  test("calendar=#julian overrides a Gregorian site default") {
    val (dumped, reporter) = convert("""<date when="1798-08-11" calendar="#julian">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("Julian"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(dumped.contains("1798 August 22"), dumped)
  }

  test("year-only when is a From/To interval") {
    val (dumped, reporter) = convert("""<date when="1800">1800</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("From"), dumped)
    assert(dumped.contains("To"), dumped)
    assert(dumped.contains("1800 January 1"), dumped)
    assert(dumped.contains("1800 December 31"), dumped)
  }

  test("month-only when is that month's first and last day") {
    val (dumped, reporter) = convert("""<date when="1800-08">August 1800</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("1800 August 1"), dumped)
    assert(dumped.contains("1800 August 31"), dumped)
  }

  test("explicit day range 15..16") {
    val (dumped, reporter) = convert("""<date when="1800-11-15..16">15 or 16</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("1800 November 15"), dumped)
    assert(dumped.contains("1800 November 16"), dumped)
  }

  test("explicit month range 11..12") {
    val (dumped, reporter) = convert("""<date when="1800-11..12">Nov or Dec</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("1800 November 1"), dumped)
    assert(dumped.contains("1800 December 31"), dumped)
  }

  test("missing when is left unchanged") {
    val (dumped, reporter) = convert("""<date>1994</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("1994"), dumped)
  }

  test("invalid when is reported and left unchanged") {
    val (dumped, reporter) = convert("""<date when="not-a-date">x</date>""")
    assert(reporter.errors.exists(_._1 eq PageError.InvalidDate), reporter.errors)
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("when=\"not-a-date\""), dumped)
    assert(dumped.contains(">x<"), dumped)
  }
