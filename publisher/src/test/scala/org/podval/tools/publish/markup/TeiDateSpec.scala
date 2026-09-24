package org.podval.tools.publish.markup

import org.podval.metadata.Language
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
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
    assert(dumped.contains("""<span class="date-tip">"""), dumped)
    assert(!dumped.contains("""tei-class="date-tip""""), dumped)
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

  test("notBefore and notAfter day bounds use uncertainty headers") {
    val (dumped, reporter) = convert(
      """<date notBefore="1800-11-15" notAfter="1800-11-16">15 or 16</date>"""
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">Not before<"), dumped)
    assert(dumped.contains(">Not after<"), dumped)
    assert(dumped.contains("1800 November 15"), dumped)
    assert(dumped.contains("1800 November 16"), dumped)
    assert(!dumped.contains(">From<"), dumped)
    assert(!dumped.contains(">To<"), dumped)
  }

  test("notBefore and notAfter month bounds expand to first and last day") {
    val (dumped, reporter) = convert("""<date notBefore="1800-11" notAfter="1800-12">Nov or Dec</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">Not before<"), dumped)
    assert(dumped.contains(">Not after<"), dumped)
    assert(dumped.contains("1800 November 1"), dumped)
    assert(dumped.contains("1800 December 31"), dumped)
  }

  test("dotdot in when is InvalidDate and leaves the attribute") {
    val (dumped, reporter) = convert("""<date when="1800-11-15..16">15 or 16</date>""")
    assert(invalid(reporter) == Seq("`..` is not a W3C temporal value (when=1800-11-15..16)"))
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("when=\"1800-11-15..16\""), dumped)
    assert(dumped.contains(">15 or 16<"), dumped)
  }

  test("dotdot month in when is InvalidDate") {
    val (dumped, reporter) = convert("""<date when="1800-11..12">Nov or Dec</date>""")
    assert(invalid(reporter) == Seq("`..` is not a W3C temporal value (when=1800-11..12)"))
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("when=\"1800-11..12\""), dumped)
  }

  test("from only is a From column on the Gregorian default") {
    val (dumped, reporter) = convert("""<date from="1798-08-11">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">From<"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(!dumped.contains(">To<"), dumped)
    assert(!dumped.contains("Julian"), dumped)
  }

  test("to only is a To column") {
    val (dumped, reporter) = convert("""<date to="1798-08-11">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">To<"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(!dumped.contains(">From<"), dumped)
  }

  test("notBefore only is a Not before column") {
    val (dumped, reporter) = convert("""<date notBefore="1798-08-11">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">Not before<"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(!dumped.contains(">Not after<"), dumped)
  }

  test("notAfter only is a Not after column") {
    val (dumped, reporter) = convert("""<date notAfter="1798-08-11">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">Not after<"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(!dumped.contains(">Not before<"), dumped)
  }

  test("from month expands only the first day") {
    val (dumped, reporter) = convert("""<date from="1800-11">Nov</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("1800 November 1"), dumped)
    assert(!dumped.contains("December"), dumped)
    assert(!dumped.contains(">To<"), dumped)
  }

  test("from year expands only 1 January") {
    val (dumped, reporter) = convert("""<date from="1800">1800</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("1800 January 1"), dumped)
    assert(!dumped.contains("1800 December 31"), dumped)
    assert(dumped.contains(">From<"), dumped)
  }

  test("notAfter year is 31 December") {
    val (dumped, reporter) = convert("""<date notAfter="1800">1800</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("1800 December 31"), dumped)
    assert(!dumped.contains("1800 January 1"), dumped)
    assert(dumped.contains(">Not after<"), dumped)
    assert(!dumped.contains(">Not before<"), dumped)
  }

  test("from with calendar=#julian on a Gregorian default") {
    val (dumped, reporter) = convert("""<date from="1798-08-11" calendar="#julian">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">From<"), dumped)
    assert(dumped.contains("Julian"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(dumped.contains("1798 August 22"), dumped)
  }

  test("Russian Julian from keeps the From header") {
    val (dumped, reporter) = convert(
      """<date from="1798-08-11">x</date>""",
      language = Language.Russian,
      julian = true
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">From<"), dumped)
    assert(dumped.contains("1798 август 11"), dumped)
    assert(dumped.contains("1798 август 22"), dumped)
  }

  test("from and to are a From/To period") {
    val (dumped, reporter) = convert("""<date to="1863-06-01" from="1863-05-28">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">From<"), dumped)
    assert(dumped.contains(">To<"), dumped)
    assert(dumped.contains("1863 May 28"), dumped)
    assert(dumped.contains("1863 June 1"), dumped)
    assert(!dumped.contains("Not before"), dumped)
    assert(dumped.indexOf(">From<") < dumped.indexOf(">To<"), dumped)
    assert(dumped.indexOf("1863 May 28") < dumped.indexOf("1863 June 1"), dumped)
  }

  test("from and notAfter are those two headers only") {
    val (dumped, reporter) = convert("""<date from="1857-03-01" notAfter="1857-04-30">x</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">From<"), dumped)
    assert(dumped.contains(">Not after<"), dumped)
    assert(!dumped.contains(">To<"), dumped)
    assert(!dumped.contains(">Not before<"), dumped)
    assert(!dumped.contains(">Date<"), dumped)
    assert(dumped.contains("1857 March 1"), dumped)
    assert(dumped.contains("1857 April 30"), dumped)
  }

  test("equal notBefore and notAfter stay two columns") {
    val (dumped, reporter) = convert(
      """<date notBefore="1798-08-11" notAfter="1798-08-11">x</date>"""
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains(">Not before<"), dumped)
    assert(dumped.contains(">Not after<"), dumped)
    assert(!dumped.contains(">Date<"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
  }

  test("when plus from is InvalidDate and keeps both attributes") {
    val (dumped, reporter) = convert("""<date when="1798-08-11" from="1798-08-12">x</date>""")
    assert(invalid(reporter) == Seq(
      "The @when attribute cannot be used with @from (when=1798-08-11, from=1798-08-12)"
    ))
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("when=\"1798-08-11\""), dumped)
    assert(dumped.contains("from=\"1798-08-12\""), dumped)
  }

  test("from plus notBefore and to plus notAfter are InvalidDate") {
    val fromBefore = convert("""<date from="1798-08-11" notBefore="1798-08-12">x</date>""")
    assert(invalid(fromBefore._2) == Seq(
      "The @from and @notBefore attributes cannot be used together (from=1798-08-11, notBefore=1798-08-12)"
    ))
    assert(!fromBefore._1.contains("date-ref"), fromBefore._1)
    assert(fromBefore._1.contains("from=\"1798-08-11\""), fromBefore._1)
    assert(fromBefore._1.contains("notBefore=\"1798-08-12\""), fromBefore._1)
    val toAfter = convert("""<date to="1798-08-11" notAfter="1798-08-12">x</date>""")
    assert(invalid(toAfter._2) == Seq(
      "The @to and @notAfter attributes cannot be used together (to=1798-08-11, notAfter=1798-08-12)"
    ))
    assert(!toAfter._1.contains("date-ref"), toAfter._1)
    assert(toAfter._1.contains("to=\"1798-08-11\""), toAfter._1)
    assert(toAfter._1.contains("notAfter=\"1798-08-12\""), toAfter._1)
  }

  test("dotdot in from is InvalidDate") {
    val (dumped, reporter) = convert("""<date from="1800-11-15..16">x</date>""")
    assert(invalid(reporter) == Seq("`..` is not a W3C temporal value (from=1800-11-15..16)"))
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("from=\"1800-11-15..16\""), dumped)
  }

  test("whitespace-only from and no when is unchanged") {
    val (dumped, reporter) = convert("""<date from="   ">1994</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("1994"), dumped)
  }

  test("two bad ends are two InvalidDate errors and no table") {
    val (dumped, reporter) = convert("""<date from="foo" to="bar">x</date>""")
    assert(invalid(reporter) == Seq("Not a date (from=foo)", "Not a date (to=bar)"))
    assert(!dumped.contains("date-ref"), dumped)
  }

  test("missing when is left unchanged") {
    val (dumped, reporter) = convert("""<date>1994</date>""")
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("1994"), dumped)
  }

  test("too few dashes names the attribute once") {
    val (dumped, reporter) = convert("""<date from="1800--11">x</date>""")
    assert(invalid(reporter) == Seq("Too few dashes in 'from': 1800--11"))
    assert(!dumped.contains("date-ref"), dumped)
  }

  test("too many dashes names the attribute once") {
    val (dumped, reporter) = convert("""<date from="1800-11-15-16">x</date>""")
    assert(invalid(reporter) == Seq("Too many dashes in 'from': 1800-11-15-16"))
    assert(!dumped.contains("date-ref"), dumped)
  }

  test("trailing hyphen is too few dashes") {
    val (dumped, reporter) = convert("""<date from="1800-11-">x</date>""")
    assert(invalid(reporter) == Seq("Too few dashes in 'from': 1800-11-"))
    assert(!dumped.contains("date-ref"), dumped)
    val day: (String, Reporter) = convert("""<date when="1800-11-15-">x</date>""")
    assert(invalid(day._2) == Seq("Too few dashes in 'when': 1800-11-15-"))
    assert(!day._1.contains("date-ref"), day._1)
  }

  test("invalid when is reported and left unchanged") {
    val (dumped, reporter) = convert("""<date when="not-a-date">x</date>""")
    assert(invalid(reporter) == Seq("Not a date (when=not-a-date)"))
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("when=\"not-a-date\""), dumped)
    assert(dumped.contains(">x<"), dumped)
  }

  private def invalid(reporter: Reporter): Seq[String] =
    assert(reporter.errors.forall(_._1 eq PageError.InvalidDate), reporter.errors)
    reporter.errors.map(_._2)
