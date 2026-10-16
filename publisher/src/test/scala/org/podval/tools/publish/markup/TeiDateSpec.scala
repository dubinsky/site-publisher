package org.podval.tools.publish.markup

import org.podval.metadata.Language
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

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
    assert(dumped.contains("Юлианский"), dumped)
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
    assert(dumped.contains(">С<"), dumped)
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

  private def file(xml: String): TeiDate.Filing =
    TeiDate.filingYears(parse(xml))

  private def enRange(start: String, end: String): String =
    s"$start \u2013 $end"

  private def proseRange(prose: String, start: String, end: String): String =
    s"$prose \u2014 ${enRange(start, end)}"

  test("when with child text files that year and the prose") {
    val filed: TeiDate.Filing = file("""<date when="1798-12-20">20 декабря 1798</date>""")
    assert(filed.years == Seq(1798))
    assert(filed.label == "20 декабря 1798")
    assert(filed.sortIn(1798) == (12, 20))
    assert(filed.sortIn(1801) == (12, 20))
  }

  test("when with no child text uses the raw attribute") {
    val filed: TeiDate.Filing = file("""<date when="1798-12-20"/>""")
    assert(filed.years == Seq(1798))
    assert(filed.label == "1798-12-20")
    assert(filed.sortIn(1798) == (12, 20))
  }

  test("when 1800-12-22 stays in 1800") {
    val filed: TeiDate.Filing = file("""<date when="1800-12-22" calendar="#julian"/>""")
    assert(filed.years == Seq(1800))
    assert(filed.label == "1800-12-22")
    assert(filed.sortIn(1801) == (12, 22))
  }

  test("prose with no temporal attribute is undated") {
    val filed: TeiDate.Filing = file("""<date>около 1800</date>""")
    assert(filed.years.isEmpty)
    assert(filed.label == "около 1800")
    val empty: TeiDate.Filing = file("""<date/>""")
    assert(empty.years.isEmpty)
    assert(empty.label.isEmpty)
  }

  test("same-year day range with prose keeps the prose only") {
    val filed: TeiDate.Filing = file(
      """<date notBefore="1800-11-15" notAfter="1800-11-16">15 или 16 ноября 1800</date>"""
    )
    assert(filed.years == Seq(1800))
    assert(filed.label == "15 или 16 ноября 1800")
    assert(!filed.label.contains("1800-11"))
    assert(filed.sortIn(1800) == (11, 15))
  }

  test("same-year day range with no prose joins the raw attributes") {
    val filed: TeiDate.Filing = file("""<date notBefore="1800-11-15" notAfter="1800-11-16"/>""")
    assert(filed.years == Seq(1800))
    assert(filed.label == enRange("1800-11-15", "1800-11-16"))
    assert(filed.sortIn(1800) == (11, 15))
  }

  test("same-year month range with no prose files that year") {
    val filed: TeiDate.Filing = file("""<date notBefore="1800-11" notAfter="1800-12"/>""")
    assert(filed.years == Seq(1800))
    assert(filed.label == enRange("1800-11", "1800-12"))
    assert(filed.sortIn(1800) == (11, 0))
    assert(filed.slotsIn(1800) == Seq(TeiDate.Slot(11, 0, false), TeiDate.Slot(12, 0, false)))
  }

  test("an exact day groups and a same-month day range does not") {
    val exact: TeiDate.Filing = file("""<date when="1800-11-15"/>""")
    assert(exact.slotsIn(1800) == Seq(TeiDate.Slot(11, 15, true)))
    val range: TeiDate.Filing = file("""<date notBefore="1800-11-15" notAfter="1800-11-16"/>""")
    assert(range.slotsIn(1800) == Seq(TeiDate.Slot(11, 15, false)))
    assert(range.sortIn(1800) == (11, 15))
  }

  test("adjacent years file both and sort each bound") {
    val prose: String = "конец 1798"
    val filed: TeiDate.Filing = file(
      s"""<date notBefore="1798-12-01" notAfter="1799-01-02">$prose</date>"""
    )
    assert(filed.years == Seq(1798, 1799))
    assert(filed.label == proseRange(prose, "1798-12-01", "1799-01-02"))
    assert(filed.sortIn(1798) == (12, 1))
    assert(filed.sortIn(1799) == (1, 2))
  }

  test("adjacent years with no prose join the raw attributes") {
    val filed: TeiDate.Filing = file("""<date notBefore="1798-12-01" notAfter="1799-01-02"/>""")
    assert(filed.years == Seq(1798, 1799))
    assert(filed.label == enRange("1798-12-01", "1799-01-02"))
    assert(filed.sortIn(1798) == (12, 1))
    assert(filed.sortIn(1799) == (1, 2))
  }

  test("from and to are the range ends") {
    val filed: TeiDate.Filing = file("""<date from="1798-12-01" to="1799-01-02"/>""")
    assert(filed.years == Seq(1798, 1799))
    assert(filed.label == enRange("1798-12-01", "1799-01-02"))
    assert(filed.sortIn(1798) == (12, 1))
    assert(filed.sortIn(1799) == (1, 2))
  }

  test("a span of two or more years files only the start year") {
    val filed: TeiDate.Filing = file("""<date notBefore="1790" notAfter="1800"/>""")
    assert(filed.years == Seq(1790))
    assert(filed.label.contains(enRange("1790", "1800")))
    assert(filed.label == enRange("1790", "1800"))
    assert(filed.sortIn(1790) == (0, 0))
  }

  test("one parsed end is that year") {
    val bare: TeiDate.Filing = file("""<date notBefore="1798-08"/>""")
    assert(bare.years == Seq(1798))
    assert(bare.label == "1798-08")
    assert(bare.sortIn(1798) == (8, 0))
    val withProse: TeiDate.Filing = file("""<date to="1799-01-02">после</date>""")
    assert(withProse.years == Seq(1799))
    assert(withProse.label == "после")
    assert(withProse.sortIn(1799) == (1, 2))
  }

  test("when together with an end is undated") {
    val withProse: TeiDate.Filing = file(
      """<date when="1798-12-20" notBefore="1798-12-01">20 декабря 1798</date>"""
    )
    assert(withProse.years.isEmpty)
    assert(withProse.label == "20 декабря 1798")
    val bare: TeiDate.Filing = file("""<date when="1798-12-20" notBefore="1798-12-01"/>""")
    assert(bare.years.isEmpty)
    assert(bare.label.isEmpty)
  }

  test("dotdot in when is undated and the label is the prose") {
    val withProse: TeiDate.Filing = file("""<date when="1800-11-..">около</date>""")
    assert(withProse.years.isEmpty)
    assert(withProse.label == "около")
    assert(!withProse.label.contains(".."))
    val bare: TeiDate.Filing = file("""<date when="1800-11-.."/>""")
    assert(bare.years.isEmpty)
    assert(bare.label.isEmpty)
  }

  test("an end that does not parse is undated and does not use the raw attribute") {
    val withProse: TeiDate.Filing = file(
      """<date notBefore="1800-11-15" notAfter="..">15 ноября</date>"""
    )
    assert(withProse.years.isEmpty)
    assert(withProse.label == "15 ноября")
    assert(!withProse.label.contains(".."))
    val bare: TeiDate.Filing = file("""<date notBefore=".." notAfter="1800"/>""")
    assert(bare.years.isEmpty)
    assert(bare.label.isEmpty)
  }

  test("an end year before the start year is undated and keeps the range") {
    val prose: String = "наоборот"
    val withProse: TeiDate.Filing = file(
      s"""<date notBefore="1801-02-03" notAfter="1799-11-01">$prose</date>"""
    )
    assert(withProse.years.isEmpty)
    assert(withProse.label == proseRange(prose, "1801-02-03", "1799-11-01"))
    val bare: TeiDate.Filing = file("""<date notBefore="1801-02-03" notAfter="1799-11-01"/>""")
    assert(bare.years.isEmpty)
    assert(bare.label == enRange("1801-02-03", "1799-11-01"))
  }

  test("a note child is not part of the filing label") {
    val filed: TeiDate.Filing = file(
      """<date when="1798-12-20">20 декабря <note>сноска</note> 1798</date>"""
    )
    assert(filed.years == Seq(1798))
    assert(filed.label == "20 декабря 1798")
    assert(!filed.label.contains("сноска"))
  }

  test("markdown date uses the site calendar and keeps the words") {
    val (dumped, reporter) = markdown(
      """<date when="1798-08-11">11 августа</date>""",
      Language.Russian,
      julian = true
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("date-ref"), dumped)
    assert(dumped.contains("11 августа"), dumped)
    assert(dumped.contains("Юлианский"), dumped)
    assert(dumped.contains("1798 август 11"), dumped)
    assert(dumped.contains("1798 август 22"), dumped)
    assert(dumped.contains("5558 Элул 10"), dumped)
  }

  test("markdown notBefore and notAfter use the uncertainty headers") {
    val (dumped, reporter) = markdown(
      """<date notBefore="1800-11-15" notAfter="1800-11-16">15 или 16 ноября</date>""",
      Language.Russian,
      julian = true
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(dumped.contains("Не раньше"), dumped)
    assert(dumped.contains("Не позже"), dumped)
    assert(dumped.contains("15 или 16 ноября"), dumped)
  }

  test("markdown heading date keeps the words as the section id") {
    val (xml, reporter) = markdownXml(
      """### <date when="1798-05-08">8 мая</date>""",
      Language.Russian,
      julian = true
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    val normalized: Xml.Element = xml.transform(Section.normalize(_, IdGenerator("_id")))
    val dumped: String = render(normalized)
    assert(dumped.contains("""id="8-мая""""), dumped)
    assert(dumped.contains("Юлианский"), dumped)
    val section: Xml.Element = normalized.childElements.find(Section.is).get
    assert(Section.headingText(Section.heading(section).get) == "8 мая")
  }

  test("a date in a markdown fence is not a hover") {
    val (dumped, reporter) = markdown(
      """```
        |<date when="1798-08-11">11 августа</date>
        |```
        |""".stripMargin
    )
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains("1798-08-11"), dumped)
  }

  test("markdown invalid date is reported and left as written") {
    val (dumped, reporter) = markdown("""<date when="1798-13-40">x</date>""")
    assert(invalid(reporter).nonEmpty)
    assert(!dumped.contains("date-ref"), dumped)
    assert(dumped.contains(">x<"), dumped)
  }

  private def markdown(
    source: String,
    language: Language = Language.English,
    julian: Boolean = false
  ): (String, Reporter) =
    val (xml, reporter) = markdownXml(source, language, julian)
    (render(xml), reporter)

  private def markdownXml(
    source: String,
    language: Language,
    julian: Boolean
  ): (Xml.Element, Reporter) =
    val reporter: Reporter = Reporter(language, julian)
    val xml: Xml.Element = MarkdownMarkup.process(
      parse(MarkdownMarkup.xmlContent(source, File("t.md"))),
      reporter
    )._1
    (xml, reporter)

  test("visible skips notes and tip tokens when asked") {
    val element: Xml.Element = parse(
      """<date>a <note>n</note> <span class="x date-tip">Julian</span> <span tei-class="gloss-tip">tip</span> b</date>"""
    )
    assert(PlainText.visible(element, skipNotes = true, skipTips = false) == "a Julian tip b")
    assert(PlainText.visible(element, skipNotes = true, skipTips = true) == "a b")
    assert(PlainText.visible(element, skipNotes = false, skipTips = true) == "a n b")
  }

  private def invalid(reporter: Reporter): Seq[String] =
    assert(reporter.errors.forall(_._1 eq PageError.InvalidDate), reporter.errors)
    reporter.errors.map(_._2)
