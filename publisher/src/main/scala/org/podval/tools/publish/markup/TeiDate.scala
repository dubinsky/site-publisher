package org.podval.tools.publish.markup

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.{Gregorian, Julian}
import org.podval.metadata.Language
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

/** TEI `<date when>` (and `..` ranges) → hover table in Julian / Gregorian / Jewish. */
object TeiDate:
  val julianName: String = "julian"
  val gregorianName: String = "gregorian"
  val julianCalendarRef: String = "#julian"

  object RefClass extends CssClass("date-ref")
  object TipClass extends CssClass("date-tip")
  private object Converted extends XmlAttribute("data-calendars")

  def defaultIsJulian(value: Option[String]): Boolean =
    value.map(_.trim).filter(_.nonEmpty) match
      case None => false
      case Some(name) if name == gregorianName => false
      case Some(name) if name == julianName => true
      case Some(name) =>
        throw IllegalArgumentException(
          s"Unknown tei-default-calendar '$name' (expected $julianName or $gregorianName)"
        )

  // TODO look into TEI `dateRange` @from/@to
  def convert(element: Xml.Element, errorReporter: PageErrorReporter): Xml.Element =
    if !element.isNamed("date") || element.get(Converted).isDefined then element
    else
      val when: Option[String] = element.get("when").map(_.trim).filter(_.nonEmpty)
      when.fold(element): value =>
        val useJulian: Boolean =
          element.get("calendar").map(_.trim).filter(_.nonEmpty) match
            case Some(calendar) => calendar == julianCalendarRef
            case None => errorReporter.teiDefaultCalendarIsJulian
        given Language.Spec = errorReporter.languageSpec
        tooltipTable(value, useJulian) match
          case Right(table) => wrap(element, table)
          case Left(message) =>
            errorReporter.error(PageError.InvalidDate, s"$message (when=$value)")
            element

  private def wrap(date: Xml.Element, table: Xml.Element): Xml.Element =
    var tip: Xml.Element = Xml
      .element(XmlElement.Span)
      .add(TipClass)
      .setChildren(Seq(table: Xml.Node))
    var value: Xml.Element = date.set(Converted, "true")
    date.getId.foreach: id =>
      val tipId: String = s"$id-tip"
      tip = tip.setId(tipId).set(XmlAttribute.Role, "tooltip")
      value = value.set("aria-describedby", tipId)
    Xml
      .element(XmlElement.Span)
      .add(RefClass)
      .setChildren(Seq(value: Xml.Node, tip: Xml.Node))

  private enum Parsed derives CanEqual:
    case Point(year: Int, month: Int, day: Int)
    case Range(from: Seq[Int], to: Seq[Int])

  private def tooltipTable(when: String, useJulian: Boolean)(using spec: Language.Spec): Either[String, Xml.Element] =
    parsedWhen(when).flatMap: parsed =>
      if useJulian then julianTable(parsed) else gregorianTable(parsed)

  private def julianTable(parsed: Parsed)(using spec: Language.Spec): Either[String, Xml.Element] =
    parsed match
      case Parsed.Point(year, month, day) =>
        catchBound(Julian.Year(year).month(month).day(day)).map: d =>
          pointTable(
            julian = Some(dayString(d)),
            gregorian = dayString(d.to(Gregorian)),
            jewish = dayString(d.to(Jewish))
          )
      case Parsed.Range(from, to) =>
        for
          fromDay <- catchBound(julianBound(from, last = false))
          toDay <- catchBound(julianBound(to, last = true))
        yield intervalTable(
          julian = Some((dayString(fromDay), dayString(toDay))),
          gregorian = (dayString(fromDay.to(Gregorian)), dayString(toDay.to(Gregorian))),
          jewish = (dayString(fromDay.to(Jewish)), dayString(toDay.to(Jewish)))
        )

  private def gregorianTable(parsed: Parsed)(using spec: Language.Spec): Either[String, Xml.Element] =
    parsed match
      case Parsed.Point(year, month, day) =>
        catchBound(Gregorian.Year(year).month(month).day(day)).map: d =>
          pointTable(
            julian = None,
            gregorian = dayString(d),
            jewish = dayString(d.to(Jewish))
          )
      case Parsed.Range(from, to) =>
        for
          fromDay <- catchBound(gregorianBound(from, last = false))
          toDay <- catchBound(gregorianBound(to, last = true))
        yield intervalTable(
          julian = None,
          gregorian = (dayString(fromDay), dayString(toDay)),
          jewish = (dayString(fromDay.to(Jewish)), dayString(toDay.to(Jewish)))
        )

  private def julianBound(numbers: Seq[Int], last: Boolean): Julian.Day =
    numbers match
      case Seq(year) =>
        val y: Julian.Year = Julian.Year(year)
        if last then y.lastDay else y.firstDay
      case Seq(year, month) =>
        val m: Julian.Month = Julian.Year(year).month(month)
        if last then m.lastDay else m.firstDay
      case Seq(year, month, day) =>
        Julian.Year(year).month(month).day(day)
      case _ =>
        throw IllegalArgumentException(s"Too few dashes in 'when': ${numbers.mkString("-")}")

  private def gregorianBound(numbers: Seq[Int], last: Boolean): Gregorian.Day =
    numbers match
      case Seq(year) =>
        val y: Gregorian.Year = Gregorian.Year(year)
        if last then y.lastDay else y.firstDay
      case Seq(year, month) =>
        val m: Gregorian.Month = Gregorian.Year(year).month(month)
        if last then m.lastDay else m.firstDay
      case Seq(year, month, day) =>
        Gregorian.Year(year).month(month).day(day)
      case _ =>
        throw IllegalArgumentException(s"Too few dashes in 'when': ${numbers.mkString("-")}")

  private def catchBound[A](load: => A): Either[String, A] =
    try Right(load)
    catch case e: IllegalArgumentException => Left(e.getMessage)

  private def dayString(day: Language.ToString)(using spec: Language.Spec): String =
    day.toLanguageString

  private def pointTable(julian: Option[String], gregorian: String, jewish: String): Xml.Element =
    val julianRow: Option[Xml.Element] = julian.map(value => dataRow("Julian", Seq(value)))
    table(Seq(headerRow(Seq("Calendar", "Date"))) ++ julianRow.toSeq ++ Seq(
      dataRow("Gregorian", Seq(gregorian)),
      dataRow("Jewish", Seq(jewish))
    ))

  private def intervalTable(
    julian: Option[(String, String)],
    gregorian: (String, String),
    jewish: (String, String)
  ): Xml.Element =
    val julianRow: Option[Xml.Element] = julian.map((from, to) => dataRow("Julian", Seq(from, to)))
    val (gregorianFrom, gregorianTo) = gregorian
    val (jewishFrom, jewishTo) = jewish
    table(Seq(headerRow(Seq("Calendar", "From", "To"))) ++ julianRow.toSeq ++ Seq(
      dataRow("Gregorian", Seq(gregorianFrom, gregorianTo)),
      dataRow("Jewish", Seq(jewishFrom, jewishTo))
    ))

  private def table(rows: Seq[Xml.Element]): Xml.Element =
    Xml.element(XmlElement.Table).setChildren(rows.map(el => el: Xml.Node))

  private def headerRow(labels: Seq[String]): Xml.Element =
    Xml.element(XmlElement.Tr).setChildren(
      labels.map(label => Xml.element(XmlElement.Th).setText(label): Xml.Node)
    )

  private def dataRow(calendar: String, values: Seq[String]): Xml.Element =
    Xml.element(XmlElement.Tr).setChildren(
      (calendar +: values).map(text => Xml.element(XmlElement.Td).setText(text): Xml.Node)
    )

  private def parsedWhen(when: String): Either[String, Parsed] =
    if when.contains("..") then
      when.split("\\.\\.", 2).toSeq match
        case Seq(fromPart, toPart) if fromPart.nonEmpty && toPart.nonEmpty =>
          for
            fromNumbers <- parseNumbers(fromPart)
            toNumbers <- parseNumbers(toPart)
            _ <- Either.cond(
              toNumbers.length <= fromNumbers.length,
              (),
              s"Too many dashes in the 'to': $when"
            )
          yield Parsed.Range(fromNumbers, fromNumbers.dropRight(toNumbers.length) ++ toNumbers)
        case _ =>
          Left(s"Bad explicit interval: $when")
    else
      parseNumbers(when).flatMap:
        case Seq(year, month, day) => Right(Parsed.Point(year, month, day))
        case numbers => Right(Parsed.Range(numbers, numbers))

  private def parseNumbers(when: String): Either[String, Seq[Int]] =
    val parts: Seq[String] = when.split("-").toSeq
    if parts.isEmpty || parts.exists(_.isEmpty) then Left(s"Too few dashes in 'when': $when")
    else if parts.length > 3 then Left(s"Too many dashes in 'when': $when")
    else
      try Right(parts.map(_.toInt))
      catch case _: NumberFormatException => Left(s"Not a date: $when")
