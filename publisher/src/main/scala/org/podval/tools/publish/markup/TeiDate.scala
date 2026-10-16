package org.podval.tools.publish.markup

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.{Gregorian, Julian}
import org.podval.metadata.Language
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

/** TEI `<date>` temporal attributes → hover table in Julian / Gregorian / Jewish.
  * Call after `Xml2Html`. The walk re-enters `span.date-tip`, and `Xml2Html.convert`
  * on that visit renames `class` to `tei-class`. */
object TeiDate:
  val julianName: String = "julian"
  val gregorianName: String = "gregorian"
  val julianCalendarRef: String = "#julian"

  object RefClass extends CssClass("date-ref")
  object TipClass extends CssClass("date-tip")
  private object Converted extends XmlAttribute("data-calendars")

  private val temporalNames: Seq[String] = Seq("when", "notBefore", "notAfter", "from", "to")

  // Start-like then end-like. `from`/`notBefore` and `to`/`notAfter` are mutually exclusive.
  private val endSpecs: Seq[(String, String, Boolean)] = Seq(
    ("from", "From", false),
    ("notBefore", "Not before", false),
    ("to", "To", true),
    ("notAfter", "Not after", true)
  )

  def defaultIsJulian(value: Option[String]): Boolean =
    value.map(_.trim).filter(_.nonEmpty) match
      case None => false
      case Some(name) if name == gregorianName => false
      case Some(name) if name == julianName => true
      case Some(name) =>
        throw IllegalArgumentException(
          s"Unknown tei-default-calendar '$name' (expected $julianName or $gregorianName)"
        )

  def convert(element: Xml.Element, errorReporter: PageErrorReporter): Xml.Element =
    // The wrapped `<date>` is visited again as a child of `span.date-ref`.
    if !element.isNamed("date") || element.get(Converted).isDefined then element
    else
      val values: Map[String, String] = temporalValues(element)
      if values.isEmpty then element
      else
        val conflicts: Seq[String] = schematronErrors(values)
        if conflicts.nonEmpty then reject(element, errorReporter, conflicts)
        else
          val useJulian: Boolean = sourceIsJulian(element, errorReporter)
          given Language.Spec = errorReporter.languageSpec
          hover(values, useJulian) match
            case Right(table) => wrap(element, table, errorReporter)
            case Left(messages) => reject(element, errorReporter, messages)

  private def reject(
    element: Xml.Element,
    errorReporter: PageErrorReporter,
    messages: Seq[String]
  ): Xml.Element =
    messages.foreach(message => errorReporter.error(PageError.InvalidDate, message))
    element

  private def temporalValues(element: Xml.Element): Map[String, String] =
    Map.from(temporalNames.flatMap: name =>
      element.get(name).map(_.trim).filter(_.nonEmpty).map(value => name -> value)
    )

  private def sourceIsJulian(element: Xml.Element, errorReporter: PageErrorReporter): Boolean =
    element.get("calendar").map(_.trim).filter(_.nonEmpty) match
      case Some(calendar) => calendar == julianCalendarRef
      case None => errorReporter.teiDefaultCalendarIsJulian

  private def schematronErrors(values: Map[String, String]): Seq[String] =
    val endOrder: Seq[String] = Seq("notBefore", "notAfter", "from", "to")
    val withWhen: Seq[String] = endOrder.filter(values.contains)
    val whenError: Seq[String] = values.get("when").filter(_ => withWhen.nonEmpty).toSeq.map: when =>
      val names: String = withWhen.map(name => s"@$name").mkString(", ")
      val shown: String = ("when" +: withWhen).map(name => s"$name=${values(name)}").mkString(", ")
      s"The @when attribute cannot be used with $names ($shown)"
    val fromError: Seq[String] = (values.get("from"), values.get("notBefore")) match
      case (Some(from), Some(notBefore)) =>
        Seq(s"The @from and @notBefore attributes cannot be used together (from=$from, notBefore=$notBefore)")
      case _ => Seq.empty
    val toError: Seq[String] = (values.get("to"), values.get("notAfter")) match
      case (Some(to), Some(notAfter)) =>
        Seq(s"The @to and @notAfter attributes cannot be used together (to=$to, notAfter=$notAfter)")
      case _ => Seq.empty
    whenError ++ fromError ++ toError

  private def wrap(date: Xml.Element, table: Xml.Element, reporter: PageErrorReporter): Xml.Element =
    var tip: Xml.Element = Xml
      .element(XmlElement.Span)
      .add(TipClass)
      .setChildren(Seq(table: Xml.Node))
    reporter.interfaceChrome.foreach: pair =>
      val (code, dir) = pair
      tip = tip.set(XmlAttribute.Lang, code).set("dir", dir)
    var value: Xml.Element = date.set(Converted, "true")
    date.getId.foreach: id =>
      val tipId: String = s"$id-tip"
      tip = tip.setId(tipId).set(XmlAttribute.Role, "tooltip")
      value = value.set("aria-describedby", tipId)
    Xml
      .element(XmlElement.Span)
      .add(RefClass)
      .setChildren(Seq(value: Xml.Node, tip: Xml.Node))

  private final case class Bound(
    header: String,
    name: String,
    raw: String,
    numbers: Seq[Int],
    last: Boolean
  )

  private final case class Column(julian: Option[String], gregorian: String, jewish: String)

  private def hover(
    values: Map[String, String],
    useJulian: Boolean
  )(using Language.Spec): Either[Seq[String], Xml.Element] =
    values.get("when") match
      case Some(raw) =>
        parseValue("when", raw) match
          case Left(message) => Left(Seq(message))
          case Right(numbers) =>
            val bounds: Seq[Bound] = numbers match
              case Seq(_, _, _) => Seq(Bound("Date", "when", raw, numbers, last = false))
              case _ => Seq(
                Bound("From", "when", raw, numbers, last = false),
                Bound("To", "when", raw, numbers, last = true)
              )
            endsColumns(bounds, useJulian)
      case None =>
        val parsed: Seq[Either[String, Bound]] = endSpecs.flatMap: (name, header, last) =>
          values.get(name).map: raw =>
            parseValue(name, raw).map(numbers => Bound(header, name, raw, numbers, last))
        val parseErrors: Seq[String] = parsed.collect { case Left(message) => message }
        val bounds: Seq[Bound] = parsed.collect { case Right(bound) => bound }
        if bounds.isEmpty then Left(parseErrors)
        else endsColumns(bounds, useJulian) match
          case Left(calendarErrors) => Left(parseErrors ++ calendarErrors)
          case Right(_) if parseErrors.nonEmpty => Left(parseErrors)
          case Right(table) => Right(table)

  private def endsColumns(
    bounds: Seq[Bound],
    useJulian: Boolean
  )(using Language.Spec): Either[Seq[String], Xml.Element] =
    val loaded: Seq[Either[String, Column]] = bounds.map(bound => loadBound(bound, useJulian))
    val errors: Seq[String] = loaded.collect { case Left(message) => message }.distinct
    if errors.nonEmpty then Left(errors)
    else
      val columns: Seq[Column] = loaded.collect { case Right(column) => column }
      Right(endsTable(
        valueHeaders = bounds.map(_.header),
        julian = if useJulian then Some(columns.map(_.julian.get)) else None,
        gregorian = columns.map(_.gregorian),
        jewish = columns.map(_.jewish)
      ))

  private def loadBound(bound: Bound, useJulian: Boolean)(using Language.Spec): Either[String, Column] =
    val loaded: Either[String, Column] =
      if useJulian then
        catchBound(julianBound(bound.name, bound.numbers, bound.last)).map: day =>
          Column(Some(dayString(day)), dayString(day.to(Gregorian)), dayString(day.to(Jewish)))
      else
        catchBound(gregorianBound(bound.name, bound.numbers, bound.last)).map: day =>
          Column(None, dayString(day), dayString(day.to(Jewish)))
    loaded.left.map(message => s"$message (${bound.name}=${bound.raw})")

  private def julianBound(name: String, numbers: Seq[Int], last: Boolean): Julian.Day =
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
        throw IllegalArgumentException(s"Too few dashes in '$name': ${numbers.mkString("-")}")

  private def gregorianBound(name: String, numbers: Seq[Int], last: Boolean): Gregorian.Day =
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
        throw IllegalArgumentException(s"Too few dashes in '$name': ${numbers.mkString("-")}")

  private def catchBound[A](load: => A): Either[String, A] =
    try Right(load)
    catch case e: IllegalArgumentException => Left(e.getMessage)

  private def dayString(day: Language.ToString)(using spec: Language.Spec): String =
    day.toLanguageString

  private def endsTable(
    valueHeaders: Seq[String],
    julian: Option[Seq[String]],
    gregorian: Seq[String],
    jewish: Seq[String]
  ): Xml.Element =
    val julianRow: Option[Xml.Element] = julian.map(values => dataRow("Julian", values))
    table(Seq(headerRow("Calendar" +: valueHeaders)) ++ julianRow.toSeq ++ Seq(
      dataRow("Gregorian", gregorian),
      dataRow("Jewish", jewish)
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

  private def parseValue(name: String, raw: String): Either[String, Seq[Int]] =
    if raw.contains("..") then Left(s"`..` is not a W3C temporal value ($name=$raw)")
    else parseNumbers(name, raw) match
      case Left("Not a date") => Left(s"Not a date ($name=$raw)")
      case other => other

  private def parseNumbers(name: String, raw: String): Either[String, Seq[Int]] =
    // limit -1 keeps a trailing empty piece; the default split would drop it and accept `1800-11-`.
    val parts: Seq[String] = raw.split("-", -1).toSeq
    if parts.isEmpty || parts.exists(_.isEmpty) then Left(s"Too few dashes in '$name': $raw")
    else if parts.length > 3 then Left(s"Too many dashes in '$name': $raw")
    else
      try Right(parts.map(_.toInt))
      catch case _: NumberFormatException => Left("Not a date")

  /** One place a date occupies in a year. Month `0` means the date names no month.
    * `groupsDay` is true only for one exact day, so a range does not join that day's line. */
  final case class Slot(month: Int, day: Int, groupsDay: Boolean) derives CanEqual

  final case class Filing(
    years: Seq[Int],
    label: String,
    slotsIn: Int => Seq[Slot]
  ):
    def sortIn(year: Int): (Int, Int) =
      slotsIn(year).headOption.fold((0, 0))(slot => (slot.month, slot.day))

  def emptyFiling: Filing = undated("")

  def filingYears(date: Xml.Element): Filing =
    val values: Map[String, String] = temporalValues(date)
    val prose: String = PlainText.visible(date, skipNotes = true, skipTips = false)
    if values.isEmpty || schematronErrors(values).nonEmpty then undated(prose)
    else values.get("when") match
      case Some(raw) => whenFiling(raw, prose)
      case None => endsFiling(values, prose)

  private def undated(label: String): Filing =
    Filing(Seq.empty, label, _ => Seq(Slot(0, 0, false)))

  private def whenFiling(raw: String, prose: String): Filing =
    parseValue("when", raw) match
      case Left(_) => undated(prose)
      case Right(numbers) => oneYear(numbers, raw, prose, groupsDay = numbers.length == 3)

  private def endsFiling(values: Map[String, String], prose: String): Filing =
    val startName: String = if values.contains("from") then "from" else "notBefore"
    val endName: String = if values.contains("to") then "to" else "notAfter"
    (bound(values, startName), bound(values, endName)) match
      case (Some(Left(_)), _) | (_, Some(Left(_))) => undated(prose)
      case (Some(Right((raw, numbers))), None) => oneYear(numbers, raw, prose, groupsDay = false)
      case (None, Some(Right((raw, numbers)))) => oneYear(numbers, raw, prose, groupsDay = false)
      case (Some(Right(start)), Some(Right(end))) => bothEnds(start, end, prose)
      case _ => undated(prose)

  private def bound(
    values: Map[String, String],
    name: String
  ): Option[Either[String, (String, Seq[Int])]] =
    values.get(name).map(raw => parseValue(name, raw).map(numbers => raw -> numbers))

  private def oneYear(numbers: Seq[Int], raw: String, prose: String, groupsDay: Boolean): Filing =
    Filing(
      Seq(numbers.head),
      if prose.nonEmpty then prose else raw,
      _ => Seq(slotOf(numbers, groupsDay))
    )

  private def bothEnds(
    start: (String, Seq[Int]),
    end: (String, Seq[Int]),
    prose: String
  ): Filing =
    val (aRaw, aNumbers) = start
    val (bRaw, bNumbers) = end
    val a: Int = aNumbers.head
    val b: Int = bNumbers.head
    // U+2013 between the attribute strings. U+2014 between prose and that range.
    val label: String =
      if a == b then
        if prose.nonEmpty then prose else rangeLabel(aRaw, bRaw)
      else if prose.nonEmpty then s"$prose \u2014 ${rangeLabel(aRaw, bRaw)}"
      else rangeLabel(aRaw, bRaw)
    val slots: Int => Seq[Slot] =
      if b == a then _ => sameYearSlots(aNumbers, bNumbers)
      else if b == a + 1 then year => Seq(slotOf(if year == b then bNumbers else aNumbers, groupsDay = false))
      else _ => Seq(slotOf(aNumbers, groupsDay = false))
    if b < a then undated(label)
    else if b == a + 1 then Filing(Seq(a, b), label, slots)
    else Filing(Seq(a), label, slots)

  private def rangeLabel(aRaw: String, bRaw: String): String =
    s"$aRaw \u2013 $bRaw"

  private def slotOf(numbers: Seq[Int], groupsDay: Boolean): Slot =
    numbers.drop(1) match
      case Seq(month, day) => Slot(month, day, groupsDay)
      case Seq(month) => Slot(month, 0, false)
      case _ => Slot(0, 0, false)

  /** Both ends name a month: one slot per month from the first through the last.
    * The first slot keeps the old sort position. A range joins a day line only when both ends are that day. */
  private def sameYearSlots(start: Seq[Int], end: Seq[Int]): Seq[Slot] =
    (start.lift(1), end.lift(1)) match
      case (Some(fromMonth), Some(toMonth)) if toMonth >= fromMonth =>
        val startDay: Int = start.lift(2).getOrElse(0)
        val endDay: Int = end.lift(2).getOrElse(0)
        val sameDay: Boolean =
          fromMonth == toMonth && start.length >= 3 && end.length >= 3 && startDay == endDay
        (fromMonth to toMonth).toSeq.map: month =>
          val day: Int =
            if month == fromMonth then startDay
            else if month == toMonth then endDay
            else 0
          Slot(month, day, sameDay && month == fromMonth)
      case _ =>
        Seq(slotOf(start, groupsDay = false))
