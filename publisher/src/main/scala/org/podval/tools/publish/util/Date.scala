package org.podval.tools.publish.util

import zio.blocks.schema.yaml.{Yaml, YamlCodec}
import java.time.{LocalDate, LocalDateTime, OffsetDateTime, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeParseException}

sealed trait Date:
  def localDate: LocalDate
  def toString: String
  def toShortString: String = localDate.format(Date.shortFormat)

object Date:
  given Ordering[Date] = Ordering.by(_.localDate)

  private val shortFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("LLL d, yyyy")

  final class Local(val value: LocalDate) extends Date:
    override def localDate: LocalDate = value
    override def toString: String = value.toString

  final class LocalTime(val value: LocalDateTime) extends Date:
    override def localDate: LocalDate = value.toLocalDate
    override def toString: String = value.toString

  final class OffsetTime(val value: OffsetDateTime) extends Date:
    override def localDate: LocalDate = value.toLocalDate
    override def toString: String = value.toString

  /** RFC 3339 instant in `zone`. An `OffsetTime` keeps the offset it was written with. */
  def toOffsetDateTime(date: Date, zone: ZoneId): OffsetDateTime = date match
    case offset: OffsetTime => offset.value
    case localTime: LocalTime => localTime.value.atZone(zone).toOffsetDateTime
    case local: Local =>
      local.value.atTime(java.time.LocalTime.MIDNIGHT).atZone(zone).toOffsetDateTime

  def codec: YamlCodec[Date] = new YamlCodec[Date]:
    def encodeValue(date: Date): Yaml = Yaml.Scalar(date.toString)

    def decodeValue(yaml: Yaml): Date = yaml match
      case Yaml.Scalar(value, _) => decodeLocalDateUnsafe(value.trim)
      case _ => error("Expected scalar value")

    private def decodeLocalDateUnsafe(value: String): Date =
      try Local(LocalDate.parse(value)) // 2026-03-29
      catch case e: DateTimeParseException =>
        try LocalTime(LocalDateTime.parse(value)) // 2010-01-28T14:24:00
        catch case e: DateTimeParseException =>
          try OffsetTime(OffsetDateTime.parse(value)) //2010-01-28T14:24:00.004-05:00
          catch case e: DateTimeParseException => throw IllegalArgumentException(s"Not a date: $value ${e.getMessage}")
