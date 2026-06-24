package org.podval.tools.publish.util

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{LoggerContext, Level as LogBackLevel, Logger as LogBackLogger}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.status.InfoStatus
import net.logstash.logback.encoder.LogstashEncoder
import org.slf4j.event.Level
import org.slf4j.{Logger, LoggerFactory}
import scala.jdk.CollectionConverters.SeqHasAsJava

object Logging:
  def configureLogBack(
    level: String,
    useLogStash: Boolean
  ): Unit = LoggerFactory.getILoggerFactory match
    case loggerContext: LoggerContext => configureLogback(
      loggerContext,
      Level.valueOf(level.toUpperCase),
      useLogStash
    )
    case _ =>

  private def configureLogback(
    loggerContext: LoggerContext,
    level: Level,
    useLogStash: Boolean
  ): Unit =
    val statusManager = loggerContext.getStatusManager
    if statusManager != null then statusManager.add(InfoStatus("Configuring logger", loggerContext))
    loggerContext.reset()

    if !useLogStash then
      val encoder = new PatternLayoutEncoder
      // Simplify default pattern
      encoder.setPattern("[%-5level] %msg%n")
      useEncoder(loggerContext, encoder, "simple")
    else
      val encoder = new LogstashEncoder
      // Ignore default logging fields
      encoder.setExcludeMdcKeyNames(List("timestamp", "version", "logger", "thread", "level", "levelValue").asJava)
      useEncoder(loggerContext, encoder, "json")

    val rootLogger: LogBackLogger = loggerContext.getLogger(Logger.ROOT_LOGGER_NAME)

    given CanEqual[Level, Level] = CanEqual.derived

    rootLogger.setLevel(level match
      case Level.ERROR => LogBackLevel.ERROR
      case Level.WARN  => LogBackLevel.WARN
      case Level.INFO  => LogBackLevel.INFO
      case Level.DEBUG => LogBackLevel.DEBUG
      case Level.TRACE => LogBackLevel.TRACE
    )

  // Note: attempt to remove duplication by returning the encoder from the if expression
  // run into `java.lang.ClassNotFoundException: net.logstash.logback.encoder.LogstashEncoder`...
  private def useEncoder(
    loggerContext: LoggerContext,
    encoder: Encoder[ILoggingEvent],
    appenderName: String
  ): Unit =
    encoder.setContext(loggerContext)
    encoder.start()

    val appender = new ConsoleAppender[ILoggingEvent]
    appender.setName(s"${appenderName}ConsoleAppender")
    appender.setContext(loggerContext)
    appender.setEncoder(encoder)
    appender.start()

    val rootLogger: LogBackLogger = loggerContext.getLogger(Logger.ROOT_LOGGER_NAME)
    rootLogger.addAppender(appender)
