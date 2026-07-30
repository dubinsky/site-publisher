package org.podval.tools.publish.util

import org.podval.tools.publish.util.IdGenerator.Prefixed

final class IdGenerator:
  private val generals: Prefixed = Prefixed("_generated_id")
  def general(): String = generals.generate()

  private val footnoteCorrelationIds: Prefixed = Prefixed("")
  def footnoteCorrelationId(): String = footnoteCorrelationIds.generate()
  
  private val footnoteNumbers: Prefixed = Prefixed("")
  def footnoteNumber(): String = footnoteNumbers.generate()

object IdGenerator:
  private final class Prefixed(prefix: String):
    private var number: Int = 0

    def generate(): String =
      number = number + 1
      s"$prefix$number"
