package org.podval.tools.publish.util

final class IdGenerator(prefix: String):
  private var number: Int = 0

  def generate(): String =
    number = number + 1
    s"$prefix$number"

object IdGenerator:
  def toId(text: String): String = text.trim.replace(' ', '-')
