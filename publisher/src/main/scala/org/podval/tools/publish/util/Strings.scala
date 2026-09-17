package org.podval.tools.publish.util

object Strings:
  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index + 1)))

  def splitFirst(what: String, on: Char): (String, Option[String]) = what.indexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index + 1)))
