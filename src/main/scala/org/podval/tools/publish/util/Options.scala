package org.podval.tools.publish.util

final class Options(
  args: Array[String],
  environmentVariablesPrefix: String
):
  private val (optionArgs: List[String], positionalArgs: List[String]) = args.toList.partition(_.startsWith("--"))

  def positional(position: Int): String = positionalArgs(position)
  
  private val options: List[(String, String)] = optionArgs.map(_.substring(2)).map: string =>
    val eqIndex = string.indexOf('=')
    (string.substring(0, eqIndex), string.substring(eqIndex + 1))

  def option(name: String, default: String): String = options
    .find(_._1 == name)
    .map(_._2)
    .orElse(sys.env.get(s"SITE_PUBLISHER_${name.toUpperCase.replaceAll("-", "_")}"))
    .getOrElse(default)

  def booleanOption(name: String): Boolean = option(name, "false").toBoolean
