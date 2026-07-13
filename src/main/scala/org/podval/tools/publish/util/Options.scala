package org.podval.tools.publish.util

final class Options(
  args: Array[String],
  environmentVariablesPrefix: String
):
  private val (optionArgs: List[String], positionalArgs: List[String]) = args.toList.partition(_.startsWith("--"))

  def positional(position: Int): String = positionalArgs(position)
  
  private val options: List[(String, String)] = optionArgs.map(_.substring(2)).map: string =>
    val eqIndex = string.indexOf('=')
    if eqIndex != -1
    then (string.substring(0, eqIndex), string.substring(eqIndex + 1))
    else (string, "true") // option without value is treated as a boolean option with value "true"

  def option(name: String, default: String): String = options
    .find(_._1 == name)
    .map(_._2)
    .orElse(sys.env.get(s"${environmentVariablesPrefix}_${name.toUpperCase.replaceAll("-", "_")}"))
    .getOrElse(default)

  def booleanOption(name: String): Boolean = option(name, "false").toBoolean
