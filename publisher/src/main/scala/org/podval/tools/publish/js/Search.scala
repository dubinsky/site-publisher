package org.podval.tools.publish.js

object Search extends JSLibrary:
  val scriptPath: String = "/assets/js/search.js"

  override def isModule: Boolean = true

  override def cdn: String = ""

  override def inlineJs: Some[String] = Some:
    val script: String = Js.quote(scriptPath)
    s"""import { run } from $script;
       |run();
       |""".stripMargin
