package org.podval.tools.publish.js

object Mermaid extends JSLibrary:
  val version: String = "11.13.0"
  
  override def isModule: Boolean = true

  override def inlineJs: Some[String] = Some:
    val mermaid: String = Js.quote(s"$cdn/mermaid.esm.min.mjs")
    s"""import mermaid from $mermaid;
       |mermaid.initialize({ startOnLoad: false });
       |await mermaid.run({ querySelector: '.language-mermaid', });
       |""".stripMargin

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}mermaid/$version",
    s"${JSLibrary.jsDelivr}mermaid@$version/dist"
  )
