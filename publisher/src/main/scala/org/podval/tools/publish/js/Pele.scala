package org.podval.tools.publish.js

object Pele extends JSLibrary:
  val version: String = "1.8.0"

  override def isModule: Boolean = true

  override def inlineJs: Some[String] = Some:
    val pele: String = Js.quote(s"$cdn/pele.js")
    s"""import { mount } from $pele;
       |document.querySelectorAll("code.language-mermaid").forEach(function (el) {
       |  mount(el, el.textContent);
       |});
       |""".stripMargin

  // Pele is published on npm, not cdnjs, so there is no CloudFlare URL to prefer.
  override def cdn: String = s"${JSLibrary.jsDelivr}pele@$version/dist"
