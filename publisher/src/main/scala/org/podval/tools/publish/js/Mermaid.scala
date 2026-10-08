package org.podval.tools.publish.js

object Mermaid extends JSLibrary:
  val version: String = "11.13.0"
  
  override def isModule: Boolean = true

  override def inlineJs: Some[String] = Some:
    val mermaid: String = Js.quote(s"$cdn/mermaid.esm.min.mjs")
    s"""import mermaid from $mermaid;
       |var sources = [];
       |document.querySelectorAll(".language-mermaid").forEach(function (el) {
       |  sources.push({ el: el, text: el.textContent });
       |});
       |var running = false;
       |var again = false;
       |async function renderMermaid() {
       |  if (running) { again = true; return; }
       |  running = true;
       |  try {
       |    var dark = document.documentElement.classList.contains("color-scheme-dark");
       |    mermaid.initialize({ startOnLoad: false, theme: dark ? "dark" : "default" });
       |    sources.forEach(function (item) {
       |      item.el.removeAttribute("data-processed");
       |      item.el.textContent = item.text;
       |    });
       |    await mermaid.run({ querySelector: ".language-mermaid" });
       |  } finally {
       |    running = false;
       |    if (again) { again = false; await renderMermaid(); }
       |  }
       |}
       |await renderMermaid();
       |document.documentElement.addEventListener("site-color-scheme", function () {
       |  renderMermaid();
       |});
       |""".stripMargin

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}mermaid/$version",
    s"${JSLibrary.jsDelivr}mermaid@$version/dist"
  )
