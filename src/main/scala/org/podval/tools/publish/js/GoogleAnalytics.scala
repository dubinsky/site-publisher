package org.podval.tools.publish.js

import zio.blocks.html.*

// https://support.google.com/tagmanager/answer/14842164
// https://support.google.com/analytics/answer/14171598?hl=en
final class GoogleAnalytics(id: String) extends JSLibrary:
  override def scripts: List[Dom.Element.Script] = List(
    script().externalJs(s"https://www.googletagmanager.com/gtag/js?id=$id"),
    script().inlineJs(
      js"""window.dataLayer = window.dataLayer || [];
          |function gtag(){window.dataLayer.push(arguments);}
          |gtag('js', new Date());
          |gtag('config', '$id');
          |""".stripMargin
    )
  )
  