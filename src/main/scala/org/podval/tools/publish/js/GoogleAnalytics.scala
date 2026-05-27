package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

// https://support.google.com/tagmanager/answer/14842164
// https://support.google.com/analytics/answer/14171598?hl=en
final class GoogleAnalytics(id: String) extends JSLibrary:
  override def cdn: String = "https://www.googletagmanager.com/gtag"
  
  override def imports: List[String] = List(s"/js?id=$id")

  override def inlineJs: Some[Js] = Some:
    js"""window.dataLayer = window.dataLayer || [];
        |function gtag(){window.dataLayer.push(arguments);}
        |gtag('js', new Date());
        |gtag('config', '$id');
        |""".stripMargin
    
