package org.podval.tools.publish.util

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.Site

object AsciidoctorUtil:
  def asciidoctor(site: Site): Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    //    // Note: only extensions packaged as jars will work - if they are on the classpath.
    //    site.asciidoctorExtensions.foreach: gemName =>
    //      site.log.info(s"Loading AsciiDoc extension gem '$gemName'")
    //      result.requireLibrary(gemName)
    result
