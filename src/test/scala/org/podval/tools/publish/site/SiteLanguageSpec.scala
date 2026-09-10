package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.scalatest.funsuite.AnyFunSuite

final class SiteLanguageSpec extends AnyFunSuite:
  test("languageSpec from config lang; omitted or unknown is English") {
    assert(Site.languageSpec(Some("ru")) == Language.Russian.toSpec)
    assert(Site.languageSpec(Some("en")) == Language.English.toSpec)
    assert(Site.languageSpec(Some("en-US")) == Language.English.toSpec)
    assert(Site.languageSpec(Some("ru-RU")) == Language.Russian.toSpec)
    assert(Site.languageSpec(Some("Russian")) == Language.Russian.toSpec)
    assert(Site.languageSpec(Some("русский")) == Language.Russian.toSpec)
    assert(Site.languageSpec(None) == Language.English.toSpec)
    assert(Site.languageSpec(Some("")) == Language.English.toSpec)
    assert(Site.languageSpec(Some("zz")) == Language.English.toSpec)
  }
