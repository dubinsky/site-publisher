package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.store.selector
import org.podval.tools.publish.site.Terms
import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  test("built-in document and name resolve; an unknown string is not a selector") {
    val ru: Language.Spec = Language.Russian.toSpec
    val en: Language.Spec = Language.English.toSpec
    val terms = Terms.catalog
    assert(terms.selector("document").id == "document")
    assert(terms.selector("name").id == "name")
    assert(terms.selector("note").id == "note")
    assert(terms.word("document")(using ru) == "документ")
    assert(terms.word("document")(using en) == "document")
    assert(terms.word("name")(using ru) == "имя")
    assert(terms.word("name")(using en) == "name")
    val name = terms.getById("name")
    assert(name.plural(en).contains("names"))
    assert(name.plural(ru).contains("имена"))
    assert(terms.forName("name").exists(_.id == "name"))
    assert(terms.forName("имя").exists(_.id == "name"))
    assert(terms.forName("names").isEmpty)
    assert(terms.forName("имена").isEmpty)
    assert(terms.forName("item").isEmpty)
    assert(terms.forName("document").exists(_.id == "document"))
  }
