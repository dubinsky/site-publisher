package org.podval.tools.publish.markup

import org.podval.xml.XmlWriterConfig

object TeiXmlWriterConfig extends XmlWriterConfig(
  selfCloseEmpty = true,
  stack = Set(
    "store", "collection", "by",
    "entityLists", "listPerson", "listPlace", "listOrg",
    "person", "place", "org"
  ),
  unStack = Set("choice"),
  // `l` is not nested: a long verse line would otherwise break onto its own `<l>` / `</l>` lines.
  stick = Set("l"),
  nest = Set("p", "head", "salute", "dateline"),
  break = Set("lb"),
  cling = Set(
    "note", "lb", "sic", "corr",
    "persName", "placeName", "orgName",
    "hi", "fw", "date", "ref", "ptr",
    "del", "emph", "unclear", "seg", "gap", "supplied", "add", "pb"
  )
)
