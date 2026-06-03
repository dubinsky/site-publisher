package org.podval.store

import org.podval.xml.XmlDialect

object StoreXmlDialect extends XmlDialect(
  name = "Store",
  root = Set("store")
)
