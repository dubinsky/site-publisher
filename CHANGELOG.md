# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
- xml: extracted to https://github.com/dubinsky/xml (`org.podval:org.podval.xml`); optional `includeBuild` of sibling `../xml`
- xml: folded `Pure` into `Stores` (`storesPure` → `stores`, `Stores.With`)
- xml: `org.podval.store` and `org.podval.metadata` (from OpenTorah `core`); store walks and `HasName.bind` are synchronous
- xml: `loadCatalog(this, codec)` derives `Foo.xml` / `<Foo>` from the caller's class; `XmlDecode` holds the hand-codec helpers
- xml: leftover checks ignore `xml:base` (XInclude writes it on included roots)
- xml: `parseCatalog` can expand XInclude (Tanach.xml includes the chumash books)
- xml: ignore comments, PIs, and text outside the document element (catalog files with a prologue comment)
- xml: `XmlCodec.decodeCatalog` / `XmlParser.parseCatalog` load a named wrapper and decode each child (Selector catalog uses it)
- xml: `XmlParser` loads from URL, file, and classpath; XInclude is off by default (store `xi:include` stays a page ref); `xinclude = true` expands includes and sets `xml:base` relative to the initial document
- xml: `XmlParserSax` accepts `InputSource` / stream / reader (HTML `parseHtml` from URL or file)
- xml: Gradle subproject `org.podval.xml` with a Schema-derived document binder (`XmlCodec`) over any `XmlAst` (unwrapped sequences, leaf-record attributes, `XmlNode` identity, leftover `XmlExtras`)
- xml: removed `RawXml` / `WithRawXml`; `Entity` / `EntityReference` keep leftovers in `XmlExtras`
- xml: `XmlCodec` lives in `org.podval.xml`; Selector, collection parts, and entity-lists decode with it
- xml: `XmlTag` binds a field from the element name; `Entity` / `EntityName` / `EntityReference` / `EntityList` use `kind: EntityKind`

## [0.0.1] - 2026-
- chore: initial check-in
