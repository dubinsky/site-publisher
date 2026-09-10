# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
- xml: 0.1.0
- Store index titles use `Selector.pluralOrNames` (singular when there is no plural).
- `Selector.xml` lives here (`page.Selectors`); xml `By(name)` needs that catalog in scope.
- xml: `Named` is `HasNames`
- Stop pre-escaping text into the XML tree (`Feed`, malformed markup). Atom `content`/`summary` HTML is CDATA so undeclared `&nbsp;` stays well-formed.
- use `org.podval.store.Selector` for collector `by/@selector` labels; drop the publisher copy of the type and `Selector.xml`
- xml: TagSoup is `compileOnly` in the library; declare it here for HTML parse
- xml: extracted to https://github.com/dubinsky/xml (`org.podval:org.podval.xml`); optional `includeBuild` of sibling `../xml`
- xml: folded `Pure` into `Stores` (`storesPure` → `stores`, `Stores.With`)
- xml: `org.podval.store` and `org.podval.metadata` (from OpenTorah `core`); store walks and `HasName.bind` are synchronous
- xml: `loadCatalog(this, codec)` derives `Foo.xml` / `<Foo>` from the caller's class; `XmlDecode` holds the hand-codec helpers
- xml: ignore comments, PIs, and text outside the document element (catalog files with a prologue comment)
- xml: `XmlCodec.decodeCatalog` / `XmlParser.parseCatalog` load a named wrapper and decode each child (Selector catalog uses it)
- xml: `XmlParser` loads from URL, file, and classpath; `xi:include` is not expanded (store `xi:include` stays a page ref)
- xml: `XmlParserSax` accepts `InputSource` / stream / reader (HTML `parseHtml` from URL or file)
- xml: Gradle subproject `org.podval.xml` with a Schema-derived document binder (`XmlCodec`) over any `XmlAst` (unwrapped sequences, leaf-record attributes, `Xml.Element` identity)
- xml: removed `RawXml` / `WithRawXml` / `XmlNode` / `XmlExtras`; leftover parent content is an error
- xml: `XmlCodec` lives in `org.podval.xml`; Selector, collection parts, and entity-lists decode with it
- xml: `XmlTag` binds a field from the element name; `EntityList` uses `kind: EntityKind`
- xml: drop unused `Entity` / `EntityName` / `EntityReference` codecs; collection `part` titles are `Xml.Element`

## [0.0.1] - 2026-
- chore: initial check-in
