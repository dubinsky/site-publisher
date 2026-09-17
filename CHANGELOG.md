# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
- TEI names without `@ref`, `unclear`, and misnamed entity files are page errors on first parse (Errors page). No `/report` harvest, pages, or inbound Worker prefixes. Entity-file name elements are not `name without @ref`. Each kind on `/errors.html` has a fragment id (`#no-ref`, `#unclear`, …).
- fix: `deploy-alias-worker` copies Worker JS from `publisher/` (empty Gradle root).

## [0.2.0] - 2026-09-16

- Empty Gradle root with sibling `publisher/` (library) and `plugin/` subprojects. Maven coordinates unchanged. Dogfood: `pluginManagement { includeBuild }` for the plugin id plus settings-body `includeBuild` of the same directory for the library child.
- Podval Gradle convention plugins.
- Gradle plugin `org.podval.tools.site-publisher`: `generateSite` / `serveSite` as `JavaExec` on a detached `sitePublisher` configuration (Java subproject; publisher stays off the Gradle daemon). Dogfood with `pluginManagement { includeBuild }`, not `mavenLocal()`. README **Gradle plugin**; design note subsection.
- Backlinks from TEI collections group under the collection path header; snippet text skips `*-tip` subtrees.
- Report harvest: names without `@ref`, TEI `unclear`, entity file id vs underscored main name.
- Inbound `/name` (all entities) and `/name/{id}`; `/report` routes. Worker table includes those prefixes.
- Opt-in named windows (`named-windows`): four collector `window.name`s; default off.
- Facsimile viewer: photos scroll in a viewport-filling pane under the header (not a 1060×1586 `resize: both` box).
- Entity page `<h1>` / `<title>` use the first TEI name, not the file name.
- TEI `gap@reason` gets a hover tip (`span.gap-ref` / `span.gap-tip`), same family as date tips.

## [0.1.0] - 2026-09-10
- First Maven Central publication (`org.podval.tools:org.podval.tools.publisher`).
- CLI: `--serve`; default `--log-level` is `INFO` (was `DEBUG`).
- Playwright Chromium cache: `PLAYWRIGHT_BROWSERS_PATH` or `~/.gradle/ms-playwright` (`$GRADLE_USER_HOME/ms-playwright` from Gradle).
- GitHub `generate` action resolves the published JAR from Maven Central (does not compile the action checkout); caches Playwright browsers.
- README: Maven coordinates, dogfooding `includeBuild`; Gradle plugin plan in `gradle-plugin-plan.md` (not implemented).
- fix: empty-href store/collection scan lists the directory (nested TEI documents were dropped as `NotInStore`)
- TEI `date/@when` (and `..` ranges) get Julian/Gregorian/Jewish hover tables via `org.opentorah:opentorah-core`; site config `tei-default-calendar` (`julian`/`gregorian`, default gregorian)
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
