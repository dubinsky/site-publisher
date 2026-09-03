# Site Publisher Agent Guidelines

Static site generator written in Scala 3 + Gradle. Produces sites from Markdown, AsciiDoc, HTML, TEI, and DocBook using a Minima-inspired layout.

## Project Layout

- Main logic: `src/main/scala/org/podval/tools/publish/`
  - `markup/` — `Markup` dialects, shared IR (`Citation`, `BibliographyItem`, `Footnote`, `Glossary`, `Callout`, `Admonition`, `Aside`, `Quote`, `Strike`, `Figure`, `PdfEmbed`, `Video`, `Section`, `Toc`, wiki links), `Bibliography` (citeproc), TEI harvest (`StoreIndex`, `EntityLists.harvest`, `DocumentHeader`)
  - `page/` — `PageContent` + `Content` (store / entity lists / TEI document / entity / markup), `PageHeader`, `PagedList`, generated collection/entity-list indexes (`CollectionIndex`, `EntityLists.generate`), front matter, chunking, PDF pages
  - `site/` — `Site`, `Pages` (page graph, `resolve` / `resolveAsset`), `BackLinks` / `BackLink`, config, sitemap, errors
- Supporting libraries:
  - `org.podval.xml` (`org.podval:org.podval.xml`, repo https://github.com/dubinsky/xml) —
    dialect-aware XML (parsing from string/URL/file/classpath, writing, transform/gather, Xml2Html) and derived document
    codecs (`org.podval.xml.XmlCodec`) over any `XmlAst`. `XmlParser` does not expand `xi:include` unless
    `xinclude = true` (publisher stores treat `xi:include/@href` as a page ref). Catalog files
    (`Selector.xml`) use `parseCatalog` / `decodeCatalog` (wrapper root, child codec).
    Same artifact also has `org.podval.metadata` (multilingual `Name`/`Names`/`Language` catalogs) and
    `org.podval.store` (in-memory named tree, path `resolve`). Publisher `StoreContent` is a filesystem
    page graph and does not use these types. Local unreleased xml: optional `includeBuild` of sibling
    `../xml` (or `-PxmlDir=`).
  - `org.podval.tei` — TEI XML dialect + entity handling
- Resources (CSS): `src/main/resources/org/podval/tools/publish/site/assets/css/`
- Tests: `src/test/scala/...` — ScalaTest `AnyFunSuite`. xml tests live in `dubinsky/xml`.
- Fixture site: `src/test/site` (committed). `SiteSpec` generates into `build/test-site` via an absolute `--target-directory-name` (gitignored under `build/`). Do not generate into `src/test/site/_site` and do not point tests at real sites. ScalaTest classes must be named `*Spec` or Gradle will not run them. One fixture page (`glossary.md`) has `pdf: true`; `SiteSpec` opens that Chromium PDF with PDFBox (page count, a named dest, a string of text). Not visual diffs, not every page.

## Build & Run Commands

```bash
./gradlew build                 # compile + test
./gradlew test                  # run tests only
./gradlew clean
./gradlew run --args="/path/to/source-directory"
```

The application expects a source directory containing `_site_config.yml` as its first (and only) positional argument. `Site.main` is `Site(SiteOptions.forArgs(args)).generate()`.

### Development entry point (`generate`)

`@main def generate()` in `Site.scala` hardcodes a local site path and serves it:

```scala
@main def generate(): Unit = Site(SiteOptions(
  sourceDirectoryPath =
    "/home/dub/OpenTorah/opentorah.org/docs",
  logLevelOpt = Some("INFO"),
  treatErrorsAsWarnings = true
)).serve()
```

Run from IntelliJ (that `@main`) or via `./gradlew run` (CLI `Site.main`, needs `--args`). Modify the path in `generate()` temporarily for other sites during development; do not commit that change.

## Key Concepts for Agents

- `Site` is the top-level coordinator.
- `Pages` scans the source tree, builds the `Page` graph (including synthetic pages like `/posts`, `/tags`, `/errors`, `sitemap.xml`, etc.).
- Content pipeline: dialect `Markup` (md / adoc / html / tei / docbook) → `FrontMatter` + `Xml.Element` → `Content.parse` on the raw root (`StoreContent` / `EntityListsContent` skip dialect `process`; `DocumentContent` / `EntityContent` / `MarkupContent` run it) → `PageContent` (authored: sections, links, footnotes, glossary, citations) → Minima-inspired HTML → write. `PageContent.doc` is the typed variant; `Page.content` is still `Option[PageContent]`.
- Special source directories: `_posts/`, `_drafts/`, Obsidian daily-notes folder (configured via `.obsidian`).
- Links: wiki-style `[[...]]`, internal link resolution, backlinks, TOC. Front-matter `permalink` / `aliases` are Refresh pages; if the target is a directory, `/permalink/child` and `[[permalink/child]]` resolve `child` under it (`Pages.find` longest alias prefix). TEI `store`/`collection` `@alias` is the collection short name (collector `site.xml` `<alias n= to=>`); it does not write a Refresh file. `Page.publishedPath` emits the short href; `serve()` rewrites it to the written file. `Path.fromHref` treats a last-dot suffix as an extension unless it is all digits (`255.2` stays one segment). Local media (`img@src`, `video`/`audio`/`source@src`, PDF `object@data`) go through `AssetRef` stamps and `Pages.resolveAsset` (published path, missing-asset errors), not page-link title-walk. TEI entity files are roots `person` / `place` / `org` (`EntityContent`); `persName` / `placeName` / `orgName` `@ref` resolves by matching kind and the entity file name without `.xml` (not title-walk). `entityLists` as `dir.xml` beside `dir/` is `EntityListsContent`: kind+role buckets (`listPerson` / `listPlace` / `listOrg`); not a store include list; lists are generated at render so the index does not create backlinks.
- Citations: two kinds, usable together. External: dialect syntax → `Citation` IR → the document's front matter `bibliography` (path relative to the source file) and `csl` (no site default). citeproc-java formats; locale is page `lang`, else site `lang`, else `en-US`; list ids are `bibl-{key}`. `.bib` files are ignored at scan (not published); citeproc still reads them from source. Internal: native lists (`BibliographyItem`) — AsciiDoc `[bibliography]` / `[[[id]]]` / `<<id>>`, TEI `listBibl` / `bibl` and `ref`/`ptr` `@target="#id"`, DocBook `bibliography` / `biblioentry` and `link`/`biblioref` `@linkend` — authored ids, hover tips. TEI `@cRef` is the external citeproc key; DocBook uses `<citation>key</citation>` or a `biblioref` whose `linkend` is not a native entry. Markdown has no native in-document list.
- Chunking (`chunk` / `chunk-depth` in front matter) and PDF are markup-independent. Site config `paginate-posts` (optional int) batches the synthetic `/posts` listing (`/posts.html`, `/posts/2.html`, …). Site config `home` (absolute path) makes `/index.html` a Refresh alias to that page (chunked TOC: `P/index.html`); do not also author `index.md`. A TEI `store` at the source root (`archive.xml`) also gets `{name}-collections.html` (tree) and `{name}-index.html` (flat collections); with one such store, `/collections` rewrites to the tree page. Site config `facsimiles-url` (optional) is the facsimile JPEG base (collector `tei@facsimilesUrl`): a `FacsimilePage` at `P/facsimile.html`; JPEG `{url}{TEI source directory}/{n}.jpg`; in-text `pb` and the header images icon link there (`target="facsimile"`); translations share the original's viewer. Inbound collector `/alias/facsimile/P` rewrites to that page (`Pages.find` / `serve()`; Worker must too).
- Every real page writes its `textContent` (or copies assets).

## When Working on the Code

- Prefer making changes that keep the core small and plugin-free (the project's stated philosophy).
- Markup-specific conversion lives next to the dialect (`AsciiDocMarkup` / `AsciiDocCiteExtension`, `MarkdownMarkup` / `MarkdownCite`, `DocBookMarkup`, …). Shared IR stays in `markup/` (`Citation`, `Bibliography`, `Footnote`, `Glossary`, `Section`, …). Link/asset lookup is `Pages.resolve` / `Pages.resolveAsset`; backlinks are `site.BackLink` / `BackLinks`. Page chrome and generated indexes are `page/` (`PageHeader`, `PagedList`, `CollectionIndex`, `EntityLists.generate`, `StoreIndexes` / `StoreIndexPage`). HTML-shaped leftovers (bare `<aside>` / `<blockquote>`, `<s>`, standalone `p>img`, PDF `<object>`, `<video>`, YouTube/Vimeo `<iframe>`) go through `HtmlIr.normalize`, the shared tail of `HtmlMarkup.process`. TEI and DocBook do not use that pass yet. There is no `feature/` package.
- Keep existing comments when moving or refactoring (TODOs, ordering constraints, "why" notes). Move them with the code they describe. Drop or rewrite a comment only if it is factually wrong; do not delete comments to tidy a diff.
- Documentation split: **design** (pipeline, IR, why) goes in the Obsidian note `dub.podval.org/notes/Publishing/Site Publisher.md` under **Design**, with a per-feature subsection when a feature has IR or non-obvious architecture. **User documentation** (how to run the generator; syntax of each construct in each markup) stays in this repo’s `README.adoc`. Some overlap is expected (IR HTML shape in the note vs HTML/author syntax in the README). Do not put design essays in the README or author syntax in the Design section.
- Run `./gradlew test` before considering a change complete.
- Generated directories (`build/`, `out/`, `target/`, `.gradle/`) are gitignored. With `respect_gitignore = true` they should stay out of searches and listings.
- The `build/` directory can be very large — avoid reading files from it.

## IntelliJ + Grok Build Tips

- Semantic Scala work (rename, find symbol/usages, inspections) goes through IntelliJ MCP. Procedure, including prompting to start the IDE and open this project: `~/.grok/rules/intellij-mcp.md`.
- Run `grok` directly inside the IntelliJ terminal (it is detected as a JetBrains terminal).
- Run `/terminal-setup` inside Grok for diagnostics (clipboard, colors, key handling, etc.).
- JetBrains full ACP integration is "Coming soon". Until then the terminal plus MCP is the supported way.
- Attach files with `@path` (e.g. `@src/main/scala/org/podval/tools/publish/site/Site.scala`).
- Use the project root as the working directory when starting Grok so it picks up `AGENTS.md` and `.gitignore`.

## Style / Conventions (from build.gradle)

Scala 3.8.4, Java 25 toolchain.

Compiler flags:
- `-new-syntax`
- `-feature`
- `-language:strictEquality`
- `-source:future`

## Common Pitfalls

- Do not commit changes to the hardcoded path in `generate()`.
- Posts and daily notes have strict filename conventions (`YYYY-MM-DD-title`).
- Directories that should not produce pages (e.g. `_posts`) are specially handled by `Posts.isDirectoryEmptiedOut`.
- TEI `store`/`collection` as `dir.xml` beside `dir/` is `StoreContent`: `xi:include/@href` is a page ref (never XInclude); `StoreContent.bind` orders children and reports `NotInStore`; selector hops in hrefs are not pages. Empty `href`s keep filesystem listing. A `collection` body is `table.collection-index` (parts, translations as Язык variants, `pb` page links); a `store` body stays the directory list.
- TEI entity `@ref` is the file name without `.xml` of a `person`/`place`/`org` file of the same kind; it does not title-walk.
- TEI `entityLists` as `dir.xml` beside `dir/` is `EntityListsContent` (kind+role, `listPerson` `@n` `@role`), not a store `xi:include` list.
- XML dialects are disambiguated by root element for `.xml` files (TEI vs DocBook).
- `Site.targetDirectory` is `sourceDirectory / name` unless `target-directory-name` is absolute; Java `File(parent, "/abs")` on Unix does *not* ignore the parent.
- The Errors page is written after other HTML so unknown citations and unresolved links found while rendering appear on it.
- AsciiDoc `cite:[key]` needs a custom inline-macro regexp (empty target); `cite:[k1, k2]` must join all positional attributes, not just `1`.
