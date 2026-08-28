# Publish TEI XML next to HTML

Stashed 2026-08-28; not implemented yet.

## Goal

Write the authored TEI (documents and entities) into the site as `.xml` beside the HTML, with collector-style header fields filled in. No site-header download icon: the HTML URL with `.xml` is the link, as on live alter-rebbe (`/rgada/029` → `/rgada/029.xml`).

Collector `renderTei` (and `Collector.content` for `TextFacet` + `.xml`) is the reference. Typed `org.podval.tei` codec round-trip is **not** used: that codec is still incomplete. Inject by XML tree surgery on the **raw** tree (before `TeiMarkup.process`).

## Decisions (locked)

| Topic | Choice |
|---|---|
| Which files | Root `TEI` documents **and** `person` / `place` / `org` entities. Not `store` / `collection` / `entityLists`. |
| When | Always. No front-matter flag. |
| URL | `P.xml` next to `P.html` (same as `PdfPage` uses `withExtension`). Collection alias → `/rgada/029.xml`. |
| HTML chrome | None. Do not add a `formatLinks` icon. |
| Header fields | Documents: `publicationStmt` + `langUsage` always (when the data exists). `sourceDesc` and `calendarDesc` only if site config has them. Overwrite those slots even if the source file already has them. Entities: no `teiHeader` injection (they have no header). |
| Pretty-print | `TeiXmlDialect.render` + `<?xml version="1.0" encoding="UTF-8"?>`. TEI `xmlns` on the root if missing. No DOCTYPE (collector `renderWithHeader` used `doctype = None`). |

## Collector fields (documents only)

From `Site.renderTei` / live `/rgada/029.xml` vs source `029.xml`:

**`fileDesc/publicationStmt`** (always replace):

- `publisher/ptr@target` = site `url`. If `url` has no scheme, prefix `http://` (collector did; live XML is `http://www.alter-rebbe.org`). If `url` already has `://`, use it as-is (fixture is `http://fixture.test`).
- `availability@status="free"`.
- If `license` is set: child `licence/ab/ref@n="license" @target=license-link` with the license name as text (TEI spelling `licence`). Omit that child when `license` is missing (alter-rebbe `_site_config.yml` currently has no license; do not invent one).

**`fileDesc/sourceDesc`**: replace with the parsed inner XML of config `tei-source-desc` when that key is non-empty. Omit the element when the key is absent.

**`profileDesc/langUsage`**: replace with `<language ident="{text/@xml:lang}"/>` when `text/@xml:lang` (or `lang`) is present. Skip when the document has no language (do not crash the way collector `.get` would).

**`profileDesc/calendarDesc`**: replace with the parsed inner XML of config `tei-calendar-desc` when set.

Preserve TEI child order when inserting:

- `fileDesc`: `titleStmt`, `editionStmt`, `extent`, **`publicationStmt`**, `seriesStmt`, `notesStmt`, **`sourceDesc`**
- `profileDesc`: `abstract`, `creation`, **`langUsage`**, `textClass`, `correspDesc`, **`calendarDesc`**, `handNotes`, `listTranspose`

Create `fileDesc` / `profileDesc` if `teiHeader` exists but that child is missing. If there is no `teiHeader` (entities), do not add one.

## Config

Add optional strings on `Config` (kebab-case YAML, same as `facsimiles-url`):

```yaml
tei-source-desc: |
  <p>Facsimile</p>
tei-calendar-desc: |
  <calendar xml:id="julian"><p>Julian calendar</p></calendar>
```

Values are **inner** XML of those TEI elements, not a wrapper tag. Parse with `XmlParser.parseXml` around a dummy root; a bad snippet is a site-level `PageError` (do not abort the whole generate if treat-errors-as-warnings). Existing `license` / `license-link` / `url` are reused; no new license keys.

This repo does not change alter-rebbe `_site_config.yml` in this work. Without `tei-source-desc` / `tei-calendar-desc` / `license`, published archive XML will have `publicationStmt` (publisher + empty-ish availability) and `langUsage` only.

## Page graph

Mirror `PdfPage` / `FacsimilePage`:

1. Keep the pre-`process` tree on `DocumentContent` and `EntityContent` (`rawXml: Xml.Element`). `TeiMarkup.process` does not mutate the original (new tree). Store/collection/entityLists stay as they are.

2. `TeiXmlPage(document: FullMarkupPage) extends SyntheticXmlAsset(site, document.path.withExtension("xml"))`.
   - `needed`: `doc` is `DocumentContent` or `EntityContent`.
   - `xmlContent`: `TeiPublish.decorate(rawXml, config)` for documents; entities get xmlns-only (plus declaration at write).
   - Override `textContent` to prepend the XML declaration. Do not use `XmlDialect.Plain` (use `TeiXmlDialect`).
   - Default parent is the same directory as the HTML page (unlike `FacsimilePage`, do **not** add a path segment).

3. Register in `Pages.add` next to PDF/facsimile:

```scala
if TeiXmlPage.needed(page) then add(TeiXmlPage(page))
```

4. Hide from directory listings and collection tables the same way as `PdfPage`:

   - `DirectoryPage` children
   - `CollectionIndex.originalsUnder`

5. Sitemap already keeps HTML-only (`path.extension.contains("html")`). Do not add XML.

6. No `formatLinks` change.

## URLs and serve

`Path.withExtension("xml")` on `…/029.html` is `…/029.xml`. `publishedPath` already copies the extension through a collection alias, so public href is `/rgada/029.xml`.

`rewriteRequest` / `findUnderAliased` already pass `path.extension` into `findExact`. Cover this with a test modeled on `FacsimileSpec` inbound facsimile URLs:

- `/col/000.xml` → written `…/000.xml`
- `/rgada/029.xml` (alias) → written archive path, **not** `.html`

Local `sendFile` uses `URLConnection.guessContentTypeFromName` (`application/xml` for `.xml`). Optional nicety: `application/tei+xml` for these files (`Tei.mimeType`). GitHub Pages will still serve `application/xml`; that is acceptable.

**Cloudflare Worker (not this PR, same class of follow-up as facsimiles):** today the Worker appends `.html` after alias prefix replace. Requests that already have `.xml` must keep that extension or `/rgada/029.xml` becomes a 404. Note it in README and the design note the same way facsimile inbound URLs are noted (`Worker must too`).

## Injection implementation

New `org.podval.tools.publish.markup.TeiPublish` (next to `Facsimile` / `TeiMarkup`, not in `org.podval.tei`):

- `decorate(root, config): Xml.Element` for `TEI` roots.
- Helpers: replace-or-insert named child in a parent, respecting the order lists above.
- Parse config snippets once per site (lazy on `Site` or `TeiPublish`) rather than per file.

Do **not** pretty-print through `Tei.parse` / zio-blocks codec. Do **not** copy the source file bytes (`AssetWithSourcePath`): headers must be added and pretty-print will reflow, as the collector did.

## Tests

New `TeiPublishSpec` (fixture style of `FacsimileSpec` / `EntitySpec`), plus a couple of lines in `SiteSpec` for the committed fixture.

Documents:

- Target contains `tei-sample.xml` (or fixture `doc.xml`) as XML, not HTML.
- Contains `publicationStmt/publisher/ptr@target` from site `url`, `availability@status="free"`.
- Contains `langUsage/language@ident` from `text/@xml:lang` when present; omitted when absent.
- `sourceDesc` / `calendarDesc` appear only when config keys are set; contents match the snippets.
- Existing `publicationStmt` in the source is overwritten, not merged.
- HTML page has **no** new `page-format` icon pointing at the XML.
- Store/collection index does **not** write `index.xml` / `col.xml`.
- Collection listing and directory `ul.page-list` do not list the XML as a sibling.
- Alias inbound `/alias/P.xml` rewrites to the written file.

Entities:

- `people/alter-rebbe.xml` is written; root stays `person`; no `teiHeader` / `publicationStmt`.
- `xmlns="http://www.tei-c.org/ns/1.0"` present; XML declaration present.

Config:

- `ConfigSpec`: `tei-source-desc` / `tei-calendar-desc` optional and kebab-case.

## Docs

- **README.adoc** (user): new short section under TEI (near facsimiles). URL convention `P.html` ↔ `P.xml`; which files; which header fields; the two optional config keys; Worker `.xml` caveat.
- **Design note** `dub.podval.org/notes/Publishing/Site Publisher.md`: subsection under Design — raw tree, tree surgery not codec, synthetic `TeiXmlPage`, alias rewrite, Worker.
- **AGENTS.md**: one sentence next to the facsimile bullet (published `P.xml`, documents+entities, header inject, no icon).

Do not put author syntax in the Design section or a pipeline essay in the README.

## Out of scope

- Header download icon.
- Publishing store/collection/entityLists XML.
- Changing alter-rebbe site config (license, sourceDesc, calendarDesc) in this repo.
- Cloudflare Worker implementation.
- Completing the `org.podval.tei` encode path.
- Sitemap XML declaration TODO.
- Wrapping entity files in `TEI` / `teiHeader`.
