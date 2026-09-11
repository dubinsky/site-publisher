# Collector gaps on ng.alter-rebbe.org

Inventory of live [www.alter-rebbe.org](https://www.alter-rebbe.org) (old Collector, Cloud Run)
functionality that [ng.alter-rebbe.org](https://ng.alter-rebbe.org) (this publisher, GitHub Pages)
does not have. Compared 2026-09-10.

This file is the working list and closing plan. It is not user documentation (that stays in
`README.adoc`) and not the Design essay (that stays in
`dub.podval.org/notes/Publishing/Site Publisher.md`). When a gap closes, strike it here and
update the Design note if a former non-goal changed.

Former Design non-goals are **in scope** (mentions, facsimile chrome / facets as UX).

## Decisions

- **Bar:** collector parity, including those former non-goals.
- **Canonical URLs (emitted hrefs, canonical, sitemap):** keep ng shapes. Inbound collector
  URLs rewrite (`Pages.find` / `serve()` / Worker), same as `/rgada/facsimile/003` already does.
- **Entity page body (mentions vs backlinks):** option 3 — keep backlinks; group
  `from` pages that sit under a TEI `collection` by that collection
  (`pathHeaderHorizontal` disambiguates `003`). Not collector mentions (no **Имена:**
  line, no compact doc-id list). See <<grouped-backlinks>>.

Analytics is not on this list. UA ids and `--production` are a follow-up for every site
(Design note **Analytics**).

## Inventory

Live www has it; live ng does not.

### Named windows

Help (`note/help.md`) still describes four window types: collection TOC, names, transcription,
facsimile. Collector sets `window.name` and `target` so each type reuses one window
(`hierarchyViewer`, `apparatusViewer`, `textViewer`, `facsimileViewer`). `js/window.js` also
restores scroll.

ng: only transcription ↔ facsimile use `target="facsimile"` / `target="text"`. Everything else
is same-tab. `js/window.js` is in the alter-rebbe tree and is copied to `_site`, but pages do
not load it.

### Facsimile viewer chrome

www: `div.facsimileWrapper` / `div.facsimileViewer` — 1060×1586, `resize: both`, inner
`.facsimileScroller`. ng: `html.facsimile` + a vertical stack of images (~1100px), page scroll,
no resize handle.

Same JPEGs and `#p{n}`. Emitted URL stays `/alias/P/facsimile.html`. Inbound
`/alias/facsimile/P` already rewrites.

### Document TEI as XML

`GET /rgada/003.xml` on www is the TEI with `publicationStmt` (site URL, CC BY 4.0),
`sourceDesc`, `langUsage`, `calendarDesc`. ng 404s. Worker rewrite of `/rgada/003.xml` would
already target a sibling `.xml` if that file were written
(`extension` is kept; default `.html` only when the request has none).

### Reports (`/report`)

Not in the header nav on www either; still generated.

| Path | Title |
|---|---|
| `/report` | Отчеты |
| `/report/no-refs` | `persName` / `placeName` / `orgName` with no `@ref` |
| `/report/unclears` | TEI `unclear`, with source |
| `/report/misnamed-entities` | Entity file id ≠ underscored main name |

### Names / entities

- `/name` — flat alphabetical list of every entity (~193). ng `/names` is only the buckets
  (`jews`, `officials`, …).
- Entity URL `/name/alter-rebbe` vs ng `/names/alter-rebbe.html`. Old `/name/…` 404s on ng
  (Worker routes are collection aliases only).
- Collector `<title>` / body title is the main TEI name (Залман Борухович). Done: `Page.title`
  uses `entityDisplayName` (`<h1>` / `<title>`). File name remains `titleFromPath` / URL.
- Collector **mentions** (per-collection doc ids + **Имена:**) vs ng **backlinks**
  (flat accordion, title `003`). Closing via grouped backlinks, not mentions
  (<<grouped-backlinks>>).

### TEI `gap@reason`

Done. `TeiGap`: `span.gap-ref` / `span.gap-tip` after the Xml2Html pass (so `class` is not
`tei-class`). Hover/focus CSS with the other tips; hidden in print. No `@reason` → no wrap.

## Not gaps

Search, in-place editing, and the help-page “Блог” link are not on live www. Collection
indexes, store headers, aliases (`/rgada/003`), facsimile JPEGs, translations, entity-list
buckets with ⇗, notes, date calendar tips, inbound `/alias/facsimile/P` are on ng.

ng extras (not collector): sitemap, Atom feed, `/errors`, settings / glossary-expand, Open Graph.

## Closing plan

Publisher-generic unless noted. alter-rebbe.org CI / `window.js` / help text are that repo.

### 1. `gap@reason`

Done (`TeiGap`, IR pass + `convertFragment`, `TeiMarkupSpec`).

### 2. Entity title

Done (`Page.title` prefers `entityDisplayName`; `EntitySpec`).

### 3. Facsimile resize box

Keep emitted `/P/facsimile.html`. Restore collector wrapper markup on `FacsimilePage` and the
1060×1586 `resize: both` rules (today’s `.facsimile-scroller` stays the inner scroller).
Do not emit `/alias/facsimile/P` (inbound only).

### 4. Named windows

Load a publisher-owned window helper (extract name + scroll restore from alter-rebbe
`js/window.js`; leave GA to gtag). Set `window.name` from the page’s viewer:

| Viewer | Pages |
|---|---|
| `hierarchyViewer` | stores, collections, notes, home, default |
| `apparatusViewer` | entities, entity lists, reports |
| `textViewer` | transcription (`DocumentContent`) |
| `facsimileViewer` | `FacsimilePage` |

Every internal link `target`s the **destination** viewer (today only pb / facsimile chrome
do, and they use `"facsimile"` / `"text"`). Switch those two names to the collector ones so
help.md and a leftover www tab still match. Site title stays `hierarchyViewer` (collector:
the tab that opened `/` is the collection window).

`siteSettings.js` can stay; it does not set `window.name`.

### 5. Inbound `/name/…`, all-entities list, `/report`

Canonical entity stays `/names/{id}.html`. Add:

- Synthetic all-entities page (collector `/name`). Written path can be `/name/index.html`
  (ng never had this page; no collision with `/names`).
- Worker + `Pages.find` / `rewriteRequest`: `/name` → that index; `/name/{id}` →
  `/names/{id}.html`. Same slash-delimited prefix table as collection aliases — extend
  `collection-aliases.json` (or rename it to a general rewrite table) with `/name` and
  `/report`.
- Reports as synthetic pages at `/report/index.html`, `/report/no-refs.html`, … (collector
  paths; ng never had them). Harvest at render, same timing as `EntityLists` / `CollectionIndex`
  so the report XML is empty and hrefs are not backlinks.

Harvest (collector `Collector.writeReferences` / `writeUnclears`):

- **no-refs:** entity-name elements with empty `@ref`, with source path.
- **unclears:** `unclear`, with source.
- **misnamed-entities:** `id != spacesToUnderscores(mainName)`.

Walk documents, entity files, and store title/abstract/body (collector `allWithSource`). Do
not use generated collection-index or entity-list member links.

Worker routes today are collection aliases only; `/name*` and `/report*` never hit the
Worker. Prefix routes must include the new prefixes (same “CSS/JS skip the Worker” rule).

### 6. Document XML

Write `{published-dir}/{base}.xml` next to `{base}.html` for `DocumentContent`. Content is
collector `renderTei`: authored TEI plus `publicationStmt` / `availability` / `sourceDesc` /
`langUsage` / `calendarDesc` from site config (url, license, `tei-default-calendar`).
`GET /rgada/003.xml` then rewrites to that file. Translations: `{base}-{xx}.xml` if we serve
the translation page; originals only is enough for www parity (`/rgada/003.xml`).

`SyntheticXmlAsset` is the existing hook.

### 7. Grouped backlinks

See <<grouped-backlinks>>. One renderer (`BackLinks.html`); no mentions harvest.

### 8. Docs once behaviour matches

- This file: mark gaps done.
- Design note: drop “not collector-style per-collection mentions” and “not a 1060×1586
  resize box” if those ship; keep “not `/collection/facsimile/P` as the *emitted* URL”.
- `README.adoc`: reports, `/name` inbound, document `.xml`, named `target`s, `gap` tip —
  author-visible behaviour only.
- alter-rebbe `note/help.md`: already describes four windows; no change if §5 ships.

### Suggested PR order

Independent unless noted.

1. `gap@reason` tip.
2. Entity `<h1>` / `<title>` = display name.
3. Facsimile resize wrapper (CSS + `FacsimilePage` markup).
4. Named windows + window helper (depends on nothing; switches existing `target`s).
5. Rewrite table: `/name`, `/report`; all-entities page; `/name/{id}` inbound.
6. Document `.xml` next to `.html`.
7. Reports (needs the same harvest walk as mentions-from-documents).
8. Grouped backlinks (<<grouped-backlinks>>). Independent of 1–7.
9. Design note / README / help as above.

Tests: fixture site already has a tiny store + entities (`EntitySpec`, `FacsimileSpec`,
`CollectionAliasesSpec`). Extend those rather than hitting live alter-rebbe.

## Grouped backlinks [[grouped-backlinks]]

Chosen. Still `BackLinks` (snippets, per-page `<details>`). Outer group is the TEI
`collection` that `from` sits under, so two `003`s are not adjacent twins: the collection
path header is the disambiguator. Inner label stays the short document id (`from.ref()` →
title `003`). Not collector `p.mentions`.

### Target HTML

```
Backlinks
  архив РГАДА, разряд VII, опись 2, дело 3140     ← link to the collection index
    000 (3)
      snippets…
    003 (12)
      snippets…
  архив книги, книга Дубнов
    084 (1)
  some note title (2)                            ← no collection ancestor: ungrouped, as today
```

### Grouping

`BackLinks.html` already does `groupBy(_.from)` then `sortBy(_._1.title)`. Keep that inner
grouping. Add an outer partition:

- **Collection of `from`:** last store-ancestor (or `from` itself) with
  `store.exists(_.isCollection)`. Same chain as `PageHeader.collectorAncestors`
  (`(ancestors :+ from).findLast(...)`).
- **Header string:** `StoreIndexes.pathHeaderHorizontal(collection, root)` — the same
  “архив РГАДА, разряд VII, опись 2, дело 3140” as `/archive-index.html`. `root` is the
  unique `StoreIndexes.isRootStore` that is an ancestor of `collection`; if there is none,
  do not drop a prefix (header is still the selector+name chain).
- **Header href:** `collection.publishedPath` (short alias when the collection has `alias`).
- **Order of groups:** `StoreIndexes.collectionsUnder(root)` when a root store exists
  (collector `collectionPaths` order), then leftover collection groups, then ungrouped
  pages. Inside a group: `from.title` as today (`000` before `003`; `003-ru` separate).
- **Ungrouped:** notes, other entities, stores that are not under a collection. Same
  `<details>` as today. Sort by title. After the collection groups.
- **`from` is the collection index itself:** that page is a member of its own group; inner
  label is `listTitle` / `ref()`, not `003`. Same group as its documents.
- Site-wide: one renderer. Sites without collections (dub.podval.org) keep a flat list.

Do not harvest a new mentions walk. Do not add an **Имена:** line. Generated collection-index
and entity-list member hrefs stay out (index XML empty at `Site.load`).

### Snippet leak (same PR)

`LinkContext` uses `Xml.toString` on sibling nodes. A following `span.date-ref` includes
`span.date-tip` (calendar table), so snippets show `CalendarDateJulian…`. Walk text for
context but skip any element whose class ends with `-tip` (dates, footnotes, glossary,
citations). Do not stringify the tip subtree.

### CSS

`.backlinks > ul` is already unbulleted. Add a nested group (`li.backlinks-collection > ul`)
so document `<details>` indent under the path header; keep `.backlinks-count` on the inner
summary. Print: existing backlinks rules; no new exception.

### Files

- `site/BackLinks.scala` — partition, collection header, reuse inner `<details>`.
- `site/LinkContext.scala` — tip-skipping text for `before` / `after`.
- `page/StoreIndexes.scala` — `pathHeaderHorizontal` already exists; maybe a
  `collectionOf(page)` helper next to `collectorAncestors` (`PageHeader` or `StoreIndexes`).
- `assets/css/layout.css` — nested group.
- Tests: two collections each with `000.xml` linking the same entity (`CollectionIndexSpec`
  fixture is one collection; add a second or a small `BackLinksSpec`). Assert both
  `pathHeaderHorizontal` strings, both `/col/000` and `/other/000` hrefs, and that a note
  backlink stays ungrouped. Date-in-sibling fixture: snippet must not contain `Julian` /
  `date-tip`. Existing `EntityListsSpec` “entity page keeps document backlinks” still
  passes (`/doc.html` may be ungrouped if `doc.xml` is not under a collection).

### Not in this PR

Named windows, `/name` inbound, reports, document `.xml`, facsimile box, entity `<h1>`.
Design note keeps “not collector-style per-collection mentions”.
