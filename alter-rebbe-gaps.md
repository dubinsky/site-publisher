# Collector gaps on ng.alter-rebbe.org

Decision record for the collector → publisher pass
([www.alter-rebbe.org](https://www.alter-rebbe.org) → [ng.alter-rebbe.org](https://ng.alter-rebbe.org)).
Compared 2026-09-10. User docs: `README.adoc`. Design:
`dub.podval.org/notes/Publishing/Site Publisher.md`. All listed gaps shipped.

Shipped: TEI `gap@reason` tip; entity `<h1>` / `<title>` = first TEI name;
facsimile viewport scroller (not collector `resize: both`); four named windows (opt-in);
`/name` all-entities and `/name/{id}` inbound; `/report` harvest (no-refs, unclears, misnamed);
grouped backlinks.

Former Design non-goals are **in scope** except facsimile `resize: both` — see <<facsimile-viewer>>.

## Decisions

- **Bar:** collector parity, including those former non-goals.
- **Canonical URLs (emitted hrefs, canonical, sitemap):** keep ng shapes. Inbound collector
  URLs rewrite (`Pages.find` / `serve()` / Worker), same as `/rgada/facsimile/003` already does.
- **Entity page body (mentions vs backlinks):** option 3 — keep backlinks; group
  `from` pages that sit under a TEI `collection` by that collection
  (`pathHeaderHorizontal` disambiguates `003`). Not collector mentions (no **Имена:**
  line, no compact doc-id list). See <<grouped-backlinks>>.
- **Document TEI as XML:** do not serve `/rgada/003.xml` (stamped `publicationStmt` etc.)
  until someone asks (Design note, TEI facsimiles).
- **Named windows:** four names, opt-in, default off (`named-windows: true` on alter-rebbe).
  See <<named-windows>>.

Analytics is not on this list. UA ids and `--production` are a follow-up for every site
(Design note **Analytics**).

## Not gaps

Search, in-place editing, the help-page “Блог” link, and stamped document `.xml` are not
on this list. Collection indexes, store headers, aliases (`/rgada/003`), facsimile JPEGs,
translations, entity-list buckets with ⇗, notes, date calendar tips, inbound
`/alias/facsimile/P`, inbound `/name` / `/name/{id}`, `/report` harvest are on ng.

ng extras (not collector): sitemap, Atom feed, `/errors`, settings / glossary-expand, Open Graph.

## Named windows [[named-windows]]

Chosen and shipped: **four names, opt-in, default off.** HTML `target="name"` reuses a browsing context with that `window.name`. Collector set
the name on every page (`loadWindow`) and put `target` on every internal link so at most one
window existed per *kind*. Help.md still describes four kinds: collection TOC, names,
transcription, facsimile.

ng already uses two names (`facsimile` / `text`) on pb, the facsimile icon, and photo→text
links, but does not set `window.name`, so reuse is flaky. Other sites have no such links.

A helper (name + scroll restore only; no UA) is cheap. The cost is **how many names** and
**which sites get `target=` on internal `<a>`s**.

### Approaches

| | Names | Who gets `target` | Non-TEI sites (dub.podval.org, chumashquestions, mathworlds, opentorah docs) |
|---|---|---|---|
| **0** | none | strip even facsimile/`text` | no change |
| **2** | text, facsimile | only transcription ↔ photos (plus `loadWindow` on those pages) | no such pages → no `target`, no helper |
| **3** | hierarchy, text, facsimile | collection/notes/default stay in one window; document opens text; photos open facsimile. Names share hierarchy | if **on**: every wiki/nav link is `hierarchyViewer` (same-tab once `window.name` is set; first click can spawn a second tab if JS did not run). if **off**: like **2** or nothing |
| **4** | + apparatus for entities/lists/reports | collector/help.md: looking up a name from text does not steal the collection window | same as **3** unless gated **off** |

**Always-on 4** (old plan): every publisher site stamps `target` on every internal link. A blog
does not need it. Middle-click + two tabs with the same `window.name` is messy.

**Gated 4:** alter-rebbe (and any future archive) gets collector tiling; other sites emit
nothing. Gate: explicit `_site_config.yml` (default off), or infer from a root TEI store /
`facsimiles-url`. Explicit is predictable.

**2 + loadWindow:** smallest change; dual-pane photos work; opening a document still
*replaces* the collection index; a name click still *replaces* the transcription.

### Recommendation

**Four names, opt-in, default off.** `named-windows: true` on alter-rebbe only. Helper on
every page of that site; `target` is the *destination* page’s viewer (`hierarchyViewer` /
`apparatusViewer` / `textViewer` / `facsimileViewer`). Keep today’s `facsimile`/`text` as
aliases or rename to the collector strings so leftover www tabs still match.

Non-TEI sites: flag off → no helper, no `target` on wiki/header/backlinks (except existing
`rel=me` `_blank` on social). Fixture tests stay off unless a TEI fixture sets the flag.

PDF / chunked HTML icons stay in the text window. Site title → `hierarchyViewer` (the tab
that opened `/` is the collection window), matching help.md.

## Facsimile viewer [[facsimile-viewer]]

Shipped. Do **not** restore collector `div.facsimileWrapper` / `div.facsimileViewer` (1060×1586,
`resize: both`, inner scroll).

www’s box existed to (1) keep site chrome still while paging photos and (2) size the photo
next to a transcription **window**. The handle is a `textarea`-style CSS `resize` — hard to
find, unused on touch, and a 1586px-tall box is taller than a typical laptop content area.
On a tiling WM the browser window is already the pane; an inner resize box fights that.

ng already has the better document: `html.facsimile`, stacked `figure`s, `max-width: 100%`,
`#p{n}` with `scroll-margin-top: 4rem`. That works as a long page (pinch-zoom, print, space
/ page-down). What it lacks vs www is a **contained** scroll so the header does not ride
away, which matters once named windows park facsimile beside text.

**Do:** keep the image stack. On `html.facsimile` only, make `.facsimile-scroller` fill
`calc(100vh - header)` (or `100dvh`) with `overflow-y: auto`. Keep `scroll-margin-top` on
`figure` / `img` so `pb` jumps land below the header. Images stay `max-width: 100%`. Optional:
compact or hide the site footer on facsimile pages. No fixed pixel size, no `resize: both`.
Emitted URL stays `/P/facsimile.html`.

Not in this item: named windows (next), a text+image split on one page.

## Grouped backlinks [[grouped-backlinks]]

Shipped. Still `BackLinks` (snippets, per-page `<details>`). Outer group is the TEI
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
