# Planned and considered

Inventory of site-publisher work that is planned or considered and not yet implemented.
Drawn from GitHub issues, the design note (`dub.podval.org/notes/Publishing/Site Publisher.md`), `README.adoc`,
`AGENTS.md`, and source TODOs.

## Product features

### TEI / collector remaining

- **Apparatus** ([#22](https://github.com/dubinsky/site-publisher/issues/22)): use `gap` / `supplied` / `corr` in place
  of some notes (with CSS); use `hand` instead of notes like “написано пером, зачёркнуто карандашом”.
  Gap `@reason` tooltips already exist.
- **Store-tree `By("names")` / `By("name")` hops** (`/jews`, `/jews/alter-rebbe`) — design-note TODO.
- **Entity mentions, collector-style** ([#15](https://github.com/dubinsky/site-publisher/issues/15),
  [#9](https://github.com/dubinsky/site-publisher/issues/9)):
  - ids on all entity references, used in mentions
  - mention links go to the first occurrence in the document, not only the document start
  - mention links show the document title
  - process name tags such as `surname`
  - compact “Имена:” / document-id lists (current grouping is only by collection path on ordinary backlinks)
- **Facsimile: viewer for individual images** (fragments, alternative scans) — leftover of
  [#17](https://github.com/dubinsky/site-publisher/issues/17).

### Publishing / SEO / analytics

- **GA4**: replace leftover Universal Analytics `UA-…` ids on published sites, then pass `--production` so gtag actually
  runs.
  Do not turn `--production` on until the ids are GA4.

### Markup / dialect parity

- **DocBook**: TODO: implement some DocBook styling parameters (using dot-named frontmatter fields a.b.c :).
- **CSS cleanup / modularize / modernize** ([#18](https://github.com/dubinsky/site-publisher/issues/18)); tei.css still
  wants to merge `em` and `hi`.

### Scan / robustness

- Title-walk candidate index for `Pages.findWalk`.
  Exact published path is a map (`Pages.get`).
  Store includes use a source-path map (`findBySource`).
  Entity `@ref` uses `entityByKindAndId`.
  A link that misses those still walks every page.
  The last segment matches `titleFromPath` or `title`, then ancestors, then a source-path suffix.
  Index the last segment to candidates in list order and keep that ancestor walk.
  A source-path suffix stays a scan.
- `Pages.facsimilePage` still scans for the viewer of a document.
