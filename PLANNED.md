# Planned and considered

Inventory of site-publisher work that is planned or considered and not yet implemented.
Drawn from GitHub issues, the design note (`dub.podval.org/notes/Publishing/Site Publisher.md`), `README.adoc`,
`AGENTS.md`, and source TODOs.

## Product features

### Graph (explicit follow-ups)

- **Local graph** (“PR 4”): `?focus=/notes.html` on `/graph.html`, and/or a per-page overlay.
  v1 is the global page only.
  Overlay must not load Cytoscape + JSON on every TEI page.
- **Arrows checkbox**: JSON is already directed so a later setting can show A→B vs B→A.
  The canvas currently draws undirected strokes.

### Tags and categories ([issue #10](https://github.com/dubinsky/site-publisher/issues/10))

- Handle **categories** (possibly as wiki links).
  Front matter already parses `categories`; nothing consumes them.
  `LinkKind.Category` is unused.
- **Auto-create category pages.**
- **Per-tag pages.**
  There is one synthetic `/tags.html` with fragments, not `/tags/{tag}.html`.
  `LinkKind.Tag` is unused.

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

- **X Articles** — publish a page to X if/when there is an API (design note +
  [#21](https://github.com/dubinsky/site-publisher/issues/21)).
- **SEO images, Facebook, webmaster proofs** — documented as absent.
- **GA4**: replace leftover Universal Analytics `UA-…` ids on published sites, then pass `--production` so gtag actually
  runs.
  Do not turn `--production` on until the ids are GA4.
- **Package a CLI** ([#20](https://github.com/dubinsky/site-publisher/issues/20) leftover).
  Library + Gradle plugin are on Central; a packaged CLI is not.

### Markup / dialect parity

- **Obsidian `|WIDTH` / `|WIDTHxHEIGHT` on `![alt](src)`** — FlexMark leaves the size in `alt`.
  Wiki `![[image]]` already gets `width`/`height`.
- **Markdown native in-document bibliography** — none; citeproc only.
- **DocBook**: no task lists, wiki links, or PDF embeds.
  Front matter TODO: implement some DocBook styling parameters (using dot-named frontmatter fields a.b.c :).
- **`HtmlIr.normalize` for TEI and DocBook** — not run yet (leftovers stay native names until dialect converters).
- **Dotted front-matter keys** (`toc.depth`, …) — considered, not done.
- **`modified_time` as a typed field** — blocked on ZIO Blocks YAML kebab-case
  ([#6](https://github.com/dubinsky/site-publisher/issues/6)); currently an extra-key workaround.
- **Load less CSS/MathJax** by dialect actually present on the page (today every HTML page always ships all of it,
  because of transclusion).
- **Configurable HTML converter** (commented `Configurer` in `Site`).
- **CSS cleanup / modularize / modernize** ([#18](https://github.com/dubinsky/site-publisher/issues/18)); tei.css still
  wants to merge `em` and `hi`.

### Scan / robustness

- Page lookup indexes (path / title / `LinkKind`) — entity lookup is indexed; general `get`/`find` is still linear.
  `Pages.get` still walks the page list (`TODO make a map for quick lookups`).

### Research only ([#21](https://github.com/dubinsky/site-publisher/issues/21))

- Look at Steph Ango’s vault, MkDocs Material, KaTeX as a MathJax alternative.
- README still has open questions about Obsidian wiki-link case sensitivity, titles vs file names, ambiguous names,
  wrapping, agglutination — not a committed feature list.
- An Obsidian plugin so the editor also resolves by front-matter title (vault-side, not the generator).

## Not features of the publisher (rollout / docs)

- README “Opinionated” section: “TODO expound.”
- Narrative TODOs in the design-note intro (dates, 11ty link, Jekyll plugin link) — writing, not product.

## GitHub issues that already shipped (still open)

Treat these as stale trackers, not remaining work: transclusion (#11), named windows (#12), hover tables (#13), gap tips
(#14), `/errors.html` reports (#16), facsimile document viewer (#17), Maven artifact (#20, except CLI), entity
first-name titles / `/name` catalog (#15 mostly), backlinks grouped by collection (#9 mostly), `Content` kinds (#8).
The `..` encoding is withdrawn.
