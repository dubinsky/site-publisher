# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

- The gear groups term definitions and expansions under an In the text heading, and the marked apparatus choice is
  labeled Marked.
- The collection count line uses the cardinal of `document` (`Document: 1.`, `Документа 22.`, `Документов 11.`).
  The graph status keeps `{n}`, `{e}`, `{nodes}`, and `{edges}` and picks the node and edge forms in the browser.
  A missing form uses the one form and warns.
- One terms catalog replaces `Selector.xml` and `Messages.xml`.
  Built-in axes are `document`, `name`, and `note`.
  A site `_terms.xml` adds axes.
  `ref` and `plural="yes"` on a store or an `entityLists` catalog name it from the lexicon plural.
  The heading is that plural in sentence case.
  An authored name stays as written.
  Pin xml 0.7.0.
- The tree index is titled Tree, or «Дерево» when `lang` is Russian.
  It no longer uses the root store’s `by/@selector` plural.
- The place map is `/map.html`, with leaf alias `/map`, when any place has a `geo`.
  It is not drawn on the place list.
  The list does not link to it.
- A markup file at a name-list path, or at `map`, is rendered above that generated page.
  An `h1` or front-matter title replaces that page's heading.
  The catalog TOC stays the list `<head>`.
  A catalog with one non-empty list takes the file on the catalog page and still does not publish `{n}.html`.
- A file named `{root}-collections` or `{root}-index` is rendered above that generated list, as `{root}-years` already
  is.
  A title in the file replaces the page title.
- The flat index is titled Collections, or «Коллекции» when `lang` is Russian.
- One root store does not install `/collections` or `/years`.
  That store stays off the path text of its descendants.
- The collection count line states how many documents there are.
  Missing photographs, split into empty leaves and leaves with text, and the scanned leaves of a document that has no
  transcription, stay on that line.
  Each non-zero leaf count opens one link per leaf.
  A zero count is left off.
- The page-break icon and folio label stay on one line.
  If they do not fit at the end of a line, both move to the next line.
- A Markdown `<date>` with `@when`, `@from`, `@to`, `@notBefore`, or `@notAfter` gets the same calendar hover as a TEI
  date.
  The words on the page stay as written.
  A `<date>` in code stays as written.
  A hover tip inside a heading is not part of the section id or the table-of-contents title.

## [0.8.0] - 2026-10-15

- Pin xml 0.6.0.
- A name-list page heading is the list title.
  The `name` selector word and the axis line (`имя:`) are not shown.
- A `{base}+reg` file is a regularized reading of `{base}`, not a language.
  It keeps the same `xml:lang`, is not a collection row, and is left out of prev/next and search.
  It shares the original's facsimile.
  The original's header links `[reg]`.
- A `{base}-{xx}` suffix and a site-root language directory (`en/notes/about.md`) are two readings of one page.
  The header links them by autonym, both ways, including outside a collection.
  The collection Язык cell stays the code.
  `html lang` is `text/@xml:lang`, then the suffix, then that directory, then front matter, then the site language.
  Disagreeing signals on the file are one `Wrong language` error.
  A cluster emits `hreflang` for every member, including itself, and `x-default` for the unsuffixed page.
  Canonical stays the page itself.
  There is no redirect.
- Generated chrome follows the site language in `_site_config.yml`.
  English is the fallback.
  The gear, the collection table, and the search page use that language.
  Scripts read the sentence from the page.
- The gear checkbox that shows abbreviation expansions also shows corrections in the text.
  Off, the correction is a hover tip and the source word keeps its underline.
  Print and PDF still show the correction.
- Search keeps a `break="no"` line-break hyphen and stores the joined spelling as the next word.
  The joined spelling is letters and numbers only, so punctuation stays on the hyphenated word.
  Consecutive `break="no"` hyphens are one word.
  An ordinary hyphen, a hyphen before a normal line break, and a maqaf are not joined.
- `dir`, alignment, and `data-interface-lang` follow the site language.
  When the document language and the site language differ, chrome uses the interface language and the page body uses the
  document language.
  A content `xml:lang` is copied to HTML `lang`.
  `<code lang>` stays the highlight language.
  A citeproc locale that is missing falls back to `en-US`.
- A page break shows the images icon and the leaf name.
  The name is smaller than the transcription.
  Hovering a scan shows the leaf name and «Факсимиле.».
  A missing photograph keeps its own line, with that icon faded, the leaf name, and «Нет фотографии.».
  The line is not a link.
  Hovering it shows the same sentence.
  An empty leaf still adds «Пустая.».
- Markdown `@key` inside inline code or a fenced block stays as written.
  A citation in the surrounding prose is unchanged.
- A collection index states how many documents there are, how many have a transcription, and how many have a scan.
  A missing photograph, split into empty leaves and leaves with text, and the scanned leaves of a document that has no
  transcription, are counts above the table.
  Each non-zero count opens a list of links.
  A zero count is left off.
- A missing photograph is a line in the transcription: the leaf name, then «Нет фотографии.».
  An empty leaf adds «Пустая.».
  The line is not a link.
  While the facsimile pane is open, that line hides the scan and shows «Нет фотографии.».
- A place list draws a map of places that have a coordinate.
  The area of a circle is the number of cases that name the place.
  A place with a coordinate and no such case is a ring.
  A place far from the others stays in the table.
  The coordinate is TEI `geo`: latitude, then longitude.
  Rivers and the coast come from `_place-map-base.txt` at the site root.
  Without that file, the map has no such lines.
- A file at the year page's path is rendered above the generated list.
  The page title stays the year-page title when that file has none.
- A link to a generated page keeps its fragment.
  The page has no authored ids to check the fragment against.
  The year index keeps such a fragment when a file is rendered above the list.
  Those ids are on the list.
  A fragment that an authored page does not contain is still left off.
- Markdown pretty-print leaves a table as written, including one that shares a paragraph with the text above it.
  A hard break in a blockquote keeps that quote's marker.
- A facsimile page with no transcribed text stays in the side-by-side pane.
  While the pane is open, that page is a screen tall, so scrolling reaches its scan.
  A missing photo does not grow.
- Archive paths use `/`, in the spelling of the document breadcrumb.
  That spelling is the backlink group, the flat index, search hits, the year index, the citation, and published
  `sourceDesc`.
  A backlink group stays one link to the collection.
- The current step on the path is the same weight as the steps before it.
- Entity lists sort by the heading, the first name element, in the site language.
  The same heading keeps file-id order.
- `@stable="true"` on a `person`, `place`, or `org` keeps that file's id when the heading differs.
  That file is left off the misnamed-entity report.
- TEI `cell/@rows` greater than 1 becomes HTML `rowspan` of the same integer.
  `rows="1"`, `rows="0"`, and a non-numeric value do not set `rowspan`.
- TEI `unclear` with `@cert` or `@reason` shows that text in a hover tip.
  `@cert` is lowercased onto `data-cert`.
  `medium` and `low` gain a dotted underline.
  `unknown` gains a dashed outline.
  `high` keeps the highlight only.
  Print hides the tip.
  The Errors page still lists every `unclear`, and the line names the cert and the reason when they are present.
  A `@hand` on that reading keeps its own tip inside the unclear tip.
- A link that contains a facsimile page break is split around that icon.
  Both halves keep the link.
  The icon stays between them.
  A name without `@ref` is not split.
  `id` and `xml:id` stay on the first half.
  Both halves are listed on the entity page.

## [0.7.0] - 2026-10-13

- A site with one root store leaves that store off the path of its descendants.
  A collection document with no title keeps its number on that path, not on a separate line.
- Each TEI document is also published as XML beside the HTML, with a generated header (publication, source, language,
  calendar, revision).
  The document header links to that file.
- A TEI document's breadcrumb line has the TEI link, a GitHub link, and a Cite button that copies the citation.
  Those three have tooltips.
  The citation is not shown.
  The copied text names the build's HEAD commit as `Git hash` and includes the file URL.
  Other authored pages link to the file on GitHub when `origin` is a GitHub repository.
  There is no `repository` config key.
- On a wide window the transcription shows the current scan to the right of the text.
  The text column stays the same width.
  The gear item "Show the facsimile beside the text" hides it.
  It is on unless the reader turns it off.
  A page-break icon selects that scan while the pane is open.
  It opens the viewer when the pane is closed.
  The header facsimile icon always opens the viewer.
  Print and PDF do not include the pane.
- The year index is a closed outline: a year line with a count, then a month line with a count.
  A day line appears when three or more documents share that exact day.
  Undated documents are grouped by archive.
  A same-year range that crosses months is listed under each of those months.
- The `/search` alias rewrites `/search` and `/search.html` to the search page, and leaves `/search.json` as the index
  file.
- Opt-in search is off unless `search.enabled` is set.
  It writes `/search.json` over the diplomatic text and puts a search form in the header.
  The header field carries a magnifying-glass button.
  The search page does not describe stemming or server search.
  Dates are not converted between calendars, and there is no 4 MB size limit.
- The generator writes `{root}-years.html` for each root TEI store that has original documents, and `/years` when there
  is exactly one such root.
  The row date is the source-calendar label and is not a calendar conversion.
- `TeiDate.filingYears` buckets a date by the years written on its attributes and does not convert calendars.
- A TEI abbreviation expansion is italic inside roman parentheses.
  The gear can hide it.
  The abbreviation then keeps a dotted underline, or its titlo, and the expansion is a hover tip.
  Print always shows the expansion.
- An AsciiDoc deletion is struck through.
  Parentheses stay the abbreviation mark.
- A TEI document header has a Перевод line from `editor` with `role="translator"`.

## [0.6.0] - 2026-10-08

- Every in-page jump lands below the site header.
  A footnote and the mark it points at clear it the same way a section does.
- A collection column title stays in its row, and sticks just under the site header when the page scrolls.
  The title bar covers a date or a name scrolling under it.
- The gear menu no longer wraps Additions, Deletions, and Sources in the apparatus marks.
- A document-header line is omitted when its value has no text.
- A generated table of contents is a sticky rail beside the text when the window is at least 1100px wide.
  On a narrower window it stays at the top.
  An in-page jump lands below the site header.
- Source footnote numbers stay the text color and roman.
  Encoder footnote numbers are italic and violet.
  Danger red stays for a missing name, an unresolved citation, and a deletion.
- The gear menu groups color scheme, reading, and apparatus.
  A rule sits only between groups.
  Apparatus order is Additions, Deletions, then Sources.
  Distinct is labeled with the marks: `[Distinct]`, `(Distinct)`, `<Distinct>`.
- The feed icon sits at the end of the description.
  The license is the line under that.
- The site header stays on screen.
  Collection column titles clear it.
- An archive path is one line of short names.
  The current node keeps its long title.
  A document axis does not add a trailing label.
- A collection index scrolls sideways on a narrow screen instead of squeezing its columns.
  Part titles are section dividers.
- A name in the text uses the text color and a light underline, and the link color on hover.
- do not break a prose sentence on a title, an initial, a dotted abbreviation, or a following parenthetical label
- The footer drops its heading, lines the description up with the other columns, and shows the feed as an icon.
- Breaking config: scalar `license` and top-level `license-link` are gone.
  `license` is a nested record (`name`, `link`, `holder`, `holder-link`).
- fix: do not break up AsciiDoc macros when pretty-printing

## [0.5.0] - 2026-10-08

- The gear menu sets the color scheme: System, Light, or Dark.
  Nothing saved, and System, follow the operating system.
  Light and Dark stay as chosen.
  The choice is stored in the browser and applied before paint.
  Dark overrides the CSS tokens.
  Print and PDF stay on the light palette.
  Highlight.js, Mermaid, and the page graph follow the resolved scheme.
- CSS is split into tokens, base, chrome, prose, TEI, and print.
  The facsimile column is a separate sheet on the viewer page.
  TEI styles stay on every page because transclusion copies those elements into the host.
  Page chrome is `article.page` / `page-header` / `page-body`.
  Microformats and schema.org microdata are gone; JSON-LD stays.
  The header is a flex bar with grouped links and a menu button.
- fix: MathJax delimiter;
- TEI `correspAction type="received"` supplies the addressee.
  `persName role="addressee"` still does when no action is `received`.
- TEI `note place="foot"` is a source-footnote series, separate from `place="end"`.
- A TEI entity-list heading may be `head` or `title`.
  It is written as `head`.
- An endnote in `teiHeader` is not an article footnote.
  A `note` on the header date, abstract, or addressee is a footnote on that header cell.
- Pin xml 0.5.0.
- A TEI titlo (`abbr[rend~="titlo"]`) is an overline.
  `abbr` and `expan` cling in the authored TEI writer.
- TEI `@hand="#id"` copies that `handNote` text into a hover tip and `@medium` (`pencil` or `ink`) onto `data-medium`.
  An unresolved pointer is one page error (`/errors.html#unresolved-hand`) and the element stays unwrapped.
- An empty TEI `gap` with a non-empty `@reason` shows one `[…]` mark.
  The lost and illegible bracket rules are gone, so they do not append a second mark.
- Backlink rows open the first mention in the document text.
  A mention that exists only in `teiHeader` leaves the row on the document.
- feat: prose formatting: `--pretty-print` and `prettyPrintSite` also reflow Markdown and AsciiDoc.
  `--width=N` defaults to 120.
  `--width=0` does not wrap.
  `--sentence-per-line` defaults to true.
  `bin/pretty-print` formats those files without a site.
  File extensions come from the `Markup` instances.
- cleanup: removed JS interpolator;
- chore: dependency updates;
- cleanup: YAML parsing;

## [0.4.0] - 2026-09-24

- `categories` are wiki links to authored pages (`[[Books]]`, `[[Books|label]]`, or a bare name).
  The hub keeps its prose and lists the pages that name it, by title.
  A missing hub is an unresolved link and does not abort generation.
  `![[Note.base]]` and `![[Note.base#View]]` are omitted.
  No synthetic category pages.
- Pin xml 0.4.0.
- `check-links` (default off) requests author-written `http` and `https` URLs and lists failures on the Errors page.
- permalinks cleanup;
- TEI `note/@n` and DocBook `footnote/@label`, when non-blank, are the footnote marker.
  Series order and fragment ids are unchanged.
  The same marker may be used more than once.
- `--pretty-print` and Gradle `prettyPrintSite` rewrite authored TEI and DocBook in place.
  `generateSite` and `serveSite` pass `--pretty-print=false`.
  Unchanged files are not opened for write.
- TEI and DocBook writer configs self-close empty elements.
  Published HTML stays on `HtmlXmlWriterConfig`.

## [0.3.0] - 2026-09-23

- `<date>` accepts `@from`, `@to`, `@notBefore`, and `@notAfter`.
- The hover table's columns match the ends present.
  Uncertainty headers are `Not before` and `Not after`.
- `..` in a temporal attribute is `InvalidDate`.
- Attribute numbers stay in the source calendar (`@calendar="#julian"` or `tei-default-calendar`), not
  Gregorian-normalized.
- Minima chrome and `MarkupPage` wrap are `Xml.Element` via `org.podval.xml.dsl` (inspired by ZIO Blocks HTML).
  Drop wrap-time `.toHtml` and `zio-blocks-html`.
  Pin xml 0.3.0.
- Table-cell footnotes are a per-table lowercase letter series (`a`, `b`, …, `z`, `aa`) with the list under that table (`div.table-with-notes`).
  Markdown still defines those bodies at file end; the published page moves them.
  AsciiDoc `footnote:[…]` stays at the call site.
  Collector `collection-index` and `document-header` stay in the page arabic series.
- A footnote inside another footnote is one level of letters `a`, `b` at the end of the parent note.
  There is no nested hover tip: the outer tooltip shows inner letter markers, not a nested list.
  TEI `note place="end"` inside another `place="end"` does this.
  Markdown and AsciiDoc only if the processor emits an inner stub.
- fix: footnote harvest no longer throws on a marker that lives only inside another note (`Map.apply`).
  Repeated markers emit one body.
  Unknown, orphan, scope-conflict, nesting, and cycle cases are Errors-page kinds.
- Fixture `mermaid.md`: generated HTML keeps the Mermaid module script (newlines, no `&lt;` / `&amp;`).
- xml 0.2.0: HTML `<script>` / `<style>` are raw-text (no wrap, no `&lt;` / `&amp;`).
  Cytoscape module import is multiline; drop the head-JS `<` / `&&` / `//` constraint.
- Opt-in site graph: `graph.enabled` writes `/graph.json` and `/graph.html` (Cytoscape on that page only;
  pan/zoom/click-to-open).
- fix: graph init is `/assets/js/graph.js`, not inline, so HTML escaping of `<` / `&&` cannot empty the canvas.
- Markdown `![[note]]` / `![[note#heading]]` / `![[note#^block]]` transclude authored pages (boxed embed; Settings
  **Seamless transclusions**).
  Missing fragments error; transclusion loops are reported.
  Media `![[file]]` is unchanged.
- Errors page: TOC of kinds when more than one kind is present.
- `entityLists`: members are site-wide (kind + role); catalog with 2+ non-empty lists is TOC only; one list is that
  list; several catalogs are independent.
  Dropped synthetic `/name.html` and the `name`→`names` inbound remap; catalog directory is an identity Worker prefix.
- TEI names without `@ref`, `unclear`, and misnamed entity files are page errors on first parse (Errors page).
  No `/report` harvest, pages, or inbound Worker prefixes.
  Entity-file name elements are not `name without @ref`.
  Each kind on `/errors.html` has a fragment id (`#no-ref`, `#unclear`, …).
- fix: `deploy-alias-worker` copies Worker JS from `publisher/` (empty Gradle root).

## [0.2.0] - 2026-09-16

- Empty Gradle root with sibling `publisher/` (library) and `plugin/` subprojects.
  Maven coordinates unchanged.
  Dogfood: `pluginManagement { includeBuild }` for the plugin id plus settings-body `includeBuild` of the same directory
  for the library child.
- Podval Gradle convention plugins.
- Gradle plugin `org.podval.tools.site-publisher`: `generateSite` / `serveSite` as `JavaExec` on a detached
  `sitePublisher` configuration (Java subproject; publisher stays off the Gradle daemon).
  Dogfood with `pluginManagement { includeBuild }`, not `mavenLocal()`.
  README **Gradle plugin**; design note subsection.
- Backlinks from TEI collections group under the collection path header; snippet text skips `*-tip` subtrees.
- Report harvest: names without `@ref`, TEI `unclear`, entity file id vs underscored main name.
- Inbound `/name` (all entities) and `/name/{id}`; `/report` routes.
  Worker table includes those prefixes.
- Opt-in named windows (`named-windows`): four collector `window.name`s; default off.
- Facsimile viewer: photos scroll in a viewport-filling pane under the header (not a 1060×1586 `resize: both` box).
- Entity page `<h1>` / `<title>` use the first TEI name, not the file name.
- TEI `gap@reason` gets a hover tip (`span.gap-ref` / `span.gap-tip`), same family as date tips.

## [0.1.0] - 2026-09-10

- First Maven Central publication (`org.podval.tools:org.podval.tools.publisher`).
- CLI: `--serve`; default `--log-level` is `INFO` (was `DEBUG`).
- Playwright Chromium cache: `PLAYWRIGHT_BROWSERS_PATH` or `~/.gradle/ms-playwright` (`$GRADLE_USER_HOME/ms-playwright`
  from Gradle).
- GitHub `generate` action resolves the published JAR from Maven Central (does not compile the action checkout); caches
  Playwright browsers.
- README: Maven coordinates, dogfooding `includeBuild`; Gradle plugin plan in `gradle-plugin-plan.md` (not implemented).
- fix: empty-href store/collection scan lists the directory (nested TEI documents were dropped as `NotInStore`)
- TEI `date/@when` (and `..` ranges) get Julian/Gregorian/Jewish hover tables via `org.opentorah:opentorah-core`; site
  config `tei-default-calendar` (`julian`/`gregorian`, default gregorian)
- xml: 0.1.0
- Store index titles use `Selector.pluralOrNames` (singular when there is no plural).
- `Selector.xml` lives here (`page.Selectors`); xml `By(name)` needs that catalog in scope.
- xml: `Named` is `HasNames`
- Stop pre-escaping text into the XML tree (`Feed`, malformed markup).
  Atom `content`/`summary` HTML is CDATA so undeclared `&nbsp;` stays well-formed.
- use `org.podval.store.Selector` for collector `by/@selector` labels; drop the publisher copy of the type and
  `Selector.xml`
- xml: TagSoup is `compileOnly` in the library; declare it here for HTML parse
- xml: extracted to https://github.com/dubinsky/xml (`org.podval:org.podval.xml`); optional `includeBuild` of sibling
  `../xml`
- xml: folded `Pure` into `Stores` (`storesPure` → `stores`, `Stores.With`)
- xml: `org.podval.store` and `org.podval.metadata` (from OpenTorah `core`); store walks and `HasName.bind` are
  synchronous
- xml: `loadCatalog(this, codec)` derives `Foo.xml` / `<Foo>` from the caller's class; `XmlDecode` holds the hand-codec
  helpers
- xml: ignore comments, PIs, and text outside the document element (catalog files with a prologue comment)
- xml: `XmlCodec.decodeCatalog` / `XmlParser.parseCatalog` load a named wrapper and decode each child (Selector catalog
  uses it)
- xml: `XmlParser` loads from URL, file, and classpath; `xi:include` is not expanded (store `xi:include` stays a page
  ref)
- xml: `XmlParserSax` accepts `InputSource` / stream / reader (HTML `parseHtml` from URL or file)
- xml: Gradle subproject `org.podval.xml` with a Schema-derived document binder (`XmlCodec`) over any `XmlAst`
  (unwrapped sequences, leaf-record attributes, `Xml.Element` identity)
- xml: removed `RawXml` / `WithRawXml` / `XmlNode` / `XmlExtras`; leftover parent content is an error
- xml: `XmlCodec` lives in `org.podval.xml`; Selector, collection parts, and entity-lists decode with it
- xml: `XmlTag` binds a field from the element name; `EntityList` uses `kind: EntityKind`
- xml: drop unused `Entity` / `EntityName` / `EntityReference` codecs; collection `part` titles are `Xml.Element`

## [0.0.1] - 2026-

- chore: initial check-in
