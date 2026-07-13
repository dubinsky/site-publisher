## Summary

This is a focused, opinionated Scala 3 static site generator with a clear pipeline (scan → parse/front matter → convert/transform → Minima-style render → write) and a coherent plugin-free design. Architecture and domain modeling are strong for a personal tooling codebase, but several real correctness bugs exist in the content pipeline and asset/path handling, error reporting is incomplete, and automated tests cover almost none of the production surface. Dominant risks: silent pipeline bugs (transformers, assets, link fragments), sparse failure reporting (many TODOs still throw or ignore), O(n) page lookup on large sites, and very thin test coverage.

## Strengths

- **Clear end-to-end pipeline**: `Site` → `Pages.scan` → `PageSource`/`PageContent` → Minima layout → write is easy to follow and matches the project philosophy (small core, no plugins).
- **Markup processor composition is thoughtful**: `Configurer` + `Markup` phase buckets (`converters` / `transformers` / `postConverters` / `htmlConverters`) with ordered `convertLinks` / `transformsFootnotes` is a good extension point without a plugin system.
- **Dialect abstraction**: `XmlAst` + `XmlDialect` (transform/gather/render) cleanly separates XML/HTML/TEI concerns; `XmlParser`/`XmlWriter` are substantial and reusable.
- **Obsidian-oriented features**: wiki links, block anchors, posts/daily-notes path remapping, stand-alone front matter, backlinks — real product value beyond a minimal SSG.
- **Front matter design**: typed schema with unknown-key preservation and intentional `modified_time` workaround shows practical Obsidian interoperability.
- **Error page + treat-as-warnings mode**: good idea for iterative authoring (even if incomplete — see issues).
- **Scala 3 style**: modern syntax, `strictEquality`, focused types (`Path`, `Page` hierarchy, sealed `Date` variants).

## Issues

### Issue 1 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/page/PageContent.scala:28-30
- Description: Transformer fold does not chain results. Each transformer is applied to the original `xml`, and intermediate `result` values are discarded. With a single transformer this works; with two or more, only the last transformer’s output is kept and earlier transforms are lost.
- Suggestion: Change to `markup.transformers.foldLeft(xml)((result, transformer) => transformer.transform(result, this))`. Add a unit test with two dummy transformers asserting order and composition.
- Status: open

### Issue 2 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/page/AssetWithSourcePath.scala:9
- Description: `write()` copies from `path.file(site.sourceDirectory)` instead of the original `source`. For posts/drafts/daily assets whose destination path was remapped by `Posts.path` (e.g. `_posts/2024-01-01-diagram.png` → `/2024/01/01/diagram.png`), the source file is looked up at the *output* relative path and copy fails or copies the wrong file.
- Suggestion: Use `source.file(site.sourceDirectory)` as the copy source and `path.file(site.targetDirectory)` (via `targetFile`) as the destination. Add a test for a date-prefixed asset under `_posts`.
- Status: open

### Issue 3 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/link/Link.scala:42
- Description: Fragment splitting uses `Strings.split`, which splits on the *last* `#`. Multi-section wiki/section refs such as `Page#Section#Subsection` become path=`Page#Section` and fragment=`Subsection`, so section resolution fails for nested headings (explicitly documented as supported at lines 39–40).
- Suggestion: Split path vs fragment on the *first* `#` (or parse fragments without `lastIndexOf`). Keep `Strings.split` for extension/`|` cases where last-occurrence is correct. Add tests for `name#a#b` and intrapage `#a#b`.
- Status: open

### Issue 4 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Errors.scala:18-31
- Description: Errors are either thrown immediately or stored in `errorsVar`, but `syntheticContent` always renders an empty `div.site-errors`. The Errors page never lists collected errors, so `--treat-errors-as-warnings` loses the diagnostic UI value.
- Suggestion: Render `errorsVar` grouped by `PageError.Kind` (title already says “by kind”). Consider always collecting then failing at end of `generate()` when not treating as warnings, so authors see multiple errors in one run.
- Status: open

### Issue 5 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/markup/InternalLinksProcessor.scala:26-29
- Description: Self-link detection compares `uri.getHost` (hostname only) to `content.site.url` (full site URL from config, typically `https://example.com`). Equality never holds for normal configs, so “spurious external link to this site” is effectively dead and internal-looking absolute site URLs are treated as external.
- Suggestion: Normalize `site.url` to a host (or full origin) once at config load; compare scheme+host (and optional path prefix). Cover with unit tests for `https://site.example/foo` vs config url.
- Status: open

### Issue 6 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/util/Media.scala:4-8
- Description: Wiki embeds only recognize `jpg` as images and `ogg` as audio. Common extensions (`png`, `jpeg`, `gif`, `webp`, `svg`, `mp3`, `wav`, `m4a`, …) fall through to `None`, so `![[diagram.png]]` does not embed.
- Suggestion: Expand extension sets (or derive from a small media registry shared with icon mapping). Test `WikiLinksProcessor.embed` for png/jpg/webp and a non-media type.
- Status: open

### Issue 7 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Pages.scala:188-190
- Description: Unknown XML root element uses `markup.get`, throwing `NoSuchElementException` instead of a `PageError`. A stray or unsupported `.xml` file aborts the whole build with an opaque stack trace rather than a path-scoped diagnostic.
- Suggestion: On `None`, report `PageError.FileKind` (or similar) and skip/add a malformed placeholder page, consistent with `MarkupKind.readAndParse` parse failures.
- Status: open

### Issue 8 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/page/FrontMatter.scala:39
- Description: `modifiedTime` calls `Date.codec.decodeValue` outside the safe `parse` path. Invalid `modified_time` YAML throws at content access time and can crash generation mid-page rather than producing a front-matter error.
- Suggestion: Decode with try/Either and report via `site.error` / return `None`. Prefer a first-class schema field once camelCase/`modified_time` mapping is fixed.
- Status: open

### Issue 9 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/util/Date.scala:15
- Description: Short display format uses pattern `LLL d, YYYY`. `YYYY` is the *week-based* year; dates near year boundaries (e.g. 2024-12-31) can display the wrong calendar year.
- Suggestion: Use `yyyy` (calendar year). Add a regression test around Dec 29–Jan 3 for week-year edge cases.
- Status: open

### Issue 10 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/util/Options.scala:11-13
- Description: Option parsing assumes every `--…` arg contains `=`. Flags like `--include-drafts` or `--production` (without `=true`) throw `StringIndexOutOfBoundsException` because `eqIndex == -1`.
- Suggestion: Support bare `--flag` as boolean true; validate unknown options; use the constructor’s `environmentVariablesPrefix` instead of hardcoding `SITE_PUBLISHER_` in `option()` (line 18).
- Status: open

### Issue 11 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Path.scala:42-49
- Description: `relativize` does not normalize `.` / `..` (TODO at line 43). Alias / permalink values such as `../../other` can escape the intended URL tree and write outside the logical site layout (path traversal via content metadata).
- Suggestion: Normalize segments (drop `.`, resolve `..` with bounds check); reject aliases that escape site root; never allow `..` to leave `targetDirectory` when resolving write paths.
- Status: open

### Issue 12 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/xml/XmlWriter.scala:222-223
- Description: For HTML dialect, `encodeXmlSpecials` is disabled (`HtmlXmlDialect` default). Titles, tags, authors, and other front-matter strings rendered via the HTML DSL are written without escaping text nodes. Attribute quoting (`Strings.quote`) also does not escape `"`, so a title containing `"` can break attributes. For untrusted or multi-author content this is XSS/HTML injection risk; even for trusted content it can corrupt markup.
- Suggestion: Escape text and attributes on render (use full `Strings.escape` for attributes). Prefer encoding on output always; only skip for preformatted trusted raw HTML islands if needed.
- Status: open

### Issue 13 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Pages.scala:21-22, 219-224
- Description: Page lookup is linear (`pages.find`) and `find(..., kind)` ignores `kind` entirely. Link resolution, directory child listing patterns, duplicate detection, tags, and posts all scan full lists. For large vaults this is quadratic overall (each page × each link × all pages).
- Suggestion: Build indexes after scan: by exact path, by file name / title / titleFromPath, optionally by `LinkKind`. Use them in `get`, `find`, Tags, Posts, DirectoryPage children.
- Status: open

### Issue 14 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Pages.scala:116-118, 149, 158
- Description: Several structural conflicts are TODO’d but not enforced: dual internal+external index, index when directory is emptied (posts folder), multiple markup files for one name, multiple stand-alone front-matter files. Today the code silently picks `.head` / first wins, which can hide author mistakes.
- Suggestion: Emit `PageError`s for these cases (even if build continues under treat-as-warnings). Prefer deterministic “primary wins” only after explicit warning.
- Status: open

### Issue 15 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Site.scala:98-108
- Description: `generate()` deletes the entire target directory, then writes page-by-page. A crash mid-write leaves a partial site; concurrent readers (local server, CI publish) can observe a wiped tree. No temp-dir + atomic rename.
- Suggestion: Write to a staging directory (or `_site.tmp`) and atomically replace `_site`. Optionally preserve mtimes for unchanged assets to speed deploys.
- Status: open

### Issue 16 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/page/PageSource.scala:15-32
- Description: Content is held in `SoftReference`. Under memory pressure the full convert/transform pipeline re-runs. Pipeline is mostly deterministic for IDs, so correctness may hold, but large sites pay repeated parse/convert cost and any future non-determinism (or caching of backlink contexts) becomes fragile.
- Suggestion: Use hard references for a generation, or an explicit two-phase model (metadata first, content on demand with a generation-scoped cache). Avoid SoftReference unless profiling shows need.
- Status: open

### Issue 17 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Feed.scala:22-24
- Description: Atom feed is a stub: root `<feed>` with xmlns only; entry generation is fully commented out. Sites still get `/feed.xml` linked from the footer, but feed readers get an empty/invalid feed.
- Suggestion: Either implement entries (reuse `Posts.posts`, titles, dates, absolute URLs from `site.url`) or omit auto-adding Feed until ready. Validate with a feed checker.
- Status: open

### Issue 18 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/test/scala/org/podval/tools/publish/markdown/MarkdownSpec.scala:13-24
- Description: Test coverage is essentially only `FrontMatterSpec` plus a Markdown smoke test that asserts `true` after `println`. No tests for `Path`, `Link.resolve`, wiki links, posts path mapping, `Pages` scan/conflicts, internal link resolution, or XML parse/render.
- Suggestion: Prioritize pure unit tests for `Link.resolve`, `Posts.path`, `Strings`/`Path.relativize`, `WikiLinksProcessor`, and `PageContent` transformer chaining — these need no filesystem. Add a small fixture-site integration test under `src/test/resources`.
- Status: open

### Issue 19 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Configurer.scala:42-46
- Description: `--configurer=Name` loads an arbitrary class via `Class.forName` and instantiates it. That is a powerful extension hook but is unrestricted reflective loading of user-controlled class names.
- Suggestion: Restrict to a known package allowlist, or require a service-loader / sealed registry of configurers. Document that the option is trusted-local-only.
- Status: open

### Issue 20 -- Severity: nit
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Site.scala:167-178
- Description: Development `@main def generate()` hardcodes a personal site path. AGENTS.md already warns not to commit path changes; still easy to leak local paths or accidentally run the wrong site.
- Suggestion: Keep hardcoded paths out of committed sources (local run config / env only), or gate behind an env var with no default path in git.
- Status: open

### Issue 21 -- Severity: nit
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/page/Page.scala:110-126
- Description: Commented-out duplicate `className` assignment and a TODO about duplicate class attributes indicate incomplete migration to the zio-blocks HTML attribute API; easy to reintroduce invalid HTML when uncommenting experiments.
- Suggestion: Clean up dead comments; assert rendered `<a class=...>` has a single class attribute in a small render test.
- Status: open

## Improvement roadmap

### Short-term (correctness + safety)
1. Fix transformer fold chaining, asset copy source path, multi-`#` link fragment split, self-link host compare, date `yyyy`, Options bare flags.
2. Render collected errors on the Errors page; prefer collect-then-fail over fail-on-first when not in warning mode.
3. Expand `Media` image/audio extensions; treat unknown XML dialect as `PageError`.
4. Guard `modifiedTime` decode; normalize/reject `..` in aliases.
5. Add focused unit tests for `Link`, `Posts.path`, `PageContent` transformers, `WikiLinksProcessor`, `Path.relativize`.

### Medium-term (structure + performance)
1. Build path/title indexes after scan; use them for `find`, tags, posts, directory listings.
2. Enforce structural scan errors currently marked TODO (duplicate markup, dual index, etc.).
3. Complete Atom feed (or stop advertising it); add basic SEO/feed meta TODOs in `MarkupPage` if still desired.
4. Atomic publish (staging directory + rename); optional unchanged-asset skip.
5. Replace SoftReference content cache with an explicit generation-scoped cache.
6. Escape HTML text/attributes on render by default.

### Longer-term (product completeness + architecture)
1. TEI depth: section TOC (`TeiMarkup.sections`), entity-link kind filtering in `find`, facsimile/entity converters’ TODOs.
2. Transclusion beyond image/audio stubs; topological page order (TODO in `Site.load`).
3. Directory “store”/structure-driven listing (TODOs in `Pages.scan`) if TEI collections remain a goal.
4. Grow integration tests against a golden fixture site (links, posts, tags, sitemap, assets).
5. Keep the plugin-free philosophy: prefer new `Processor`s + `Configurer` variants over a general plugin SPI; if external configurers stay, constrain loading.
6. Documentation: README is still aspirational/TODO-heavy — document CLI flags, front-matter schema, and post/daily filename rules next to the code that enforces them.
