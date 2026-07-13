## Summary

This re-review covers two commits: dependency/wrapper bumps (`e79627b`) and a bulk correctness pass (`73e578f`) that addresses many prior review findings. Several high-impact bugs look correctly fixed: `PageContent` transformer chaining, asset copy from remapped `source`, `Link` fragment split on first `#`, expanded `Media` extensions, Date `yyyy`, Options bare flags + env prefix, and host-based self-link detection via `Site.uri`. FrontMatter `modified_time` is also effectively safer because decode now runs inside `parse`’s `NonFatal` handler.

The main remaining risk is the **Errors** redesign: treat-as-warnings mode correctly lists errors, but the non-warning path no longer fails during `load()`—it wipes the target and then throws a generic `IllegalStateException` when writing `/errors`, which is a regression versus fail-fast. Several prior items were only annotated with pasted TODOs (unknown XML dialect, `Path.relativize`, HTML escaping, page indexes, atomic publish) and remain open outside this diff’s real code changes. No automated tests were added for any of the fixes.

**Prior findings status (this branch):**
- **Fixed:** #1 transformer fold, #2 asset source path, #3 link fragment split, #5 self-link host compare, #6 Media extensions, #8 modifiedTime crash (via parse path), #9 Date `yyyy`, #10 Options bare flags / env prefix.
- **Partially fixed / regression:** #4 Errors page (renders under treat-as-warnings; fail path is incomplete and riskier).
- **Not fixed (TODO comments only or untouched):** #7 unknown XML, #11 relativize, #12 HTML escaping, #13 indexes, #14–#21 and other structural suggestions.

## Issues

### Issue 1 -- Severity: bug
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Errors.scala:20-27
- Description: With `treat-errors-as-warnings=false`, `error()` only appends and logs; it no longer throws. `Site.generate()` still calls `load()`, then `Files.deleteDirectory(targetDirectory)`, then writes pages. The fail path is deferred to `syntheticContent`, which throws `IllegalStateException("There were page errors")` when the Errors page is written. That is worse than the old fail-fast behavior: the output tree is wiped, only earlier pages (e.g. embedded assets; Errors is near the front of the list) may be written, diagnostics are not rendered as the Errors page, and the thrown type/message loses the structured `PageError` list (they were only log lines). Under treat-as-warnings the new listing UI is good; the non-warning control flow is the problem.
- Suggestion: Keep collecting during the run. Before wiping/writing, if `!treatErrorsAsWarnings && errors.nonEmpty`, fail with a summary (or rethrow the first/`PageError` aggregate) **without** deleting `targetDirectory`. Optionally still allow writing the Errors page only when treating as warnings. Prefer `generate()`-level gate over throwing from `syntheticContent`.
- Status: open

### Issue 2 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Errors.scala:28-29
- Description: Kind sections use `byKind.keys.toList.intersect(PageError.all)`. `intersect` preserves the order of the receiver, so section order follows `Map` key iteration (undefined), not the intentional `PageError.all` order. Title still says “by kind,” but presentation order will jitter across runs/JVMs.
- Suggestion: Use `PageError.all.filter(byKind.contains)` (or `flatMap(k => byKind.get(k).map(k -> _))`) so kinds always appear in `PageError.all` order.
- Status: open

### Issue 3 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/util/Media.scala:4-8
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/markdown/WikiLinksProcessor.scala:61-62
- Description: Extension sets are lowercase only; `Media.isImage` / `isAudio` do not normalize case. Wiki embeds pass the raw extension from `Files.nameAndExtension(ref)`, so `![[diagram.PNG]]` or `.JPEG` still fail to embed after the expansion fix.
- Suggestion: Compare with `extension.toLowerCase(Locale.ROOT)` (or store/lookup lowercased keys) in `isImage`/`isAudio`.
- Status: open

### Issue 4 -- Severity: suggestion
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/markup/InternalLinksProcessor.scala:28
- Description: Host-only compare fixes the previous dead self-link check for typical `https://host/...` configs, but still misses scheme mismatch (`http` vs `https` same host), null-host configs (`URI` with no host → `getHost == null` never matches real hosts, or matches other null-host URIs if both are null), and does not normalize IDN/case. Also does not treat same-origin path-prefix site roots specially (usually fine). Residual false negatives/positives vs the prior suggestion of scheme+host (and optional path prefix).
- Suggestion: Normalize `site.uri` once at config load (require absolute hierarchical URI with host). Compare scheme+host case-insensitively; document expected `url` shape in config.
- Status: open

### Issue 5 -- Severity: nit
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Pages.scala:14-19
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Pages.scala:195-198
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Path.scala:43-49
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/Site.scala:99-101
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/xml/XmlWriter.scala:222-228
- Description: Large blocks of prior review text were pasted as multi-line TODOs without implementing the fixes. They bloat the source, will go stale, and can imply work was done when only annotations changed (e.g. unknown XML still uses `markup.get`).
- Suggestion: Keep short actionable TODOs (one line) or track work in issues; implement or drop the long paste.
- Status: open

### Issue 6 -- Severity: nit
- File: /home/dub/Podval/site-publisher/src/main/scala/org/podval/tools/publish/PageError.scala:36-38
- Description: File ends without a trailing newline (diff shows `\ No newline at end of file`); minor style/tooling hygiene.
- Suggestion: Add a final newline.
- Status: open
