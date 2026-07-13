# Review Summary

- **Mode**: codebase (working tree had only a trivial dependency bump; reviewed the full site-publisher sources)
- **Target**: `src/main/scala` (+ tests, build, README)
- **Files reviewed**: ~120 Scala sources (~4.8k LOC)
- **Issue counts**: 10 bugs, 9 suggestions, 2 nits

## Top issues

- **[bug]** `PageContent.scala:28` — transformer fold ignores intermediate results
- **[bug]** `AssetWithSourcePath.scala:9` — copies from remapped path, not original source
- **[bug]** `Link.scala:42` — multi-`#` fragments split on last `#`, nested sections break
- **[bug]** `Errors.scala:18` — collected warnings never rendered on Errors page
- **[bug]** `InternalLinksProcessor.scala:28` — self-link check compares host to full `site.url`

See the full review at: /tmp/grok-review-4fa6c3a9.md
