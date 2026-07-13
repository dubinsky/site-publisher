# Review Summary

- **Mode**: branch (re-review)
- **Target**: `master` vs `origin/master` (merge-base `d07cf998`)
- **Files reviewed**: 21 (build.gradle, gradle wrapper, Errors, PageError, Pages, Path, Robots, Site, Sitemap, Link, InternalLinksProcessor, AssetWithSourcePath, FrontMatter, PageContent, Date, Media, Options, Strings, XmlWriter)
- **Diff stats**: 21 files changed, 96 insertions(+), 35 deletions(-)
- **Issue counts**: 1 bugs, 3 suggestions, 2 nits

## Top issues

- [bug] Errors.scala:20 -- treat-as-warnings=false no longer fail-fast; wipes target then throws generic ISE
- [suggestion] Errors.scala:28 -- kind section order follows Map key iteration, not PageError.all
- [suggestion] Media.scala:4 -- image/audio extension match is case-sensitive
- [suggestion] InternalLinksProcessor.scala:28 -- host-only self-link compare still misses scheme/null host
- [nit] Pages.scala / Path / Site / XmlWriter -- large pasted TODO blocks without fixes

See the full review at: /tmp/grok-1000/grok-review-4588b541.md
