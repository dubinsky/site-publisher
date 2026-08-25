# Site Publisher Agent Guidelines

Static site generator written in Scala 3 + Gradle. Produces sites from Markdown, AsciiDoc, HTML, and TEI using a Minima-inspired layout.

## Project Layout

- Main logic: `src/main/scala/org/podval/tools/publish/`
  - `markup/` — `Markup` dialects, shared IR (`Citation`, `Footnote`, `Glossary`, `Callout`, `Admonition`, `Aside`, `Quote`, `Section`, `Toc`, wiki links), and `Bibliography` resolution
  - `page/` — `PageContent`, front matter, chunking, PDF pages
  - `site/` — `Site`, `Pages`, config, sitemap, errors
- Supporting libraries in the same repo:
  - `org.podval.xml` — dialect-aware XML (parsing, writing, transform/gather, Xml2Html)
  - `org.podval.tei` — TEI XML dialect + entity handling
- Resources (CSS): `src/main/resources/org/podval/tools/publish/site/assets/css/`
- Tests: `src/test/scala/...` — ScalaTest `AnyFunSuite`
- Fixture site: `src/test/site` (committed). `SiteSpec` generates into `build/test-site` via an absolute `--target-directory-name` (gitignored under `build/`). Do not generate into `src/test/site/_site` and do not point tests at real sites. ScalaTest classes must be named `*Spec` or Gradle will not run them. One fixture page (`glossary.md`) has `pdf: true`; `SiteSpec` opens that Chromium PDF with PDFBox (page count, a named dest, a string of text). Not visual diffs, not every page.

## Build & Run Commands

```bash
./gradlew build                 # compile + test
./gradlew test                  # run tests only
./gradlew clean
./gradlew run --args="/path/to/source-directory"
```

The application expects a source directory containing `_site_config.yml` as its first (and only) positional argument. `Site.main` is `Site(SiteOptions.forArgs(args)).generate()`.

### Development entry point (`generate`)

`@main def generate()` in `Site.scala` hardcodes a local site path and serves it:

```scala
@main def generate(): Unit = Site(SiteOptions(
  sourceDirectoryPath =
    "/home/dub/OpenTorah/opentorah.org/docs",
  logLevelOpt = Some("INFO"),
  treatErrorsAsWarnings = true
)).serve()
```

Run from IntelliJ (that `@main`) or via `./gradlew run` (CLI `Site.main`, needs `--args`). Modify the path in `generate()` temporarily for other sites during development; do not commit that change.

## Key Concepts for Agents

- `Site` is the top-level coordinator.
- `Pages` scans the source tree, builds the `Page` graph (including synthetic pages like `/posts`, `/tags`, `/errors`, `sitemap.xml`, etc.).
- Content pipeline: dialect `Markup` (md / adoc / html / tei) → `FrontMatter` + `Xml.Element` → dialect converters emit shared IR → `PageContent` (sections, links, footnotes, glossary, citations) → Minima-inspired HTML → write.
- Special source directories: `_posts/`, `_drafts/`, Obsidian daily-notes folder (configured via `.obsidian`).
- Links: wiki-style `[[...]]`, internal link resolution, backlinks, TOC.
- Citations: dialect syntax → `Citation` IR → the document's front matter `bibliography` (path relative to the source file) and `csl` (no site default). citeproc-java formats; locale is page `lang`, else site `lang`, else `en-US`.
- Chunking (`chunk` / `chunk-depth` in front matter) and PDF are markup-independent.
- Every real page writes its `textContent` (or copies assets).

## When Working on the Code

- Prefer making changes that keep the core small and plugin-free (the project's stated philosophy).
- Markup-specific conversion lives next to the dialect (`AsciiDocMarkup` / `AsciiDocCiteExtension`, `MarkdownMarkup` / `MarkdownCite`, …). Shared IR and resolution stay in `markup/` (`Citation`, `Bibliography`, `Footnote`, `Glossary`, `Section`, …). There is no `feature/` package.
- Keep existing comments when moving or refactoring (TODOs, ordering constraints, "why" notes). Move them with the code they describe. Drop or rewrite a comment only if it is factually wrong; do not delete comments to tidy a diff.
- Documentation split: **design** (pipeline, IR, why) goes in the Obsidian note `dub.podval.org/notes/Publishing/Site Publisher.md` under **Design**, with a per-feature subsection when a feature has IR or non-obvious architecture. **User documentation** (how to run the generator; syntax of each construct in each markup) stays in this repo’s `README.adoc`. Some overlap is expected (IR HTML shape in the note vs HTML/author syntax in the README). Do not put design essays in the README or author syntax in the Design section.
- Run `./gradlew test` before considering a change complete.
- Generated directories (`build/`, `out/`, `target/`, `.gradle/`) are gitignored. With `respect_gitignore = true` they should stay out of searches and listings.
- The `build/` directory can be very large — avoid reading files from it.

## IntelliJ + Grok Build Tips

- Semantic Scala work (rename, find symbol/usages, inspections) goes through IntelliJ MCP. Procedure, including prompting to start the IDE and open this project: `~/.grok/rules/intellij-mcp.md`.
- Run `grok` directly inside the IntelliJ terminal (it is detected as a JetBrains terminal).
- Run `/terminal-setup` inside Grok for diagnostics (clipboard, colors, key handling, etc.).
- JetBrains full ACP integration is "Coming soon". Until then the terminal plus MCP is the supported way.
- Attach files with `@path` (e.g. `@src/main/scala/org/podval/tools/publish/site/Site.scala`).
- Use the project root as the working directory when starting Grok so it picks up `AGENTS.md` and `.gitignore`.

## Style / Conventions (from build.gradle)

Scala 3.8.4, Java 25 toolchain.

Compiler flags:
- `-new-syntax`
- `-feature`
- `-language:strictEquality`
- `-source:future`

## Common Pitfalls

- Do not commit changes to the hardcoded path in `generate()`.
- Posts and daily notes have strict filename conventions (`YYYY-MM-DD-title`).
- Directories that should not produce pages (e.g. `_posts`) are specially handled by `Posts.isDirectoryEmptiedOut`.
- XML dialects are disambiguated by root element for `.xml` files.
- `Site.targetDirectory` is `sourceDirectory / name` unless `target-directory-name` is absolute; Java `File(parent, "/abs")` on Unix does *not* ignore the parent.
- The Errors page is written after other HTML so unknown citations and unresolved links found while rendering appear on it.
- AsciiDoc `cite:[key]` needs a custom inline-macro regexp (empty target); `cite:[k1, k2]` must join all positional attributes, not just `1`.
