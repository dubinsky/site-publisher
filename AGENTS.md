# Site Publisher Agent Guidelines

Static site generator written in Scala 3 + Gradle. Produces sites from Markdown / HTML / TEI sources using a Minima-inspired layout.

## Project Layout

- Main logic: `src/main/scala/org/podval/tools/publish/`
- Supporting libraries in the same repo:
  - `org.podval.xml` — dialect-aware XML (parsing, writing, transform/gather, Xml2Html)
  - `org.podval.tei` — TEI XML dialect + entity handling
- Resources (CSS): `src/main/resources/org/podval/tools/publish/site/assets/css/`
- Tests: `src/test/scala/...` (ZIO Test + specs for Markdown/FrontMatter)

## Build & Run Commands

```bash
./gradlew build                 # compile + test
./gradlew test                  # run tests only
./gradlew clean
./gradlew run --args="/path/to/source-directory"
```

The application expects a source directory containing `_site_config.yml` as its first (and only) positional argument.

### Development entry point (Site.main)

`Site.main` (in `Site.scala`) hardcodes local site paths for quick iteration:

```scala
object Site:
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
    "--treat-errors-as-warnings=true",
    // "/home/dub/OpenTorah/alter-rebbe.org"
    "/home/dub/Podval/dub.podval.org"
    // "/home/dub/Podval/www.podval.org"
  ))
```

Run directly in IntelliJ or via:

```bash
./gradlew run
```

(Modify the path in `Site.main` temporarily for other sites during development.)

## Key Concepts for Agents

- `Site` is the top-level coordinator.
- `Pages` scans the source tree, builds the `Page` graph (including synthetic pages like `/posts`, `/tags`, `/errors`, `sitemap.xml`, etc.).
- Content pipeline: `Markup` (md/html/tei) → `FrontMatter` + `Xml.Element` → `PageContent` (converters + transformers) → `Minima` render → write.
- Special source directories: `_posts/`, `_drafts/`, Obsidian daily-notes folder (configured via `.obsidian`).
- Links: wiki-style `[[...]]`, internal link resolution, backlinks, TOC.
- Every real page writes its `textContent` (or copies assets).

## When Working on the Code

- Prefer making changes that keep the core small and plugin-free (the project's stated philosophy).
- When adding features to a markup dialect, add the corresponding processor(s) in `feature/`.
- Run `./gradlew test` before considering a change complete.
- Generated directories (`build/`, `out/`, `target/`, `.gradle/`) are gitignored. With `respect_gitignore = true` they should stay out of searches and listings.
- The `build/` directory can be very large — avoid reading files from it.

## IntelliJ + Grok Build Tips

- Run `grok` directly inside the IntelliJ terminal (it is detected as a JetBrains terminal).
- Run `/terminal-setup` inside Grok for diagnostics (clipboard, colors, key handling, etc.).
- JetBrains full ACP integration is "Coming soon". Until then the terminal experience is the supported way.
- Attach files with `@path` (e.g. `@src/main/scala/org/podval/tools/publish/Site.scala`).
- Use the project root as the working directory when starting Grok so it picks up `AGENTS.md` and `.gitignore`.

## Style / Conventions (from build.gradle)

Scala compiler flags in use:
- `-new-syntax`
- `-feature`
- `-language:strictEquality`
- `-source:future`

Java 25 toolchain.

## Common Pitfalls

- Do not commit changes to the hardcoded paths in `Site.main`.
- Posts and daily notes have strict filename conventions (`YYYY-MM-DD-title`).
- Directories that should not produce pages (e.g. `_posts`) are specially handled by `Posts.isDirectoryEmptiedOut`.
- XML dialects are disambiguated by root element for `.xml` files.
