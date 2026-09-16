# Gradle plugin plan

Not implemented. This is the intended follow-up now that the publisher is a Maven Central
library (`org.podval.tools:org.podval.tools.publisher`).

The SSG itself stays plugin-free. The Gradle plugin is only a wrapper that runs `Site.main`
the way `org.podval.tools.scalajs` wraps Scala.js — it must not load Scala 3, Playwright,
or AsciidoctorJ into the Gradle daemon.

## Why

Sites already have Gradle. Copying a detached `sitePublisher` configuration and a `JavaExec`
into each of them will drift (JVM native-access flags, Playwright env, CLI flags). A plugin
owns that block. Do **not** put the publisher on `implementation`: consumers are not all on
the same Scala version, and Playwright does not belong on the compile classpath.

Until the plugin exists, GitHub Actions resolve the published JAR via
`.github/actions/generate` (Maven Central, not a compile of the action checkout).

## Artifacts

Two artifacts, same version:

| | Coordinates | Role |
|---|---|---|
| Library (exists) | `org.podval.tools:org.podval.tools.publisher` | `Site.main` |
| Plugin (todo) | plugin id `org.podval.tools.site-publisher` | `generateSite` / `serveSite` |

Publish the plugin to the Gradle Plugin Portal and Maven Central. The plugin JAR is Java (or
Kotlin) only. It adds a `sitePublisher` configuration on the **consumer** and depends on the
library there, not on the plugin classpath.

## Extension and tasks

```gradle
plugins {
  id 'org.podval.tools.site-publisher' version '0.1.0'
}
site {
  // default: project directory (must contain _site_config.yml)
  treatErrorsAsWarnings = true
  logLevel = 'INFO'
}
```

OpenTorah `:docs`:

```gradle
tasks.named('generateSite') { dependsOn generateTables }
```

Tasks:

- `generateSite` (`JavaExec`) — `Site.main` without `--serve`.
- `serveSite` — same classpath, `--serve`.

Both set `--enable-native-access=ALL-UNNAMED`, `--sun-misc-unsafe-memory-access=allow`,
`PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=1`, and
`PLAYWRIGHT_BROWSERS_PATH=$GRADLE_USER_HOME/ms-playwright`.

Do **not** make `generateSite` a dependency of `build`. Pages CI calls it explicitly.

## Task outputs

`generateSite` must declare:

- **Output:** the target directory (`_site`, or `site.targetDirectoryName`), so
  `upload-pages-artifact` has a stable path and Gradle up-to-date can skip a run.
- **Inputs:** the source tree minus the target directory (and minus Gradle `build/`).

The generator still deletes and rewrites the target when the task runs. Up-to-date is what
avoids the run when sources are unchanged. Do not pretend the tool is incremental.

## GitHub Actions after the plugin

Site workflows become: checkout, setup-java 25, setup-gradle, `./gradlew generateSite`
(or `:docs:generateSite`), upload ` _site` / `docs/_site`, deploy-pages.

Then drop `.github/actions/generate` (or leave a five-line wrapper). `publish-pages.yml`
keeps the Pages deploy half; generation belongs to the site. Pin the plugin version in
`build.gradle`, not `uses: …@master`.

Cache `~/.gradle/ms-playwright` in CI (the generate action already does this).

## Layout

The library stays at the root of this repo (`org.podval.tools.publisher`). The plugin is a
**subproject** (`java-gradle-plugin` or equivalent, Java or Kotlin only). It must not
`implementation`-depend on the root project: that would load Scala 3 / Playwright /
AsciidoctorJ into the Gradle daemon. The plugin only adds a `sitePublisher` configuration on
the consumer and puts the library coordinate there.

One composite `includeBuild` of this repo then provides both the plugin id and the library
substitution. Do not split the plugin into its own Git repository.

## Local iteration: composite build, not mavenLocal

Use a composite build. Do **not** iterate via `publishToMavenLocal` / `mavenLocal()`.

`mavenLocal()` is how some older Gradle plugins were dogfooded (including
`org.podval.tools.scalajs` in a few checkouts). It is the wrong loop here:

- Every change needs a republish; plugin markers and version caches go stale.
- Consumers would add `mavenLocal()` to `pluginManagement.repositories`. That is easy to
  commit and then CI resolves a developer’s leftover `~/.m2` artifact (or fails without it).
- Publishing both the plugin marker and the library to `~/.m2` just to keep them in sync is
  the job composite substitution already does.
- Sites already `includeBuild` this repo for the library. Moving that include into
  `pluginManagement` is the whole local-plugin story.

### Consumer `settings.gradle`

**One** include, and only under `pluginManagement` (Gradle treats that as a composite member
for dependency substitution too). Do not also `includeBuild` the same directory in the
settings body — “included build is already included”.

```gradle
pluginManagement {
  repositories {
    mavenCentral()
    gradlePluginPortal()
  }
  final File publisherDir = file(
    providers.gradleProperty('sitePublisherDir').getOrElse('../site-publisher')
  )
  if (publisherDir.isDirectory() && new File(publisherDir, 'settings.gradle').isFile()) {
    includeBuild(publisherDir)
  }
}
```

Override with `-PsitePublisherDir=`. OpenTorah / MathWorlds defaults stay
`../../Podval/site-publisher`. When the directory is missing (GitHub Actions), Gradle
resolves the plugin id and the library from Maven Central.

### Consumer `build.gradle`

Always pin a version (CI has no composite):

```gradle
plugins {
  id 'org.podval.tools.site-publisher' version '0.1.0'
}
```

With the include above, a local checkout **substitutes** that version. No version bump, no
`mavenLocal`, no republish to try a one-line plugin change. Then `./gradlew generateSite`
and `./gradlew serveSite`.

Until the plugin exists, keep the current settings-body `includeBuild` for the library
(README **Dogfooding**). The move into `pluginManagement` is part of the first consumer
cleanup.

## Rollout: clean up sites

Do not push a site that *applies* the plugin until the plugin artifact is on Maven Central.
CI does not check out this repo; it cannot includeBuild.

1. Add the plugin subproject here. Gradle TestKit: apply the plugin to a temp project,
   assert `generateSite` / `serveSite` exist, the `sitePublisher` configuration is not the
   compile classpath, and `generateSite` declares the target-directory output.
2. Dogfood locally on one site (typically `dub.podval.org`): move `includeBuild` into
   `pluginManagement`, apply the plugin, delete the copied `configurations.sitePublisher` /
   `JavaExec` / `siteArgs` / Playwright env / JDK flags, run `generateSite` and `serveSite`.
   Do not push that site yet.
3. Publish this repo to Central (library + plugin, same version), including the plugin
   marker POM (`org.podval.tools.site-publisher.gradle.plugin`).
4. Push each consumer: `plugins { id 'org.podval.tools.site-publisher' version '…' }`,
   `pluginManagement { includeBuild(...) }` as above, delete the copied block.
   OpenTorah `:docs` keeps `tasks.named('generateSite') { dependsOn generateTables }`.
5. Site workflows: `./gradlew generateSite` (or `:docs:generateSite`). Drop or shrink
   `.github/actions/generate`.

Consumers of the copied block today:

- `dub.podval.org`
- `www.podval.org`
- `chumashquestions.org`
- `alter-rebbe.org`
- `opentorah.org` (`:docs`)
- `mathworlds-site`

## Not this plugin

Repeated Podval Gradle setup (foojay, Java 25 / Scala 3 flags, `dependencyUpdates` filters,
Central POM / signing) is **not** this plugin. That lives in `../podval-gradle` (see
`gradle-conventions-plan.md` there).
