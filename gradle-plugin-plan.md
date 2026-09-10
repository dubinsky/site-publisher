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

## Local unreleased plugin

```gradle
pluginManagement {
  includeBuild('../site-publisher') // or -PsitePublisherDir=
}
```

Library substitution for a site that already depends on the coordinate:

```gradle
final File publisherDir = file(providers.gradleProperty('sitePublisherDir').getOrElse('../site-publisher'))
if (publisherDir.isDirectory() && new File(publisherDir, 'settings.gradle').isFile()) {
  includeBuild(publisherDir)
}
```

See README **Dogfooding**.
