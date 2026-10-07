# Site Publisher Agent Guidelines

## Documentation

Design (pipeline, IR, why) goes in the Obsidian note `dub.podval.org/notes/Publishing/Site Publisher.md` under
**Design**, with a per-feature subsection when a feature has IR or non-obvious architecture.

User documentation (how to run the generator; syntax of each construct in each markup) stays in this repo’s
`README.adoc`.

Some overlap is expected (IR HTML shape in the note vs HTML/author syntax in the README).
Do not put design essays in the README or author syntax in the Design section.

## When working on the code

- Prefer making changes that keep the core small and plugin-free.
- Do not point tests at a real site.

## Local checkouts

`./gradlew run --args="<source>"` generates a site.
`--serve` generates and leaves the HTTP server running.

- `/home/dub/Podval/dub.podval.org` — https://dub.podval.org
- `/home/dub/Podval/www.podval.org` — https://www.podval.org
- `/home/dub/OpenTorah/chumashquestions.org` — https://www.chumashquestions.org
- `/home/dub/OpenTorah/alter-rebbe.org` — https://www.alter-rebbe.org
- `/home/dub/OpenTorah/opentorah.org/docs` — https://www.opentorah.org
- `/home/dub/CognoMath/mathworlds-site` — https://www.mathworlds.org (`cognomath/mathworlds-site`).

CI uses Central, not the local checkout.
