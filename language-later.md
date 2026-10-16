# Later: language doors and headnotes

Deferred.
The publisher work that has to land first is the language catalog, page language, search hyphenation, interface strings,
the alternate resolver, and the `+reg` reading.
This note is what comes after that.
Do not start it until that work has shipped.

Two languages stay separate.
The document language is `text/@xml:lang` on a TEI file, or front-matter `lang` on a prose page.
The interface language is the site language, or the language of a door.
Chrome follows the interface language.
`html lang` follows the document.
A door is a tree under a language directory.
A translation of one document is a sibling file.
Those are different texts and keep different URLs.

## English door, then the preference

On alter-rebbe.org, Russian stays at the current URLs and is `x-default`.
Add `/en/` for the entrance, the about page, and the how-to-read page.
The header link uses the autonym.
`hreflang` between the doors is absolute and reciprocal, and each page includes itself.
Each page's canonical URL is itself.

This is a writing step.
The resolver and the English interface strings have to exist before the pages do.
Notes do not grow `-en` siblings for this door.
The directory is what makes the tree one language, and the prefix still leads somewhere when one page has no
translation.

The gear then gains a preferred door, stored like the color scheme.
It changes where the site title goes.
It does not rewrite the page, and it does not swap strings on the URL the reader is already on.

`/en/niab1401/088.html`, if it is ever built, is the English frame around the Latin original: English chrome, English
headnote, Latin body.
`088-en.xml` is an English translation of the body.
Do not use one URL for both.

## Headnotes

A diplomatic file may repeat `<abstract xml:lang="…">`.
The diplomatic page shows the abstract in the document's language.
A door shows the abstract in the interface language when that abstract exists.
Translating the abstracts is content.
The diplomatic transcription stays in its own language.

## Hebrew door

`/he/` is the same kind of prefix, once there is Hebrew prose.
The page `dir` follows the interface language, so a Hebrew door is `rtl`.
A Hebrew transcription on the Russian site stays right-to-left inside a left-to-right page.

## Chumash Questions

A Russian translation of the book is a `/ru/` tree with the same section ids, its own `book.adoc`, its own chunking, and
its own PDF.
Glossary ids stay shared.
The apparatus roles (`[.add]`, `[.see]`, `[.del]`) stay on the English text.
A translation is prose and does not re-encode that apparatus.
English stays unprefixed and is `x-default`.
Build the tree when there is Russian text to put in it.
The publisher work is the prefix, the shared ids, and Russian chrome.
