// Mirrors org.podval.tools.publish.site.CollectionAliases / Path.fromHref / Facsimile.inboundRemainder.

export function parsePath(pathname) {
  const segments = String(pathname).split("/").map((s) => s.trim()).filter((s) => s.length > 0);
  if (segments.length === 0) return { path: [], extension: null };
  const last = segments[segments.length - 1];
  const dot = last.lastIndexOf(".");
  if (dot <= 0) return { path: segments, extension: null };
  const name = last.slice(0, dot).trim();
  const ext = last.slice(dot + 1);
  if (ext.length > 0 && [...ext].every((c) => c >= "0" && c <= "9")) {
    return { path: segments, extension: null };
  }
  return { path: [...segments.slice(0, -1), name], extension: ext };
}

export function splitLang(name) {
  // `+reg` is a reading, not a language, even when the stem under it would match `-xx`.
  if (name.endsWith("+reg")) return { base: name, lang: null };
  const dash = name.lastIndexOf("-");
  if (dash === -1 || dash !== name.length - 3) return { base: name, lang: null };
  return { base: name.slice(0, dash), lang: name.slice(dash + 1) };
}

function withoutReading(name) {
  return name.endsWith("+reg") ? name.slice(0, -"+reg".length) : name;
}

export function inboundFacsimile(remainder) {
  if (remainder.length === 2 && remainder[0] === "facsimile" && remainder[1].length > 0) {
    return [remainder[1], "facsimile"];
  }
  return remainder;
}

export function originalFacsimile(remainder) {
  if (remainder.length === 2 && remainder[1] === "facsimile") {
    // Strip `+reg` before splitLang so `doc-ru+reg` serves the same viewer as `doc-ru`.
    return [splitLang(withoutReading(remainder[0])).base, "facsimile"];
  }
  return remainder;
}

export function rewritePath(pathname, aliases) {
  const parsed = parsePath(pathname);
  const table = aliases?.aliases ?? aliases ?? [];
  let best = null;
  for (const entry of table) {
    const from = entry.from;
    if (from.length === 0 || from.length > parsed.path.length) continue;
    if (!from.every((seg, i) => parsed.path[i] === seg)) continue;
    if (!best || from.length > best.from.length) best = entry;
  }
  if (!best) return null;
  const remainder = parsed.path.slice(best.from.length);
  const segs = originalFacsimile(inboundFacsimile(remainder));
  if (segs.length === 0) {
    const index = best.index ?? { path: best.to, extension: "html" };
    const indexExtension = index.extension ?? null;
    // `/search` is the page. `/search.json` is the index file, not that page.
    if (parsed.extension != null && parsed.extension !== indexExtension) return null;
    const extension = index.extension ?? "html";
    const base = "/" + (index.path ?? []).join("/");
    return extension ? `${base}.${extension}` : base;
  }
  const extension = parsed.extension ?? "html";
  const path = best.to.concat(segs);
  const base = "/" + path.join("/");
  return extension ? `${base}.${extension}` : base;
}
