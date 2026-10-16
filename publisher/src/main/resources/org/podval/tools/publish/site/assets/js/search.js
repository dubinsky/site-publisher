export function run() {
  var root = document.getElementById("site-search");
  if (!root) return;
  var form = root.querySelector(".site-search-filters");
  var yearSelect = form.querySelector('select[name="year"]');
  var archiveSelect = form.querySelector('select[name="archive"]');
  var langSelect = form.querySelector('select[name="lang"]');
  var status = document.getElementById("site-search-status");
  var hits = document.getElementById("site-search-hits");
  var typeQuery = root.getAttribute("data-type-query") || "";
  var noMatches = root.getAttribute("data-none") || "";
  var couldNotLoad = root.getAttribute("data-load-failed") || "";
  var overflow = root.getAttribute("data-overflow") || "";
  var kinds = {
    document: root.getAttribute("data-kind-document") || "",
    name: root.getAttribute("data-kind-name") || "",
    note: root.getAttribute("data-kind-note") || "",
    collection: root.getAttribute("data-kind-collection") || ""
  };
  var params = new URLSearchParams(location.search);
  var q = params.get("q") || "";
  var year = params.get("year") || "";
  var archive = params.get("archive") || "";
  var lang = params.get("lang") || "";
  var tokens = q.toLowerCase().split(/[^\p{L}\p{N}]+/u).filter(function (token) {
    return token.length > 0;
  });
  var records = [];
  form.addEventListener("change", applyFilters);
  form.addEventListener("submit", function (event) {
    event.preventDefault();
    applyFilters();
  });
  fetch("/search.json").then(function (response) {
    if (!response.ok) throw new Error("search.json " + response.status);
    return response.json();
  }).then(function (data) {
    records = data.records || [];
    records.forEach(function (record) {
      record._tokens = tokenSet(record.text || "");
    });
    fillSelect(yearSelect, yearOptions(records), year);
    fillSelect(archiveSelect, archiveOptions(records), archive);
    fillSelect(langSelect, langOptions(records), lang);
    render();
  }).catch(function (err) {
    console.warn(err);
    if (status) status.textContent = couldNotLoad;
  });

  function applyFilters() {
    year = readSelect(yearSelect);
    archive = readSelect(archiveSelect);
    lang = readSelect(langSelect);
    var next = new URLSearchParams();
    if (q) next.set("q", q);
    if (year) next.set("year", year);
    if (archive) next.set("archive", archive);
    if (lang) next.set("lang", lang);
    var qs = next.toString();
    history.replaceState(null, "", location.pathname + (qs ? "?" + qs : "") + location.hash);
    render();
  }

  function readSelect(select) {
    if (!select || select.selectedIndex <= 0) return "";
    return select.value;
  }

  function render() {
    while (hits.firstChild) hits.removeChild(hits.firstChild);
    if (tokens.length === 0) {
      status.textContent = typeQuery;
      return;
    }
    var matched = records.filter(matches);
    var shown = matched.slice(0, 100);
    if (matched.length === 0) status.textContent = noMatches;
    else if (matched.length > 100) status.textContent = overflow.replace("{n}", String(matched.length));
    else status.textContent = "";
    shown.forEach(function (record) { hits.appendChild(row(record)); });
  }

  function matches(record) {
    var set = record._tokens || {};
    var i = 0;
    while (i < tokens.length) {
      if (!set[tokens[i]]) return false;
      i += 1;
    }
    if (year && (record.years || []).indexOf(Number(year)) < 0) return false;
    if (archive && record.archive !== archive) return false;
    if (lang && record.lang && record.lang !== lang) return false;
    return true;
  }

  function row(record) {
    var item = document.createElement("li");
    var snippet = document.createElement("p");
    snippet.className = "site-search-snippet";
    appendSnippet(snippet, record.text || "");
    item.appendChild(snippet);
    var meta = document.createElement("p");
    meta.className = "site-search-meta";
    meta.appendChild(document.createTextNode(record.date || ""));
    meta.appendChild(document.createTextNode(" "));
    var anchor = document.createElement("a");
    anchor.href = record.href;
    if (record.target) anchor.target = record.target;
    anchor.appendChild(document.createTextNode(record.path ? record.path : (record.title || "")));
    meta.appendChild(anchor);
    meta.appendChild(document.createTextNode(" "));
    meta.appendChild(document.createTextNode(kindWord(record.kind)));
    item.appendChild(meta);
    return item;
  }

  function kindWord(kind) {
    return kinds[kind] || kind || "";
  }

  // Required example: stored text "На" is marked when the query is "на" (original offsets, 40 before and 80 after).
  function appendSnippet(parent, text) {
    var spans = tokenSpans(text);
    var hit = null;
    var i = 0;
    while (i < spans.length) {
      if (tokens.indexOf(spans[i].raw.toLowerCase()) >= 0) { hit = spans[i]; break; }
      i += 1;
    }
    if (!hit) {
      parent.appendChild(document.createTextNode(text.slice(0, 120)));
      return;
    }
    var cutStart = Math.max(0, hit.start - 40);
    var cutEnd = Math.min(text.length, hit.end + 80);
    var start = expandStart(text, cutStart);
    var end = expandEnd(text, cutEnd);
    if (start > 0) parent.appendChild(document.createTextNode("…"));
    parent.appendChild(document.createTextNode(text.slice(start, hit.start)));
    var mark = document.createElement("mark");
    mark.appendChild(document.createTextNode(text.slice(hit.start, hit.end)));
    parent.appendChild(mark);
    parent.appendChild(document.createTextNode(text.slice(hit.end, end)));
    if (end < text.length) parent.appendChild(document.createTextNode("…"));
  }

  function expandStart(text, index) {
    var i = index;
    if (i <= 0 || isSpace(text.charAt(i)) || isSpace(text.charAt(i - 1))) return Math.max(0, i);
    while (i > 0 && !isSpace(text.charAt(i - 1))) i -= 1;
    return i;
  }

  function expandEnd(text, index) {
    var i = index;
    if (i >= text.length || isSpace(text.charAt(i)) || (i > 0 && isSpace(text.charAt(i - 1)))) return Math.min(text.length, i);
    while (i < text.length && !isSpace(text.charAt(i))) i += 1;
    return i;
  }

  function isSpace(ch) {
    return /\s/.test(ch);
  }
}

function tokenSpans(text) {
  return Array.from(text.matchAll(/[\p{L}\p{N}]+/gu), function (match) {
    return { start: match.index, end: match.index + match[0].length, raw: match[0] };
  });
}

function tokenSet(text) {
  var set = Object.create(null);
  tokenSpans(text).forEach(function (span) { set[span.raw.toLowerCase()] = true; });
  return set;
}

function yearOptions(records) {
  var seen = Object.create(null);
  records.forEach(function (record) {
    (record.years || []).forEach(function (value) { seen[value] = true; });
  });
  return Object.keys(seen).map(Number).sort(function (a, b) { return a - b; }).map(function (value) {
    return { value: String(value), label: String(value) };
  });
}

function archiveOptions(records) {
  var byId = Object.create(null);
  records.forEach(function (record) {
    if (record.archive) byId[record.archive] = record.archiveLabel || record.archive;
  });
  return Object.keys(byId).map(function (id) {
    return { value: id, label: byId[id] };
  }).sort(function (a, b) {
    if (a.label < b.label) return -1;
    if (a.label > b.label) return 1;
    return 0;
  });
}

function langOptions(records) {
  var seen = Object.create(null);
  records.forEach(function (record) { if (record.lang) seen[record.lang] = true; });
  return Object.keys(seen).sort().map(function (code) { return { value: code, label: code }; });
}

function fillSelect(select, options, selected) {
  var allLabel = select.options.length ? select.options[0].textContent : "";
  while (select.firstChild) select.removeChild(select.firstChild);
  addOption(select, "", allLabel, !selected);
  options.forEach(function (option) {
    addOption(select, option.value, option.label, option.value === selected);
  });
}

function addOption(select, value, label, selected) {
  var option = document.createElement("option");
  option.value = value;
  option.appendChild(document.createTextNode(label));
  if (selected) option.selected = true;
  select.appendChild(option);
}
