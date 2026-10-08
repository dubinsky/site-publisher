(function () {
  var html = document.documentElement;
  var keys = ["glossary-expand", "transclusion-clean"];
  var schemeKey = "color-scheme";

  function storageGet(key) {
    try { return localStorage.getItem(key); } catch (e) { return null; }
  }
  function storageSet(key, value) {
    try { localStorage.setItem(key, value); } catch (e) {}
  }

  function preference() {
    var value = storageGet(schemeKey);
    return value === "light" || value === "dark" ? value : "system";
  }

  function resolvedDark() {
    var chosen = preference();
    return chosen === "dark" ||
      (chosen === "system" && window.matchMedia("(prefers-color-scheme: dark)").matches);
  }

  function applyHighlight(dark) {
    document.querySelectorAll("link[data-hljs-theme]").forEach(function (link) {
      var isDark = link.getAttribute("data-hljs-theme") === "dark";
      link.media = isDark === dark ? "all" : "not all";
    });
  }

  function applyScheme(notify) {
    var dark = resolvedDark();
    html.classList.toggle("color-scheme-dark", dark);
    applyHighlight(dark);
    if (notify) html.dispatchEvent(new Event("site-color-scheme"));
  }

  try {
    keys.forEach(function (k) {
      if (storageGet(k) === "1") html.classList.add(k);
    });
  } catch (e) {}
  applyScheme(false);

  var schemeQuery = window.matchMedia("(prefers-color-scheme: dark)");
  schemeQuery.addEventListener("change", function () {
    if (preference() === "system") applyScheme(true);
  });

  function apply(name, on) {
    html.classList.toggle(name, on);
    storageSet(name, on ? "1" : "0");
  }

  function init() {
    document.querySelectorAll("[data-setting]").forEach(function (box) {
      var name = box.getAttribute("data-setting");
      if (!name) return;
      box.checked = html.classList.contains(name);
      box.addEventListener("change", function () {
        apply(name, box.checked);
      });
    });

    var chosen = preference();
    document.querySelectorAll('input[name="color-scheme"]').forEach(function (radio) {
      radio.checked = radio.value === chosen;
      radio.addEventListener("change", function () {
        if (!radio.checked) return;
        storageSet(schemeKey, radio.value);
        applyScheme(true);
      });
    });

    var settings = document.querySelector(".site-settings");
    var header = document.querySelector(".site-header");
    var navToggle = document.querySelector(".nav-toggle");

    function setNavOpen(open) {
      if (!header || !navToggle) return;
      header.classList.toggle("nav-open", open);
      navToggle.setAttribute("aria-expanded", open ? "true" : "false");
    }

    if (navToggle) {
      navToggle.addEventListener("click", function () {
        setNavOpen(!header.classList.contains("nav-open"));
      });
    }

    document.addEventListener("click", function (e) {
      if (settings && !settings.contains(e.target)) settings.removeAttribute("open");
      if (header && navToggle && !header.contains(e.target)) setNavOpen(false);
    });
    document.addEventListener("keydown", function (e) {
      if (e.key !== "Escape") return;
      if (settings) settings.removeAttribute("open");
      setNavOpen(false);
    });
  }

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", init);
  else
    init();

  var windowName = html.getAttribute("data-window-name");
  if (windowName) {
    window.name = windowName;
    window.addEventListener("beforeunload", function () {
      try {
        sessionStorage.setItem(window.location + "-scroll", String(scrollY()));
      } catch (e) {}
    });
    if (!window.location.hash) {
      try {
        var saved = sessionStorage.getItem(window.location + "-scroll");
        if (saved) {
          setTimeout(function () { setScrollY(saved); }, 100);
        }
      } catch (e) {}
    } else {
      setTimeout(function () {
        var h = document.querySelector(decodeURI(window.location.hash));
        if (h) h.scrollIntoView();
      }, 100);
    }
  }

  function scrollPane() {
    return document.querySelector("html.facsimile .facsimile-scroller");
  }
  function scrollY() {
    var el = scrollPane();
    return el ? el.scrollTop : window.scrollY;
  }
  function setScrollY(y) {
    var el = scrollPane();
    if (el) el.scrollTop = Number(y);
    else window.scrollTo(0, Number(y));
  }
})();
