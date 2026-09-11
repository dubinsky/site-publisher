(function () {
  var html = document.documentElement;
  var keys = ["glossary-expand"];
  try {
    keys.forEach(function (k) {
      if (localStorage.getItem(k) === "1") html.classList.add(k);
    });
  } catch (e) {}

  function apply(name, on) {
    html.classList.toggle(name, on);
    try { localStorage.setItem(name, on ? "1" : "0"); } catch (e) {}
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

    var settings = document.querySelector(".site-settings");
    if (!settings) return;
    document.addEventListener("click", function (e) {
      if (!settings.contains(e.target)) settings.removeAttribute("open");
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") settings.removeAttribute("open");
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
