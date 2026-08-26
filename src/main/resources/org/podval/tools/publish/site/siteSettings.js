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
})();
