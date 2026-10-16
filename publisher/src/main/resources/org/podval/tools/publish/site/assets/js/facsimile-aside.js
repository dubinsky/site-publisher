(function () {
  var aside = document.querySelector("aside.facsimile-aside");
  if (!aside) return;
  var img = aside.querySelector("img.facsimile-aside-img");
  if (!img) return;
  var status = aside.querySelector(".facsimile-aside-status");
  var header = document.querySelector(".site-header");
  var pageBody = document.querySelector(".page-body");

  var pin = null;
  var pinnedAt = 0;
  var queued = false;
  var loadingUrl = "";
  var failedUrl = "";
  var prefetchedUrl = "";
  var pendingImage = null;

  function paneDisplayed() {
    return getComputedStyle(aside).display !== "none";
  }

  function candidates() {
    if (!pageBody) return [];
    var nodes = pageBody.querySelectorAll("a.pb[data-facsimile-src], span.pb[data-facsimile-missing]");
    var list = [];
    var i;
    for (i = 0; i < nodes.length; i++) {
      if (!nodes[i].closest("aside.transclusion")) list.push(nodes[i]);
    }
    return list;
  }

  /* Header bottom plus the 0.75rem already in the [id] scroll-margin. */
  function readingLine() {
    var bottom = header ? header.getBoundingClientRect().bottom : 0;
    var fontSize = parseFloat(getComputedStyle(document.documentElement).fontSize);
    if (!fontSize) fontSize = 16;
    return bottom + (0.75 * fontSize);
  }

  function walked(list) {
    var line = readingLine() + 1;
    var chosen = null;
    var i;
    for (i = 0; i < list.length; i++) {
      if (list[i].getBoundingClientRect().top <= line) chosen = list[i];
    }
    return chosen || list[0] || null;
  }

  function hashCandidate(list) {
    var hash = location.hash;
    if (!hash || hash.length < 2) return null;
    var id = hash.slice(1);
    try { id = decodeURIComponent(id); } catch (e) {}
    var i;
    for (i = 0; i < list.length; i++) {
      if (list[i].id === id) return list[i];
    }
    return null;
  }

  function mark(anchor) {
    var current = pageBody ? pageBody.querySelectorAll("a.pb[aria-current]") : [];
    var i;
    for (i = 0; i < current.length; i++) {
      if (current[i] !== anchor) current[i].removeAttribute("aria-current");
    }
    anchor.setAttribute("aria-current", "true");
    if (anchor.id) img.setAttribute("data-facsimile-current", anchor.id);
  }

  function clearStatus() {
    aside.removeAttribute("data-facsimile-state");
    if (!status) return;
    status.textContent = "";
    status.hidden = true;
  }

  function showStatus() {
    aside.setAttribute("data-facsimile-state", "error");
    if (!status) return;
    status.hidden = false;
    status.textContent = status.getAttribute("data-error") || "";
  }

  function prefetchNext(list, anchor) {
    if (!paneDisplayed()) return;
    var index = list.indexOf(anchor);
    var next = index >= 0 ? list[index + 1] : null;
    var url = next ? next.getAttribute("data-facsimile-src") : "";
    if (!url || url === prefetchedUrl) return;
    prefetchedUrl = url;
    var pre = new Image();
    pre.decoding = "async";
    pre.src = url;
  }

  function isMissing(anchor) {
    return anchor.getAttribute("data-facsimile-missing") === "true";
  }

  function showMissing(anchor) {
    loadingUrl = "";
    if (pendingImage) {
      pendingImage.onload = null;
      pendingImage.onerror = null;
      pendingImage = null;
    }
    img.removeAttribute("src");
    img.alt = "";
    aside.setAttribute("data-facsimile-state", "missing");
    if (status) {
      status.hidden = false;
      status.textContent = status.getAttribute("data-missing") || "";
    }
    mark(anchor);
    prefetchNext(candidates(), anchor);
  }

  function show(anchor) {
    if (!paneDisplayed() || !anchor) return;
    if (isMissing(anchor)) {
      showMissing(anchor);
      return;
    }
    var url = anchor.getAttribute("data-facsimile-src");
    if (!url) return;
    var alt = anchor.getAttribute("data-facsimile-alt") || "";
    var list = candidates();
    if (img.getAttribute("src") === url) {
      /* An in-flight Image for another URL then sees loadingUrl !== url. */
      loadingUrl = url;
      clearStatus();
      img.alt = alt;
      mark(anchor);
      prefetchNext(list, anchor);
      return;
    }
    if (url === failedUrl) {
      loadingUrl = "";
      showStatus();
      mark(anchor);
      return;
    }
    if (url === loadingUrl) return;
    loadingUrl = url;
    if (pendingImage) {
      pendingImage.onload = null;
      pendingImage.onerror = null;
    }
    pendingImage = new Image();
    pendingImage.decoding = "async";
    pendingImage.onload = function () {
      if (loadingUrl !== url) return;
      loadingUrl = "";
      if (!paneDisplayed()) return;
      img.src = url;
      img.alt = alt;
      if (failedUrl === url) failedUrl = "";
      mark(anchor);
      clearStatus();
      prefetchNext(candidates(), anchor);
    };
    pendingImage.onerror = function () {
      if (loadingUrl !== url) return;
      loadingUrl = "";
      failedUrl = url;
      showStatus();
      prefetchNext(candidates(), anchor);
    };
    pendingImage.src = url;
  }

  function sync() {
    if (!paneDisplayed()) return;
    var list = candidates();
    if (!list.length) return;
    var anchor = null;
    if (pin && list.indexOf(pin) >= 0) anchor = pin;
    else pin = null;
    if (!anchor) anchor = walked(list);
    show(anchor);
  }

  function schedule() {
    if (queued) return;
    queued = true;
    requestAnimationFrame(function () {
      queued = false;
      sync();
    });
  }

  function clearPin() {
    if (!pin) return;
    pin = null;
    schedule();
  }

  var scrollKeys = {
    " ": true,
    "PageUp": true,
    "PageDown": true,
    "ArrowUp": true,
    "ArrowDown": true,
    "ArrowLeft": true,
    "ArrowRight": true,
    "Home": true,
    "End": true
  };

  function fromTyping(event) {
    var target = event.target;
    if (!target || target.nodeType !== 1) return false;
    var tag = target.tagName;
    return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || target.isContentEditable;
  }

  window.addEventListener("scroll", function (event) {
    /* Fragment jumps land inside 200ms. A later trusted scroll is the reader. */
    if (pin && event.isTrusted && (Date.now() - pinnedAt) > 200) pin = null;
    schedule();
  }, { passive: true });

  window.addEventListener("wheel", function () {
    clearPin();
  }, { passive: true });

  window.addEventListener("touchmove", function () {
    clearPin();
  }, { passive: true });

  window.addEventListener("keydown", function (event) {
    if (event.altKey || event.ctrlKey || event.metaKey) return;
    if (!scrollKeys[event.key] || fromTyping(event)) return;
    clearPin();
  });

  window.addEventListener("hashchange", function () {
    var found = hashCandidate(candidates());
    pin = found;
    if (found) pinnedAt = Date.now();
    schedule();
  });

  var gear = document.getElementById("setting-facsimile-aside");
  if (gear) {
    /* Registered before siteSettings.js, so the off class is not on yet. */
    gear.addEventListener("change", function () {
      schedule();
    });
  }

  window.matchMedia("(min-width: 1312px)").addEventListener("change", function () {
    schedule();
  });

  document.addEventListener("click", function (event) {
    if (event.button || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
    var node = event.target;
    if (!node) return;
    if (node.nodeType !== 1) node = node.parentElement;
    if (!node || !node.closest) return;
    var anchor = node.closest("a.pb");
    if (!anchor || anchor.closest("aside.transclusion")) return;
    if (!anchor.getAttribute("data-facsimile-src")) return;
    if (!paneDisplayed()) return;
    event.preventDefault();
    pin = anchor;
    pinnedAt = Date.now();
    failedUrl = "";
    history.replaceState(null, "", "#" + anchor.id);
    show(anchor);
  });

  var initial = hashCandidate(candidates());
  if (initial) {
    pin = initial;
    pinnedAt = Date.now();
  }
  sync();
})();
