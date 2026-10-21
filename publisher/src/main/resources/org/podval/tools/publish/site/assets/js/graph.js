export function run(cytoscape) {
  var container = document.getElementById("site-graph");
  if (!container) return;

  var search = document.getElementById("site-graph-search");
  var orphans = document.getElementById("site-graph-orphans");
  var status = document.getElementById("site-graph-status");
  var loadFailed = status ? (status.getAttribute("data-load-failed") || "") : "";
  var statusPattern = status ? (status.getAttribute("data-status") || "") : "";
  var largeNote = status ? (status.getAttribute("data-large") || "") : "";
  var plural = status ? (status.getAttribute("data-plural") || "") : "";
  var FORCE_MAX = 400;
  var CONCENTRIC_AFTER = 2000;

  // Spec: org.podval.metadata.Language.pluralCategory
  function pluralCategory(lang, n) {
    if (lang === "fr") return n === 0 || n === 1 ? "one" : "other";
    if (lang === "ru") return russianCategory(n);
    if (lang === "pl") return polishCategory(n);
    if (lang === "lt") return lithuanianCategory(n);
    if (lang === "he") {
      if (n === 1) return "one";
      if (n === 2) return "two";
      return "other";
    }
    return n === 1 ? "one" : "other";
  }

  function russianCategory(n) {
    var mod10 = n % 10;
    var mod100 = n % 100;
    if (mod10 === 1 && mod100 !== 11) return "one";
    if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) return "few";
    return "many";
  }

  function polishCategory(n) {
    var mod10 = n % 10;
    var mod100 = n % 100;
    if (n === 1) return "one";
    if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) return "few";
    return "many";
  }

  function lithuanianCategory(n) {
    var mod10 = n % 10;
    var mod100 = n % 100;
    var teen = mod100 >= 11 && mod100 <= 19;
    if (mod10 === 1 && !teen) return "one";
    if (mod10 >= 2 && mod10 <= 9 && !teen) return "few";
    return "many";
  }

  function cardinalForm(kind, count) {
    var category = pluralCategory(plural, count);
    var value = status.getAttribute("data-" + kind + "-" + category);
    if (!value) {
      console.warn("graph status: missing data-" + kind + "-" + category);
      value = status.getAttribute("data-" + kind + "-one") || "";
    }
    return value;
  }

  fetch("/graph.json").then(function (response) {
    if (!response.ok) throw new Error("graph.json " + response.status);
    return response.json();
  }).then(function (data) {
    start(data);
  }).catch(function (err) {
    console.warn(err);
    if (status) status.textContent = loadFailed;
  });

  function displayEdges(edges) {
    var seen = {};
    var out = [];
    (edges || []).forEach(function (edge) {
      var a = edge.source;
      var b = edge.target;
      var pair = [a, b].sort().join("~");
      var id = pair + ":" + edge.kind;
      if (seen[id]) return;
      seen[id] = true;
      out.push({ data: { id: id, source: a, target: b, kind: edge.kind } });
    });
    return out;
  }

  function layoutFor(n) {
    if (n > CONCENTRIC_AFTER) return { name: "concentric", animate: false };
    if (n > FORCE_MAX) return { name: "cose", animate: false, refresh: 0 };
    return { name: "cose", animate: true };
  }

  function start(data) {
    var nodes = (data.nodes || []).map(function (node) {
      return {
        data: {
          id: node.id,
          label: node.title,
          href: node.href,
          target: node.target || "",
          kind: node.kind
        }
      };
    });
    var elements = nodes.concat(displayEdges(data.edges));
    var n = nodes.length;
    function cssColor(name) {
      var probe = document.createElement("span");
      probe.style.color = "var(" + name + ")";
      document.body.appendChild(probe);
      var value = getComputedStyle(probe).color;
      probe.remove();
      return value;
    }

    function paintGraph(cy) {
      cy.style()
        .selector("node")
        .style({
          color: cssColor("--graph-label-color"),
          "background-color": cssColor("--graph-node-color")
        })
        .selector("edge")
        .style({ "line-color": cssColor("--graph-edge-color") })
        .update();
    }

    var cy = cytoscape({
      container: container,
      elements: elements,
      layout: layoutFor(n),
      wheelSensitivity: 0.3,
      style: [
        {
          selector: "node",
          style: {
            label: "data(label)",
            "font-size": 10,
            "text-valign": "bottom",
            "text-margin-y": 4,
            color: cssColor("--graph-label-color"),
            "background-color": cssColor("--graph-node-color"),
            width: 16,
            height: 16
          }
        },
        {
          selector: "edge",
          style: {
            width: 1.5,
            "line-color": cssColor("--graph-edge-color"),
            "curve-style": "haystack",
            "haystack-radius": 0.6
          }
        },
        { selector: ".faded", style: { opacity: 0.15 } },
        { selector: ".hidden", style: { display: "none" } }
      ]
    });

    cy.nodes().forEach(function (node) {
      var degree = node.degree();
      var size = 12 + Math.min(degree, 12) * 2;
      node.style({ width: size, height: size });
    });

    var edgeCount = cy.edges().length;
    var note = n > CONCENTRIC_AFTER && largeNote ? " " + largeNote : "";
    if (status) {
      status.textContent = statusPattern
        .replace("{nodes}", cardinalForm("node", n))
        .replace("{edges}", cardinalForm("edge", edgeCount))
        .replace("{n}", String(n))
        .replace("{e}", String(edgeCount)) + note;
    }

    cy.one("layoutstop", function () {
      cy.fit(undefined, 24);
      cy.stop();
    });

    cy.on("tap", "node", function (evt) {
      var d = evt.target.data();
      if (!d.href) return;
      if (d.target) window.open(d.href, d.target);
      else location.assign(d.href);
    });

    cy.on("mouseover", "node", function (evt) {
      var neighborhood = evt.target.closedNeighborhood();
      cy.elements().addClass("faded");
      neighborhood.removeClass("faded");
    });
    cy.on("mouseout", "node", function () {
      cy.elements().removeClass("faded");
    });

    function applyFilters() {
      var q = ((search && search.value) || "").toLowerCase();
      var showOrphans = !orphans || orphans.checked;
      cy.nodes().forEach(function (node) {
        var hide = false;
        if (q && String(node.data("label")).toLowerCase().indexOf(q) < 0) hide = true;
        if (!showOrphans && node.degree() === 0) hide = true;
        node.toggleClass("hidden", hide);
      });
      cy.edges().forEach(function (edge) {
        edge.toggleClass("hidden", edge.source().hasClass("hidden") || edge.target().hasClass("hidden"));
      });
    }

    if (search) search.addEventListener("input", applyFilters);
    if (orphans) orphans.addEventListener("change", applyFilters);
    document.documentElement.addEventListener("site-color-scheme", function () {
      paintGraph(cy);
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "/" && search && document.activeElement !== search) {
        e.preventDefault();
        search.focus();
      }
      if (e.key === "Escape") {
        if (search) search.value = "";
        applyFilters();
        cy.elements().removeClass("faded");
      }
    });
  }
}
