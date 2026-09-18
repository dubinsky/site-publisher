export function run(cytoscape) {
  var container = document.getElementById("site-graph");
  if (!container) return;

  var search = document.getElementById("site-graph-search");
  var orphans = document.getElementById("site-graph-orphans");
  var status = document.getElementById("site-graph-status");
  var FORCE_MAX = 400;
  var CONCENTRIC_AFTER = 2000;

  fetch("/graph.json").then(function (response) {
    if (!response.ok) throw new Error("graph.json " + response.status);
    return response.json();
  }).then(function (data) {
    start(data);
  }).catch(function (err) {
    console.warn(err);
    if (status) status.textContent = "Could not load the graph.";
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
            color: "#2a2a2a",
            "background-color": "#1e69d8",
            width: 16,
            height: 16
          }
        },
        {
          selector: "edge",
          style: {
            width: 1.5,
            "line-color": "#c8c8c8",
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
    var note = n > CONCENTRIC_AFTER ? " (large graph, simplified layout)" : "";
    if (status) status.textContent = n + " nodes, " + edgeCount + " edges" + note;

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
