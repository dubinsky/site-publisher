/**
 * TOC dotted leaders and page numbers.
 *
 * Print styling for .toc-leader / .toc-page-number is in layout.css (@media print).
 * Page numbers are not estimated here: Playwright prints a PDF, Java reads named
 * destinations, then applyTocPageNumbers writes those values. Placeholders keep
 * the 2.5em page-number column so the second print paginates the same way.
 *
 * Invoked by Playwright page.evaluate (caller wraps these declarations).
 */
function tocPageSlot(anchor) {
  const li = anchor.parentElement;
  if (!li) return null;
  let leader = li.querySelector(':scope > .toc-leader');
  if (!leader) {
    leader = document.createElement('span');
    leader.className = 'toc-leader';
    leader.setAttribute('aria-hidden', 'true');
    li.insertBefore(leader, anchor.nextSibling);
  }
  let pageEl = li.querySelector(':scope > .toc-page-number');
  if (!pageEl) {
    pageEl = document.createElement('span');
    pageEl.className = 'toc-page-number';
    li.insertBefore(pageEl, leader.nextSibling);
  }
  return pageEl;
}

function ensureTocLeaders() {
  document.querySelectorAll('ul.toc a[href*="#"]').forEach((anchor) => {
    const pageEl = tocPageSlot(anchor);
    if (pageEl && !pageEl.textContent) pageEl.textContent = '0';
  });
}

function applyTocPageNumbers(pageById) {
  document.querySelectorAll('ul.toc a[href*="#"]').forEach((anchor) => {
    const href = anchor.getAttribute('href') || '';
    const hash = href.indexOf('#');
    if (hash < 0) return;
    const id = decodeURIComponent(href.substring(hash + 1));
    if (!id || pageById[id] == null) return;
    const pageEl = tocPageSlot(anchor);
    if (pageEl) pageEl.textContent = String(pageById[id]);
  });
}
