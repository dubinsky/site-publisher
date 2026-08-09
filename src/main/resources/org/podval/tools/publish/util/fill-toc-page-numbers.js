/**
 * Append dotted leaders and estimated PDF page numbers to TOC entries.
 *
 * Print styling for the injected nodes (.toc-leader, .toc-page-number) lives in
 * layout.css under @media print.
 *
 * Runs twice so a large TOC that grows after numbers are injected still gets accurate pages.
 *
 * Invoked by Playwright page.evaluate (caller parenthesizes this declaration into a function expression).
 *
 * @param {number} pageHeight printable content height in CSS px (Letter minus paper margins)
 */
function fillTocPageNumbers(pageHeight) {
  const fill = () => {
    document.querySelectorAll('ul.toc a[href*="#"]').forEach((anchor) => {
      const href = anchor.getAttribute('href') || '';
      const hash = href.indexOf('#');
      if (hash < 0) return;
      const id = decodeURIComponent(href.substring(hash + 1));
      if (!id) return;
      const target = document.getElementById(id);
      if (!target) return;
      const top = target.getBoundingClientRect().top + window.scrollY;
      const pageNumber = Math.max(1, Math.floor(top / pageHeight) + 1);
      const li = anchor.parentElement;
      if (!li) return;
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
      pageEl.textContent = String(pageNumber);
    });
  };
  fill();
  fill();
}
