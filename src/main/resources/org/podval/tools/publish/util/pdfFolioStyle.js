/**
 * Computed `.folio` (print media, webfonts loaded). Size is CSS px.
 * Invoked by Playwright page.evaluate (caller wraps this declaration).
 */
function pdfFolioStyle() {
  const el = document.createElement('span');
  el.className = 'folio';
  el.textContent = '0';
  el.setAttribute('aria-hidden', 'true');
  document.body.appendChild(el);
  const cs = getComputedStyle(el);
  const ctx = document.createElement('canvas').getContext('2d');
  ctx.fillStyle = cs.color;
  const weight = cs.fontWeight;
  const out = {
    fontFamily: cs.fontFamily,
    fontSizePx: parseFloat(cs.fontSize),
    fontWeight: (weight === 'bold' || weight === 'bolder') ? 700
              : (weight === 'normal' || weight === 'lighter') ? 400
              : (parseInt(weight, 10) || 400),
    italic: cs.fontStyle === 'italic' || cs.fontStyle === 'oblique',
    color: ctx.fillStyle
  };
  el.remove();
  return out;
}
