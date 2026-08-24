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
  const weight = cs.fontWeight;
  const out = {
    fontFamily: cs.fontFamily,
    fontSizePx: parseFloat(cs.fontSize),
    fontWeight: (weight === 'bold' || weight === 'bolder') ? 700
              : (weight === 'normal' || weight === 'lighter') ? 400
              : (parseInt(weight, 10) || 400),
    italic: cs.fontStyle === 'italic' || cs.fontStyle === 'oblique',
    // Chromium may keep fillStyle as oklab(); rasterize to sRGB bytes.
    color: cssColorToRgb(cs.color)
  };
  el.remove();
  return out;
}

function cssColorToRgb(cssColor) {
  const canvas = document.createElement('canvas');
  canvas.width = 1;
  canvas.height = 1;
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = cssColor;
  ctx.fillRect(0, 0, 1, 1);
  const d = ctx.getImageData(0, 0, 1, 1).data;
  return `rgb(${d[0]}, ${d[1]}, ${d[2]})`;
}
