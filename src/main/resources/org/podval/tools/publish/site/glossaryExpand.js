(function () {
  var key = 'glossary-expand';
  var html = document.documentElement;
  var btn = document.getElementById('glossary-expand-toggle');
  if (!btn) return;
  function sync() {
    btn.setAttribute('aria-pressed', html.classList.contains('glossary-expand') ? 'true' : 'false');
  }
  sync();
  btn.addEventListener('click', function () {
    var on = !html.classList.contains('glossary-expand');
    html.classList.toggle('glossary-expand', on);
    try { localStorage.setItem(key, on ? '1' : '0'); } catch (e) {}
    sync();
  });
})();
