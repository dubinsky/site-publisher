function waitForFonts() {
  return (document.fonts && document.fonts.ready)
    ? document.fonts.ready
    : Promise.resolve();
}
