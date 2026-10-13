document.querySelectorAll("button.citation-copy").forEach((button) => {
  button.addEventListener("click", () => {
    const payload = button.getAttribute("data-citation") || "";
    if (payload && navigator.clipboard) {
      navigator.clipboard.writeText(payload).catch(() => {});
    }
  });
});
