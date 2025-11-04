// pkgdown/extra.js
(function () {
  const KEY = "yusufHAIGermany-color-mode";
  const MODES = ["light", "dark", "auto"];

  function apply(mode) {
    if (mode === "auto") {
      document.documentElement.removeAttribute("data-bs-theme");
    } else {
      document.documentElement.setAttribute("data-bs-theme", mode);
    }
    localStorage.setItem(KEY, mode);
    updateLabel(mode);
  }

  function updateLabel(mode) {
    const link = document.querySelector('.navbar .nav-link[data-theme-toggle]');
    if (link) link.textContent = `Theme: ${mode[0].toUpperCase()}${mode.slice(1)}`;
  }

  function injectToggle() {
    // Find the placeholder nav item labeled "Theme"
    const candidate = Array.from(document.querySelectorAll(".navbar .nav-link"))
      .find(a => a.textContent.trim().toLowerCase() === "theme");
    if (!candidate) return;

    // Turn it into a simple cycling button (no dropdowns, no Popper)
    candidate.setAttribute("data-theme-toggle", "1");
    candidate.href = "#";

    // initial mode
    const saved = localStorage.getItem(KEY);
    const mode = MODES.includes(saved) ? saved : "auto";
    apply(mode);

    candidate.addEventListener("click", function (e) {
      e.preventDefault();
      const current = localStorage.getItem(KEY) || "auto";
      const next = MODES[(MODES.indexOf(current) + 1) % MODES.length];
      apply(next);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", injectToggle);
  } else {
    injectToggle();
  }
})();
