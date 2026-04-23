// =============================================================
// theme.js
// Minimal JS hooks for the GCC Dashboard design system.
// Loaded once from app.R via tags$script(src = "theme.js").
//
// @portable: yes
// @phase: 1
// =============================================================

(function () {
  if (!window.Shiny) return;

  // -------------------------------------------------------
  // gcc_set_accent
  // Sets the background color of a .gcc-kpi-accent stripe
  // from the server without triggering a re-render. Used by
  // mod_kpi_card.R:
  //   session$sendCustomMessage("gcc_set_accent",
  //     list(id = session$ns("accent"), color = accent))
  // -------------------------------------------------------
  Shiny.addCustomMessageHandler("gcc_set_accent", function (msg) {
    if (!msg || !msg.id) return;
    var el = document.getElementById(msg.id);
    if (el) el.style.background = msg.color;
  });

  // -------------------------------------------------------
  // gcc_set_lang_class
  // Toggles body.lang-ar / body.lang-en for RTL. The existing
  // app already does this via shinyjs — this handler is a
  // portable fallback so GCCEDI can drop in the same theme
  // without depending on shinyjs.
  // -------------------------------------------------------
  Shiny.addCustomMessageHandler("gcc_set_lang_class", function (msg) {
    if (!msg || !msg.lang) return;
    var body = document.body;
    body.classList.remove("lang-en", "lang-ar");
    body.classList.add("lang-" + msg.lang);
  });
})();
