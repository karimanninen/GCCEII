# =============================================================================
# server_metadata_snippet.R
# How to wire mod_metadata.R into your existing app.R
# =============================================================================
# Assumes current_lang <- reactiveVal("en") is already set up
# per app_wiring_snippet.R.
# =============================================================================

# ── 1. In the UI: add a Metadata menu item to the sidebar ────────────────────

# Add inside sidebarMenu():
menuItem(
  t("menu_metadata", "en"),              # label; becomes reactive in server
  tabName = "metadata",
  icon    = icon("book")
)

# Add inside tabItems():
tabItem(
  tabName = "metadata",
  uiOutput("metadata_tab_ui")           # rendered reactively in server
)


# ── 2. In the server: render + call the module ────────────────────────────────

# Render the metadata tab UI reactively so it re-renders when language changes
output$metadata_tab_ui <- renderUI({
  metadata_ui("metadata", lang = current_lang())
})

# Call the server module (pass lang as a reactive)
metadata_server("metadata", lang = current_lang)


# ── 3. source() call at the top of app.R ─────────────────────────────────────

# Add alongside your other source() calls:
source("R/mod_metadata.R")
