# =============================================================================
# app.R – GCCEII bilingual wiring snippet
# =============================================================================
# This file shows ONLY the changes needed to integrate mod_landing_page.R
# and the language system into your existing app.R.
# Lines marked NEW need to be added; lines marked REPLACE swap existing code.
# =============================================================================

# ── 1. Source new modules (add after existing source() / library() calls) ────

source("R/translations.R")       # NEW
source("R/mod_landing_page.R")   # NEW


# ── 2. Load RTL CSS in dashboardBody (REPLACE existing tags$head block) ──────

dashboardBody(
  tags$head(
    # Existing styles
    tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box             { box-shadow: 0 1px 3px rgba(0,0,0,0.12); }
      .small-box       { box-shadow: 0 2px 4px rgba(0,0,0,0.15); }
      .info-box        { box-shadow: 0 1px 3px rgba(0,0,0,0.12); }
    ")),
    # NEW: RTL stylesheet
    tags$link(rel = "stylesheet", href = "css/rtl.css"),
    # NEW: Arabic Google Fonts (preload for performance)
    tags$link(
      rel  = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    tags$link(
      rel         = "stylesheet",
      href        = "https://fonts.googleapis.com/css2?family=Cairo:wght@300;400;600;700&family=Tajawal:wght@400;500;700&display=swap",
      crossorigin = NA
    )
  ),
  # Use uiOutput so the entire visible area can switch between landing / dashboard
  uiOutput("main_ui")    # NEW – replaces the existing tabItems(...) block
)


# ── 3. Server-side language + page-routing logic (add to server function) ────

server <- function(input, output, session) {

  # NEW: reactive language state (default English)
  current_lang <- reactiveVal("en")

  # NEW: react to language toggle from landing page JS
  observeEvent(input$selected_lang, {
    current_lang(input$selected_lang)
    # Push direction change to browser for dashboard pages too
    session$sendCustomMessage(
      "updateDirection",
      list(dir = get_direction(input$selected_lang))
    )
  })

  # NEW: page state – "landing" or "dashboard"
  current_page <- reactiveVal("landing")

  # NEW: server-side landing page module
  landing <- landing_page_server("landing")

  # NEW: switch to dashboard when Enter button is clicked
  observeEvent(landing$enter(), {
    req(landing$enter())
    current_page("dashboard")
  })

  # NEW: render the appropriate top-level UI
  output$main_ui <- renderUI({
    if (current_page() == "landing") {
      landing_page_ui("landing", lang = current_lang())
    } else {
      # ── Existing dashboard tabItems go here ──────────────────────────────
      # Pass current_lang() to any tab UI helpers that need it, e.g.:
      # dashboard_body_ui(lang = current_lang())
      # Or keep the existing tabItems(...) block unchanged for now:
      tagList(
        tabItems(
          # ... your existing tab definitions ...
        )
      )
    }
  })

  # ── All your existing server observers / renderPlotly / etc. go below ──────
  # To pass language to chart renders:
  #   output$some_chart <- renderPlotly({ create_chart(..., lang = current_lang()) })

}
