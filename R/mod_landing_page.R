# ==============================================================================
# LANDING PAGE MODULE
# ==============================================================================
# UI and server components for the landing page with hero carousel
# Bilingual: English / Arabic via R/translations.R
# ==============================================================================

#' Helper: build a tags$ul from a newline-separated translation key
#' @param key  Translation key whose value contains \\n-separated items
#' @param lang "en" or "ar"
.indicator_list <- function(key, lang) {
  items <- strsplit(t(key, lang), "\n")[[1]]
  do.call(tags$ul, lapply(items, tags$li))
}

#' Landing Page UI
#'
#' @param lang "en" or "ar"
#' @param exec_data Named list from exec_summary_data reactive, or NULL
#' @return Shiny UI element for landing page
landing_page_ui <- function(lang = "en", exec_data = NULL) {
  div(class = "landing-container",

      # Hero Section
      div(class = "landing-hero",

          # Background images (must be first)
          div(class = "hero-bg hero-bg-1 active"),
          div(class = "hero-bg hero-bg-2"),
          div(class = "hero-bg hero-bg-3"),
          div(class = "hero-bg hero-bg-4"),

          # Dark overlay
          div(class = "hero-overlay"),

          # Language toggle (floating top-right)
          div(id = "lang-toggle-container",
            actionButton("lang_toggle",
              label = if (lang == "ar") "English" else "\u0639\u0631\u0628\u064a",
              class = "lang-btn"
            )
          ),

          # GCC-Stat Logo
          img(src = "images/GCC-MAIN-01-WHITE.png", class = "landing-logo"),

          # Title
          h1(class = "landing-title", t("landing_title", lang)),

          # Subtitle
          p(class = "landing-subtitle", t("app_subtitle", lang)),

          # Rotating Quote Carousel
          div(class = "quote-carousel", id = "quote-display",
              t("carousel_1", lang)
          ),

          # Carousel navigation dots
          div(class = "carousel-dots",
              tags$button(class = "carousel-dot active", onclick = "showQuote(0)"),
              tags$button(class = "carousel-dot", onclick = "showQuote(1)"),
              tags$button(class = "carousel-dot", onclick = "showQuote(2)"),
              tags$button(class = "carousel-dot", onclick = "showQuote(3)")
          ),

          # Enter Dashboard button
          actionButton("enter_dashboard", t("landing_enter_dashboard", lang),
                       class = "enter-btn",
                       icon = icon("arrow-right"))
      ),

      # About Section
      landing_about_section(lang),

      # Dimensions Preview Section
      landing_dimensions_section(lang),

      # Footer
      landing_footer_section(lang),

      # Executive Summary modal (rendered with live score data)
      exec_summary_modal(lang, exec_data),

      # Carousel + modal JavaScript (language-aware)
      carousel_js(lang)
  )
}

#' Landing Page About Section
#'
#' @param lang "en" or "ar"
#' @return Shiny UI element
landing_about_section <- function(lang = "en") {
  div(class = "landing-about",
      h2(t("landing_about_heading", lang)),
      p(class = "landing-about-text", t("landing_about_p1", lang)),
      p(class = "landing-about-text",
        t("landing_about_p2", lang)
      ),

      # Key statistics
      div(class = "stats-row",
          div(class = "stat-box",
              div(class = "number", "6"),
              div(class = "label", t("stat_countries", lang))
          ),
          div(class = "stat-box",
              div(class = "number", "6"),
              div(class = "label", t("stat_dimensions", lang))
          ),
          div(class = "stat-box",
              div(class = "number", "32"),
              div(class = "label", t("stat_indicators", lang))
          ),
          div(class = "stat-box",
              div(class = "number", "2015-2024"),
              div(class = "label", t("stat_timeseries", lang))
          )
      ),

      # Executive Summary button
      div(class = "exec-summary-btn-row",
          tags$button(
            class = "exec-summary-open-btn",
            onclick = "openExecModal()",
            icon("file-alt"),
            span(t("exec_summary_btn", lang))
          )
      )
  )
}

#' Landing Page Dimensions Section
#'
#' @param lang "en" or "ar"
#' @return Shiny UI element
landing_dimensions_section <- function(lang = "en") {
  div(class = "dimensions-section",
      h3(t("landing_dim_heading", lang)),
      p(style = "text-align: center; color: #666; margin-bottom: 30px;",
        t("landing_dim_subtitle", lang)),

      # Circular buttons
      div(class = "dimension-circles",

          # Trade Integration
          div(class = "dim-circle dim-circle-trade",
              onclick = "openDimModal('trade')",
              div(class = "dim-circle-icon", icon("exchange-alt")),
              div(class = "dim-circle-label", t("dim_trade", lang))
          ),

          # Financial Integration
          div(class = "dim-circle dim-circle-financial",
              onclick = "openDimModal('financial')",
              div(class = "dim-circle-icon", icon("university")),
              div(class = "dim-circle-label", t("dim_financial", lang))
          ),

          # Labor Mobility
          div(class = "dim-circle dim-circle-labor",
              onclick = "openDimModal('labor')",
              div(class = "dim-circle-icon", icon("users")),
              div(class = "dim-circle-label", t("dim_labor", lang))
          ),

          # Infrastructure
          div(class = "dim-circle dim-circle-infrastructure",
              onclick = "openDimModal('infrastructure')",
              div(class = "dim-circle-icon", icon("road")),
              div(class = "dim-circle-label", t("dim_infrastructure", lang))
          ),

          # Sustainability
          div(class = "dim-circle dim-circle-sustainability",
              onclick = "openDimModal('sustainability')",
              div(class = "dim-circle-icon", icon("leaf")),
              div(class = "dim-circle-label", t("dim_sustainability", lang))
          ),

          # Convergence
          div(class = "dim-circle dim-circle-convergence",
              onclick = "openDimModal('convergence')",
              div(class = "dim-circle-icon", icon("chart-line")),
              div(class = "dim-circle-label", t("dim_convergence", lang))
          )
      ),

      # ===== MODALS =====
      dimension_modals(lang)
  )
}

#' Single Dimension Modal
#'
#' @param dim_id     Short id: "trade", "financial", etc.
#' @param icon_name  FontAwesome icon name
#' @param title_key  Translation key for full dimension title
#' @param desc_key   Translation key for description paragraph
#' @param ind_key    Translation key for newline-separated indicator list
#' @param lang       "en" or "ar"
.dim_modal <- function(dim_id, icon_name, title_key, desc_key, ind_key, lang) {
  div(class = "dim-modal-overlay", id = paste0("modal-", dim_id),
      onclick = paste0("closeDimModal(event, '", dim_id, "')"),
      div(class = paste("dim-modal", paste0("dim-modal-", dim_id)),
          onclick = "event.stopPropagation()",
          div(class = "dim-modal-header",
              icon(icon_name),
              h4(t(title_key, lang)),
              tags$button(class = "dim-modal-close",
                          onclick = paste0("closeDimModal(event, '", dim_id, "')"),
                          HTML("&times;"))
          ),
          div(class = "dim-modal-body",
              p(t(desc_key, lang)),
              h5(icon("search"), t("modal_indicators_heading", lang)),
              .indicator_list(ind_key, lang)
          )
      )
  )
}

#' Dimension Modals
#'
#' @param lang "en" or "ar"
#' @return Shiny UI element with all dimension modals
dimension_modals <- function(lang = "en") {
  tagList(
    .dim_modal("trade",          "exchange-alt", "modal_dim_trade",          "modal_trade_desc",          "modal_trade_indicators",          lang),
    .dim_modal("financial",      "university",   "modal_dim_financial",      "modal_financial_desc",      "modal_financial_indicators",      lang),
    .dim_modal("labor",          "users",        "modal_dim_labor",          "modal_labor_desc",          "modal_labor_indicators",          lang),
    .dim_modal("infrastructure", "road",         "modal_dim_infrastructure", "modal_infrastructure_desc", "modal_infrastructure_indicators", lang),
    .dim_modal("sustainability", "leaf",         "modal_dim_sustainability", "modal_sustainability_desc", "modal_sustainability_indicators", lang),
    .dim_modal("convergence",    "chart-line",   "modal_dim_convergence",    "modal_convergence_desc",    "modal_convergence_indicators",    lang)
  )
}

#' Executive Summary Modal
#'
#' Displays the GCC composite score, per-dimension scores, key findings, and
#' links to the narrative reports. Uses the same overlay pattern as the
#' dimension modals.
#'
#' @param lang "en" or "ar"
#' @param data Named list from exec_summary_data reactive:
#'   year, composite, delta, dimensions (named numeric), dim_deltas (named numeric)
#' @return Shiny UI element
exec_summary_modal <- function(lang = "en", data = NULL) {

  # Helper: score value + trend arrow HTML
  fmt_score <- function(val, delta) {
    arrow  <- if (delta >  0.5) "\u2191" else if (delta < -0.5) "\u2193" else "\u2192"
    colour <- if (delta >  0.5) "#2e7d32" else if (delta < -0.5) "#c62828" else "#888"
    HTML(paste0(
      '<span class="dim-score-val">',   sprintf("%.1f", val), '</span>',
      '<span class="dim-score-arrow" style="color:', colour, ';">', arrow, '</span>'
    ))
  }

  dims   <- if (!is.null(data)) data$dimensions else NULL
  deltas <- if (!is.null(data)) data$dim_deltas  else NULL

  dim_order <- c("Trade", "Financial", "Labor",
                 "Infrastructure", "Sustainability", "Convergence")
  dim_icons <- c(Trade          = "exchange-alt",
                 Financial      = "university",
                 Labor          = "users",
                 Infrastructure = "road",
                 Sustainability = "leaf",
                 Convergence    = "chart-line")

  dim_cards <- lapply(dim_order, function(d) {
    score <- if (!is.null(dims))   dims[[d]]   else NA
    delta <- if (!is.null(deltas)) deltas[[d]] else 0
    div(class = paste("exec-dim-card", paste0("exec-dim-", tolower(d))),
        div(class = "exec-dim-icon", icon(dim_icons[[d]])),
        div(class = "exec-dim-name", t(paste0("dim_", tolower(d)), lang)),
        if (!is.na(score)) fmt_score(score, delta)
        else span(class = "dim-score-na", "\u2014")
    )
  })

  # Composite score block content
  comp_val   <- if (!is.null(data)) sprintf("%.1f", data$composite) else "\u2014"
  comp_delta <- if (!is.null(data)) data$delta else 0
  delta_txt  <- if (!is.null(data)) {
    if (comp_delta > 0) paste0("\u25b2 +", sprintf("%.1f", comp_delta))
    else if (comp_delta < 0) paste0("\u25bc ", sprintf("%.1f", comp_delta))
    else "\u2192 0.0"
  } else NULL
  delta_col  <- if (!is.null(data)) {
    if (comp_delta > 0) "#a5d6a7" else if (comp_delta < 0) "#ef9a9a" else "#ccc"
  } else "#ccc"

  div(class = "dim-modal-overlay", id = "modal-exec-summary",
      onclick = "closeExecModal(event)",

      div(class = "dim-modal exec-summary-modal",
          onclick = "event.stopPropagation()",

          # ── Header ──────────────────────────────────────────────────────────
          div(class = "dim-modal-header exec-summary-header",

              div(class = "exec-summary-header-left",
                  icon("chart-bar"),
                  div(
                    h4(t("exec_summary_title", lang)),
                    span(class = "exec-year-label",
                         t("exec_summary_year_label", lang), ": ",
                         if (!is.null(data)) data$year else "\u2014")
                  )
              ),

              div(class = "exec-composite-score",
                  div(class = "exec-score-label", t("exec_summary_score_label", lang)),
                  div(class = "exec-score-value", comp_val),
                  if (!is.null(delta_txt))
                    div(class = "exec-score-delta",
                        style = paste0("color:", delta_col, ";"),
                        delta_txt)
              ),

              tags$button(class = "dim-modal-close",
                          onclick = "closeExecModal(event)",
                          HTML("&times;"))
          ),

          # ── Body ─────────────────────────────────────────────────────────────
          div(class = "dim-modal-body exec-summary-body",

              # Key findings
              h5(icon("lightbulb"), " ", t("exec_summary_findings_heading", lang)),
              div(class = "exec-findings-list",
                  lapply(
                    strsplit(t("exec_summary_findings", lang), "\n")[[1]],
                    function(f) {
                      div(class = "exec-finding",
                          icon("angle-right"),
                          span(f))
                    }
                  )
              ),

              # Dimension score grid
              h5(icon("th"), " ", t("exec_summary_dim_heading", lang)),
              div(class = "exec-dim-grid", dim_cards),

              # Report links — both in current UI language
              div(class = "exec-report-btns",
                  tags$button(
                    class = "exec-report-btn exec-report-btn-exec",
                    onclick = if (lang == "ar")
                      "window.open('reports/GCC_EII_Executive_Summary_AR.html','_blank')"
                    else
                      "window.open('reports/GCC_EII_Executive_Summary.html','_blank')",
                    icon("file-alt"),
                    span(t("exec_read_exec_summary", lang))
                  ),
                  tags$button(
                    class = "exec-report-btn exec-report-btn-narrative",
                    onclick = if (lang == "ar")
                      "window.open('reports/GCC_EII_Narrative_AR.html','_blank')"
                    else
                      "window.open('reports/GCC_EII_Narrative_Report.html','_blank')",
                    icon("book-open"),
                    span(t("exec_read_full_report", lang))
                  )
              )
          )
      )
  )
}


#' Landing Page Footer Section
#'
#' @param lang "en" or "ar"
#' @return Shiny UI element
landing_footer_section <- function(lang = "en") {
  div(class = "landing-footer",
      p(
        t("footer_copyright", lang), " | ",
        tags$a(href = "https://gccstat.org", target = "_blank", "www.gccstat.org")
      )
  )
}

#' Carousel JavaScript
#'
#' Embeds translated quotes directly into the JS array so the carousel
#' displays the correct language. Since landing_page_ui() is called inside
#' renderUI and depends on current_lang(), it re-renders on language switch.
#'
#' @param lang "en" or "ar"
#' @return Shiny tags$script element
carousel_js <- function(lang = "en") {
  q1 <- gsub("'", "\\\\'", t("carousel_1", lang))
  q2 <- gsub("'", "\\\\'", t("carousel_2", lang))
  q3 <- gsub("'", "\\\\'", t("carousel_3", lang))
  q4 <- gsub("'", "\\\\'", t("carousel_4", lang))

  tags$script(HTML(sprintf("
    var quotes = ['%s','%s','%s','%s'];

    var currentQuote = 0;
    var autoRotate;

    function showQuote(index) {
      currentQuote = index;

      // Update quote text with fade
      var display = document.getElementById('quote-display');
      if (display) {
        display.style.opacity = 0;
        setTimeout(function() {
          display.innerText = quotes[index];
          display.style.opacity = 1;
        }, 300);
      }

      // Update background images
      var bgs = document.querySelectorAll('.hero-bg');
      bgs.forEach(function(bg, i) {
        if (i === index) {
          bg.classList.add('active');
        } else {
          bg.classList.remove('active');
        }
      });

      // Update dots
      var dots = document.querySelectorAll('.carousel-dot');
      dots.forEach(function(dot, i) {
        if (i === index) {
          dot.classList.add('active');
        } else {
          dot.classList.remove('active');
        }
      });
    }

    function startCarousel() {
      if (autoRotate) clearInterval(autoRotate);
      autoRotate = setInterval(function() {
        currentQuote = (currentQuote + 1) %% quotes.length;
        showQuote(currentQuote);
      }, 7000);
    }

    $(document).on('shiny:value', function(event) {
      if (event.name === 'main_ui') {
        setTimeout(function() {
          var display = document.getElementById('quote-display');
          if (display) {
            display.style.transition = 'opacity 0.3s ease';
            startCarousel();
          }
        }, 500);
      }
    });

    // fallback: fires when Shiny finishes all rendering
    $(document).one('shiny:idle', function() {
      if (!autoRotate) {
        var display = document.getElementById('quote-display');
        if (display) {
          display.style.transition = 'opacity 0.3s ease';
          startCarousel();
        }
      }
    });

    // ===== DIMENSION MODALS =====
    function openDimModal(dimension) {
      document.getElementById('modal-' + dimension).classList.add('active');
      document.body.style.overflow = 'hidden';
    }

    function closeDimModal(event, dimension) {
      if (event.target === event.currentTarget || event.target.classList.contains('dim-modal-close')) {
        document.getElementById('modal-' + dimension).classList.remove('active');
        document.body.style.overflow = 'auto';
      }
    }

    // ===== EXECUTIVE SUMMARY MODAL =====
    function openExecModal() {
      var el = document.getElementById('modal-exec-summary');
      if (el) {
        el.classList.add('active');
        document.body.style.overflow = 'hidden';
      }
    }

    function closeExecModal(event) {
      if (event.target === event.currentTarget ||
          event.target.classList.contains('dim-modal-close')) {
        var el = document.getElementById('modal-exec-summary');
        if (el) el.classList.remove('active');
        document.body.style.overflow = 'auto';
      }
    }

    // Close ALL modals (dimension + exec summary) on Escape key
    document.addEventListener('keydown', function(event) {
      if (event.key === 'Escape') {
        document.querySelectorAll('.dim-modal-overlay.active').forEach(function(modal) {
          modal.classList.remove('active');
        });
        document.body.style.overflow = 'auto';
      }
    });
  ", q1, q2, q3, q4)))
}
