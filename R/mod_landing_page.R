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
#' @return Shiny UI element for landing page
landing_page_ui <- function(lang = "en") {
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
        }, 100);
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

    // Close modal on Escape key
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
