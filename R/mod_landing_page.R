# =============================================================================
# R/mod_landing_page.R
# GCCEII Dashboard – Bilingual Landing Page Module
# =============================================================================
# Exports:
#   landing_page_ui(id, lang)   – UI function
#   landing_page_server(id, ...)  – Server function (minimal; most logic is JS)
#
# Dependencies:
#   R/translations.R  (must be sourced before this module)
#
# Integration in app.R:
#   1. In ui:     uiOutput("main_ui")
#   2. In server: output$main_ui <- renderUI({ landing_page_ui("landing", current_lang()) })
#                 observeEvent(input$enter_dashboard, { current_lang(input$selected_lang %||% "en"); ... })
# =============================================================================

# ── Dimension metadata for cards + modals ─────────────────────────────────────

.dim_meta <- list(
  list(
    id         = "trade",
    icon       = "exchange-alt",
    color      = "#1a6fa8",
    key_en     = "Trade Integration",
    key_ar     = "تكامل التجارة",
    desc_en    = "Intra-GCC trade flows, trade intensity, and export diversification.",
    desc_ar    = "تدفقات التجارة البينية ومعدلات الكثافة التجارية وتنويع الصادرات.",
    indicators_en = c(
      "Intra-GCC trade as % of total trade",
      "Trade intensity index",
      "Export product diversification (Herfindahl index)",
      "Trade in services integration",
      "Non-oil export share"
    ),
    indicators_ar = c(
      "التجارة البينية كنسبة من إجمالي التجارة",
      "مؤشر كثافة التجارة",
      "تنويع منتجات التصدير (مؤشر هيرفيندال)",
      "تكامل تجارة الخدمات",
      "حصة الصادرات غير النفطية"
    )
  ),
  list(
    id         = "financial",
    icon       = "university",
    color      = "#1e8449",
    key_en     = "Financial & Monetary Integration",
    key_ar     = "التكامل المالي والنقدي",
    desc_en    = "Cross-border capital flows, banking integration, and monetary convergence.",
    desc_ar    = "تدفقات رأس المال عبر الحدود والتكامل المصرفي والتقارب النقدي.",
    indicators_en = c(
      "Intra-GCC FDI flows",
      "Cross-border bank claims",
      "Stock market correlation",
      "Buna payment system adoption",
      "Interest rate convergence"
    ),
    indicators_ar = c(
      "تدفقات الاستثمار الأجنبي المباشر البيني",
      "المطالبات المصرفية عبر الحدود",
      "ارتباط أسواق الأسهم",
      "اعتماد نظام بونا للمدفوعات",
      "تقارب أسعار الفائدة"
    )
  ),
  list(
    id         = "labor",
    icon       = "users",
    color      = "#7d3c98",
    key_en     = "Labor & Human Capital Integration",
    key_ar     = "تكامل العمالة ورأس المال البشري",
    desc_en    = "Intra-GCC labor mobility, workforce skills, and education cooperation.",
    desc_ar    = "حركة العمالة البينية ومهارات القوى العاملة والتعاون في مجال التعليم.",
    indicators_en = c(
      "Intra-GCC migrant worker share",
      "GCC national workforce mobility",
      "University enrolment cross-border",
      "Professional qualification recognition",
      "Labour market regulatory convergence"
    ),
    indicators_ar = c(
      "حصة العمال المهاجرين البينيين",
      "تنقل القوى العاملة من مواطني دول المجلس",
      "التسجيل الجامعي عبر الحدود",
      "الاعتراف بالمؤهلات المهنية",
      "تقارب التشريعات العمالية"
    )
  ),
  list(
    id         = "infrastructure",
    icon       = "road",
    color      = "#c0392b",
    key_en     = "Infrastructure Connectivity",
    key_ar     = "ترابط البنية التحتية",
    desc_en    = "Physical and digital connectivity across GCC transport and energy networks.",
    desc_ar    = "الترابط المادي والرقمي عبر شبكات النقل والطاقة في دول المجلس.",
    indicators_en = c(
      "GCC Railway network progress (%)",
      "Electricity grid interconnection capacity",
      "Internet exchange bandwidth (intra-GCC)",
      "Cross-border road freight volume",
      "Port connectivity index"
    ),
    indicators_ar = c(
      "نسبة إنجاز مشروع سكة حديد مجلس التعاون",
      "طاقة الربط الكهربائي",
      "عرض نطاق نقطة تبادل الإنترنت البيني",
      "حجم شحن الطرق عبر الحدود",
      "مؤشر ترابط الموانئ"
    )
  ),
  list(
    id         = "sustainability",
    icon       = "leaf",
    color      = "#27ae60",
    key_en     = "Sustainability & Diversification",
    key_ar     = "الاستدامة والتنويع",
    desc_en    = "Economic diversification, renewable energy, and environmental sustainability.",
    desc_ar    = "التنويع الاقتصادي والطاقة المتجددة والاستدامة البيئية.",
    indicators_en = c(
      "Non-oil GDP share",
      "Renewable energy capacity (% of total)",
      "CO₂ intensity of GDP (inverted)",
      "Green investment flows (intra-GCC)",
      "Economic complexity index"
    ),
    indicators_ar = c(
      "حصة الناتج المحلي الإجمالي غير النفطي",
      "طاقة الطاقة المتجددة (% من الإجمالي)",
      "كثافة ثاني أكسيد الكربون في الناتج المحلي (معكوسة)",
      "تدفقات الاستثمار الأخضر البيني",
      "مؤشر التعقيد الاقتصادي"
    )
  ),
  list(
    id         = "convergence",
    icon       = "chart-line",
    color      = "#e67e22",
    key_en     = "Macroeconomic Convergence",
    key_ar     = "التقارب الاقتصادي الكلي",
    desc_en    = "Convergence in GDP growth, inflation, fiscal balances, and policy cycles.",
    desc_ar    = "التقارب في نمو الناتج المحلي والتضخم والأرصدة المالية ودورات السياسات.",
    indicators_en = c(
      "GDP per capita convergence (σ)",
      "Inflation rate dispersion",
      "Fiscal balance correlation",
      "Business cycle synchronisation",
      "Real effective exchange rate alignment"
    ),
    indicators_ar = c(
      "تقارب نصيب الفرد من الناتج المحلي (σ)",
      "تشتت معدلات التضخم",
      "ارتباط الأرصدة المالية",
      "تزامن دورات الأعمال",
      "توافق سعر الصرف الفعلي الحقيقي"
    )
  )
)

# ── Helper: single dimension card ─────────────────────────────────────────────

.dim_card <- function(meta, lang) {
  title <- if (lang == "ar") meta$key_ar else meta$key_en
  desc  <- if (lang == "ar") meta$desc_ar else meta$desc_en
  btn_label <- t("modal_learn_more", lang)

  tags$div(
    class = "dim-card",
    style = paste0("border-top: 4px solid ", meta$color, ";"),
    tags$div(
      class = "dim-card-icon",
      style = paste0("color: ", meta$color, ";"),
      icon(meta$icon, class = "fa-2x")
    ),
    tags$div(class = "dim-card-title", title),
    tags$div(class = "dim-card-desc", desc),
    tags$button(
      class = "btn btn-sm btn-outline dim-card-btn",
      style = paste0("color: ", meta$color, "; border-color: ", meta$color, ";"),
      `data-toggle` = "modal",
      `data-target` = paste0("#modal-", meta$id),
      btn_label
    )
  )
}

# ── Helper: single dimension modal ────────────────────────────────────────────

.dim_modal <- function(meta, lang) {
  title        <- if (lang == "ar") meta$key_ar else meta$key_en
  desc         <- if (lang == "ar") meta$desc_ar else meta$desc_en
  indicators   <- if (lang == "ar") meta$indicators_ar else meta$indicators_en
  ind_label    <- t("modal_indicators_label", lang)
  close_label  <- t("modal_close", lang)
  dir_attr     <- get_direction(lang)

  tags$div(
    class = "modal fade", id = paste0("modal-", meta$id),
    tabindex = "-1", role = "dialog",
    `aria-labelledby` = paste0("modal-", meta$id, "-label"),
    tags$div(
      class = "modal-dialog", role = "document",
      tags$div(
        class = "modal-content",
        # Header
        tags$div(
          class = "modal-header",
          style = paste0("border-left: 5px solid ", meta$color, ";"),
          tags$button(
            type = "button", class = "close",
            `data-dismiss` = "modal", `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "\u00d7")
          ),
          tags$h4(
            class = "modal-title",
            id    = paste0("modal-", meta$id, "-label"),
            style = paste0("color:", meta$color, "; direction:", dir_attr),
            icon(meta$icon), " ", title
          )
        ),
        # Body
        tags$div(
          class = "modal-body",
          style = paste0("direction:", dir_attr, "; text-align:", if (lang == "ar") "right" else "left"),
          tags$p(desc),
          tags$hr(),
          tags$strong(ind_label),
          tags$ul(lapply(indicators, tags$li))
        ),
        # Footer
        tags$div(
          class = "modal-footer",
          tags$button(
            type = "button", class = "btn btn-default",
            `data-dismiss` = "modal",
            close_label
          )
        )
      )
    )
  )
}

# ── UI function ───────────────────────────────────────────────────────────────

#' Landing page UI
#'
#' @param id   Shiny module namespace id.
#' @param lang Character. "en" or "ar".
#' @return A tagList ready to be inserted into uiOutput("main_ui").
landing_page_ui <- function(id, lang = "en") {
  ns  <- NS(id)
  dir <- get_direction(lang)

  # Carousel quotes
  quotes <- list(
    t("quote_1", lang), t("quote_2", lang),
    t("quote_3", lang), t("quote_4", lang)
  )

  tagList(

    # ── Hidden input that holds the chosen language ──────────────────────────
    tags$input(
      id = "selected_lang", type = "hidden", value = lang
    ),

    # ── Language toggle (fixed overlay) ─────────────────────────────────────
    tags$div(
      id = "lang-toggle-container",
      tags$button(
        class = paste("lang-btn", if (lang == "en") "active" else ""),
        id    = "btn-lang-en",
        onclick = "setLang('en')",
        t("lang_toggle_en", lang)
      ),
      tags$button(
        class = paste("lang-btn", if (lang == "ar") "active" else ""),
        id    = "btn-lang-ar",
        onclick = "setLang('ar')",
        t("lang_toggle_ar", lang)
      )
    ),

    # ── Page wrapper ─────────────────────────────────────────────────────────
    tags$div(
      class = paste("landing-page", if (lang == "ar") "lang-ar" else ""),
      id    = "landing-page",
      style = paste0("direction:", dir),

      # ── Hero section ──────────────────────────────────────────────────────
      tags$section(
        id    = "landing-hero",
        class = "landing-hero",
        # GCC-Stat logo placeholder (replace src with actual path under www/)
        tags$div(
          class = "hero-logo",
          tags$img(
            src   = "gccstat_logo.png",
            alt   = "GCC-Stat",
            style = "height:60px; margin-bottom:16px;"
          )
        ),
        tags$h1(
          class = "hero-title",
          t("landing_title", lang)
        ),
        tags$p(
          class = "hero-subtitle",
          t("landing_subtitle", lang)
        ),
        # Enter dashboard button
        tags$button(
          class   = "btn btn-primary btn-lg enter-btn",
          id      = ns("enter_btn"),
          onclick = paste0("Shiny.setInputValue('", ns("enter_dashboard"), "', true, {priority: 'event'})"),
          icon("arrow-right"), " ",
          t("landing_enter_dashboard", lang)
        )
      ),

      # ── Carousel / pull-quotes ────────────────────────────────────────────
      tags$section(
        class = "landing-carousel",
        tags$div(
          id    = "quote-carousel",
          class = "carousel slide",
          `data-ride`    = "carousel",
          `data-interval` = "5000",
          # Indicators
          tags$ol(
            class = "carousel-indicators",
            lapply(0:3, function(i) {
              tags$li(
                `data-target` = "#quote-carousel",
                `data-slide-to` = i,
                class = if (i == 0) "active" else ""
              )
            })
          ),
          # Slides
          tags$div(
            class = "carousel-inner",
            lapply(seq_along(quotes), function(i) {
              tags$div(
                class = paste("item carousel-quote-item", if (i == 1) "active" else ""),
                tags$blockquote(
                  class = "carousel-quote",
                  tags$p(quotes[[i]])
                )
              )
            })
          ),
          # Controls
          tags$a(
            class = "left carousel-control", href = "#quote-carousel",
            `data-slide` = "prev",
            tags$span(class = "glyphicon glyphicon-chevron-left"),
            tags$span(class = "sr-only", "Previous")
          ),
          tags$a(
            class = "right carousel-control", href = "#quote-carousel",
            `data-slide` = "next",
            tags$span(class = "glyphicon glyphicon-chevron-right"),
            tags$span(class = "sr-only", "Next")
          )
        )
      ),

      # ── Stat boxes ────────────────────────────────────────────────────────
      tags$section(
        class = "landing-stats container-fluid",
        fluidRow(
          lapply(
            list(
              list(value = "6",   key = "stat_countries"),
              list(value = "6",   key = "stat_dimensions"),
              list(value = "9",   key = "stat_years"),
              list(value = "45+", key = "stat_indicators")
            ),
            function(s) {
              column(3,
                tags$div(
                  class = "landing-stat-box",
                  tags$div(class = "landing-stat-value", s$value),
                  tags$div(class = "landing-stat-label",  t(s$key, lang))
                )
              )
            }
          )
        )
      ),

      # ── About section ─────────────────────────────────────────────────────
      tags$section(
        id    = "landing-about",
        class = "landing-about",
        tags$div(
          class = "container",
          tags$h2(t("landing_about_title", lang)),
          tags$p(t("landing_about_body", lang))
        )
      ),

      # ── Dimension cards ───────────────────────────────────────────────────
      tags$section(
        class = "landing-dimensions",
        tags$div(
          class = "container-fluid",
          fluidRow(
            lapply(.dim_meta, function(meta) {
              column(2, .dim_card(meta, lang))
            })
          )
        )
      ),

      # ── Dimension modals ──────────────────────────────────────────────────
      lapply(.dim_meta, function(meta) .dim_modal(meta, lang)),

      # ── Footer ────────────────────────────────────────────────────────────
      tags$footer(
        id    = "landing-footer",
        class = "landing-footer",
        tags$p(t("footer_produced_by", lang)),
        tags$p(t("footer_methodology", lang))
      )
    ), # end .landing-page

    # ── Inline CSS: landing page layout (structural, both languages) ─────────
    tags$head(
      tags$style(HTML("
        /* ── Reset & page shell ── */
        .landing-page {
          min-height: 100vh;
          background: #ffffff;
          font-family: 'Source Sans Pro', Arial, sans-serif;
        }

        /* ── Hero ── */
        .landing-hero {
          background: linear-gradient(135deg, #1a3a5c 0%, #2e86c1 60%, #1a6fa8 100%);
          color: white;
          text-align: center;
          padding: 80px 20px 60px;
        }
        .hero-logo { margin-bottom: 12px; }
        .hero-title {
          font-size: 2.6rem;
          font-weight: 700;
          margin-bottom: 14px;
          letter-spacing: -0.5px;
        }
        .hero-subtitle {
          font-size: 1.15rem;
          opacity: 0.9;
          max-width: 680px;
          margin: 0 auto 30px;
          white-space: pre-line;
        }
        .enter-btn {
          font-size: 1.1rem;
          padding: 12px 36px;
          border-radius: 30px;
          background: #ffffff;
          color: #1a3a5c;
          border: none;
          font-weight: 600;
          transition: transform 0.15s, box-shadow 0.15s;
        }
        .enter-btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.2);
          background: #eaf3fb;
          color: #1a3a5c;
        }

        /* ── Carousel / quotes ── */
        .landing-carousel {
          background: #1a3a5c;
          padding: 30px 60px;
        }
        .carousel-quote-item {
          padding: 10px 40px;
        }
        .carousel-quote {
          text-align: center;
          border: none;
          color: #cfe2f3;
          font-size: 1.1rem;
          font-style: italic;
          padding: 0;
          margin: 0 auto;
          max-width: 750px;
        }
        .carousel-control { opacity: 0.6; }
        .carousel-indicators li { background-color: #7fb3d3; }
        .carousel-indicators .active { background-color: #fff; }

        /* ── Stat boxes ── */
        .landing-stats {
          background: #f4f6f9;
          padding: 40px 0;
        }
        .landing-stat-box {
          text-align: center;
          padding: 20px;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          margin: 6px;
        }
        .landing-stat-value {
          font-size: 2.2rem;
          font-weight: 700;
          color: #2e86c1;
        }
        .landing-stat-label {
          font-size: 0.9rem;
          color: #666;
          margin-top: 4px;
        }

        /* ── About ── */
        .landing-about {
          padding: 50px 20px;
          background: #fff;
          max-width: 820px;
          margin: 0 auto;
        }
        .landing-about h2 {
          color: #1a3a5c;
          font-size: 1.6rem;
          margin-bottom: 16px;
        }
        .landing-about p {
          font-size: 1rem;
          line-height: 1.75;
          color: #444;
        }

        /* ── Dimension cards ── */
        .landing-dimensions {
          background: #f4f6f9;
          padding: 40px 20px;
        }
        .dim-card {
          background: white;
          border-radius: 8px;
          padding: 20px 14px;
          margin: 6px 0;
          text-align: center;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          min-height: 220px;
          display: flex;
          flex-direction: column;
          align-items: center;
          gap: 8px;
        }
        .dim-card-icon { margin-bottom: 6px; }
        .dim-card-title {
          font-weight: 700;
          font-size: 0.9rem;
          color: #1a3a5c;
          line-height: 1.3;
        }
        .dim-card-desc {
          font-size: 0.8rem;
          color: #666;
          line-height: 1.4;
          flex-grow: 1;
        }
        .dim-card-btn {
          font-size: 0.78rem;
          border-radius: 20px;
          padding: 3px 12px;
        }
        .dim-card-btn:hover {
          opacity: 0.8;
        }

        /* ── Footer ── */
        .landing-footer {
          background: #1a3a5c;
          color: #aac4dd;
          text-align: center;
          padding: 24px 20px;
          font-size: 0.85rem;
        }
        .landing-footer p { margin: 4px 0; }
      "))
    ),

    # ── JavaScript: language switching ───────────────────────────────────────
    tags$script(HTML("
      function setLang(lang) {
        // Update hidden Shiny input so server knows the chosen language
        Shiny.setInputValue('selected_lang', lang, {priority: 'event'});

        // Toggle active class on buttons
        document.getElementById('btn-lang-en').classList.toggle('active', lang === 'en');
        document.getElementById('btn-lang-ar').classList.toggle('active', lang === 'ar');

        // Apply / remove RTL class on body for instant CSS response
        if (lang === 'ar') {
          document.body.classList.add('lang-ar');
        } else {
          document.body.classList.remove('lang-ar');
        }
      }

      // On load: apply class if server rendered in Arabic
      (function() {
        var lang = document.getElementById('selected_lang');
        if (lang && lang.value === 'ar') {
          document.body.classList.add('lang-ar');
        }
      })();
    "))

  ) # end tagList
}

# ── Server function ────────────────────────────────────────────────────────────

#' Landing page server (minimal – most behaviour is in app.R server)
#'
#' The main responsibilities here are:
#'   - Returning the enter_dashboard reactive so app.R can switch views.
#'   - The language reactive is managed in app.R via input$selected_lang.
#'
#' @param id Shiny module namespace id.
landing_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return the enter-dashboard trigger as a reactive
    enter <- reactive({
      input$enter_dashboard
    })
    return(list(enter = enter))
  })
}
