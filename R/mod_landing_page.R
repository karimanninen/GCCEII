# ==============================================================================
# LANDING PAGE MODULE
# ==============================================================================
# UI and server components for the landing page with hero carousel
# ==============================================================================

#' Landing Page UI
#'
#' @return Shiny UI element for landing page
landing_page_ui <- function() {
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
          h1(class = "landing-title", "GCC Economic Integration Index"),

          # Subtitle
          p(class = "landing-subtitle",
            "Measuring Regional Integration Across the Gulf Cooperation Council"),

          # Rotating Quote Carousel
          div(class = "quote-carousel", id = "quote-display",
              "Economic integration is a strategic priority for the GCC. All member states have a role in building a unified, diversified regional economy."
          ),

          # Carousel navigation dots
          div(class = "carousel-dots",
              tags$button(class = "carousel-dot active", onclick = "showQuote(0)"),
              tags$button(class = "carousel-dot", onclick = "showQuote(1)"),
              tags$button(class = "carousel-dot", onclick = "showQuote(2)"),
              tags$button(class = "carousel-dot", onclick = "showQuote(3)")
          ),

          # Enter Dashboard button
          actionButton("enter_dashboard", "Enter Dashboard",
                       class = "enter-btn",
                       icon = icon("arrow-right"))
      ),

      # About Section
      landing_about_section(),

      # Dimensions Preview Section
      landing_dimensions_section(),

      # Footer
      landing_footer_section()
  )
}

#' Landing Page About Section
#'
#' @return Shiny UI element
landing_about_section <- function() {
  div(class = "landing-about",
      h2("About the GCC Economic Integration Index"),
      p(class = "landing-about-text",
        "As the GCC region continues to pursue deeper economic, social, and institutional integration,
        the need for robust measurement tools becomes increasingly essential. Inspired by global models
        such as the Asia-Pacific Regional Cooperation and Integration Index (ARCII) and the Global
        Integration Index (GII), the GCC Regional Cooperation and Integration Index (GCC-RCII) offers
        a comprehensive, multidimensional framework to assess the progress of regional integration across
        GCC economies."
      ),
      p(class = "landing-about-text",
        "The GCC-RCII tracks integration across key areas including trade, finance, value-chain linkages,
        infrastructure connectivity, movement of people, institutional alignment, digital integration,
        and environmental cooperation. It provides policymakers, researchers, and practitioners with
        evidence-based insights to identify strengths, monitor gaps, and guide regional policy coordination."
      ),

      # Key statistics
      div(class = "stats-row",
          div(class = "stat-box",
              div(class = "number", "6"),
              div(class = "label", "Member States")
          ),
          div(class = "stat-box",
              div(class = "number", "6"),
              div(class = "label", "Dimensions")
          ),
          div(class = "stat-box",
              div(class = "number", "40+"),
              div(class = "label", "Indicators")
          ),
          div(class = "stat-box",
              div(class = "number", "2015-2023"),
              div(class = "label", "Time Series")
          )
      )
  )
}

#' Landing Page Dimensions Section
#'
#' @return Shiny UI element
landing_dimensions_section <- function() {
  div(class = "dimensions-section",
      h3("Six Integration Dimensions"),
      p(style = "text-align: center; color: #666; margin-bottom: 30px;",
        "Click on each dimension to learn more"),
      
      # Circular buttons
      div(class = "dimension-circles",
          
          # Trade Integration
          div(class = "dim-circle dim-circle-trade",
              onclick = "openDimModal('trade')",
              div(class = "dim-circle-icon", icon("exchange-alt")),
              div(class = "dim-circle-label", "Trade Integration")
          ),
          
          # Financial Integration
          div(class = "dim-circle dim-circle-financial",
              onclick = "openDimModal('financial')",
              div(class = "dim-circle-icon", icon("university")),
              div(class = "dim-circle-label", "Financial Integration")
          ),
          
          # Labor Mobility
          div(class = "dim-circle dim-circle-labor",
              onclick = "openDimModal('labor')",
              div(class = "dim-circle-icon", icon("users")),
              div(class = "dim-circle-label", "Labor & Human Capital")
          ),
          
          # Infrastructure
          div(class = "dim-circle dim-circle-infrastructure",
              onclick = "openDimModal('infrastructure')",
              div(class = "dim-circle-icon", icon("road")),
              div(class = "dim-circle-label", "Infrastructure Connectivity")
          ),
          
          # Sustainability
          div(class = "dim-circle dim-circle-sustainability",
              onclick = "openDimModal('sustainability')",
              div(class = "dim-circle-icon", icon("leaf")),
              div(class = "dim-circle-label", "Sustainability")
          ),
          
          # Convergence
          div(class = "dim-circle dim-circle-convergence",
              onclick = "openDimModal('convergence')",
              div(class = "dim-circle-icon", icon("chart-line")),
              div(class = "dim-circle-label", "Economic Convergence")
          )
      ),
      
      # ===== MODALS =====
      dimension_modals()
  )
}

#' Dimension Modals
#'
#' @return Shiny UI element with all dimension modals
dimension_modals <- function() {
  tagList(
    # Trade Modal
    div(class = "dim-modal-overlay", id = "modal-trade",
        onclick = "closeDimModal(event, 'trade')",
        div(class = "dim-modal dim-modal-trade",
            onclick = "event.stopPropagation()",
            div(class = "dim-modal-header",
                icon("exchange-alt"),
                h4("Trade Integration"),
                tags$button(class = "dim-modal-close", 
                            onclick = "closeDimModal(event, 'trade')", 
                            HTML("&times;"))
            ),
            div(class = "dim-modal-body",
                p("Measures the intensity and composition of intra-GCC merchandise and services trade, 
                  with emphasis on non-oil diversification and regional value chain participation. 
                  Higher trade integration reflects stronger economic interdependence among member states."),
                h5(icon("search"), "Indicators"),
                tags$ul(
                  tags$li("Intra-GCC Trade Intensity"),
                  tags$li("Services Trade Share"),
                  tags$li("Non-Oil Trade Share"),
                  tags$li("Services as % of Total Trade"),
                  tags$li("Intermediate Goods Share"),
                  tags$li("Trade by Product Type")
                  
                )
            )
        )
    ),
    
    # Financial Modal
    div(class = "dim-modal-overlay", id = "modal-financial",
        onclick = "closeDimModal(event, 'financial')",
        div(class = "dim-modal dim-modal-financial",
            onclick = "event.stopPropagation()",
            div(class = "dim-modal-header",
                icon("university"),
                h4("Financial & Monetary Integration"),
                tags$button(class = "dim-modal-close", 
                            onclick = "closeDimModal(event, 'financial')", 
                            HTML("&times;"))
            ),
            div(class = "dim-modal-body",
                p("Assesses convergence in monetary conditions, banking sector integration, 
                  and cross-border financial flows within the GCC. Strong financial integration 
                  facilitates investment flows and supports the long-term goal of monetary union."),
                h5(icon("search"), "Indicators"),
                tags$ul(
                  tags$li("OCA Readiness"),
                  tags$li("Intra-GCC FDI Share"),
                  tags$li("Banking Sector Penetration"),
                  tags$li("Stock Market Integration"),
                  tags$li("Financial Depth Convergence")
                )
            )
        )
    ),
    
    # Labor Modal
    div(class = "dim-modal-overlay", id = "modal-labor",
        onclick = "closeDimModal(event, 'labor')",
        div(class = "dim-modal dim-modal-labor",
            onclick = "event.stopPropagation()",
            div(class = "dim-modal-header",
                icon("users"),
                h4("Labor & Human Capital"),
                tags$button(class = "dim-modal-close", 
                            onclick = "closeDimModal(event, 'labor')", 
                            HTML("&times;"))
            ),
            div(class = "dim-modal-body",
                p("Captures the mobility of labor and people across GCC borders, including 
                  worker movements, tourism, and student exchanges. The GCC common market aims to 
                  enable free movement of GCC nationals for work and residence across member states."),
                h5(icon("search"), "Indicators"),
                tags$ul(
                  tags$li("GCC National Worker Mobility"),
                  tags$li("Intra-GCC Tourism Intensity"),
                  tags$li("Student Exchange Mobility"),
                  tags$li("Labor Force Participation Convergence"),
                  tags$li("Unemployment Rate Convergence")
                )
            )
        )
    ),
    
    # Infrastructure Modal
    div(class = "dim-modal-overlay", id = "modal-infrastructure",
        onclick = "closeDimModal(event, 'infrastructure')",
        div(class = "dim-modal dim-modal-infrastructure",
            onclick = "event.stopPropagation()",
            div(class = "dim-modal-header",
                icon("road"),
                h4("Infrastructure Connectivity"),
                tags$button(class = "dim-modal-close", 
                            onclick = "closeDimModal(event, 'infrastructure')", 
                            HTML("&times;"))
            ),
            div(class = "dim-modal-body",
                p("Tracks physical and digital connectivity infrastructure that enables 
                  cross-border economic activity. This includes transport networks, energy 
                  interconnection through the GCC Grid, and telecommunications infrastructure."),
                h5(icon("search"), "Indicators"),
                tags$ul(
                  tags$li("Aviation Connectivity"),
                  tags$li("Electricity Production Convergence")
                )
            )
        )
    ),
    
    # Sustainability Modal
    div(class = "dim-modal-overlay", id = "modal-sustainability",
        onclick = "closeDimModal(event, 'sustainability')",
        div(class = "dim-modal dim-modal-sustainability",
            onclick = "event.stopPropagation()",
            div(class = "dim-modal-header",
                icon("leaf"),
                h4("Sustainability & Diversification"),
                tags$button(class = "dim-modal-close", 
                            onclick = "closeDimModal(event, 'sustainability')", 
                            HTML("&times;"))
            ),
            div(class = "dim-modal-body",
                p("Measures progress toward economic diversification away from hydrocarbon 
                  dependence, aligned with national Vision 2030 strategies. Includes tracking 
                  of renewable energy adoption and sustainable finance development."),
                h5(icon("search"), "Indicators"),
                tags$ul(
                  tags$li("Non-oil GDP Share"),
                  tags$li("Non-oil GDP Convergence"),
                  tags$li("Manufacturing Share Convergence")
                )
            )
        )
    ),
    
    # Convergence Modal
    div(class = "dim-modal-overlay", id = "modal-convergence",
        onclick = "closeDimModal(event, 'convergence')",
        div(class = "dim-modal dim-modal-convergence",
            onclick = "event.stopPropagation()",
            div(class = "dim-modal-header",
                icon("chart-line"),
                h4("Macroeconomic Convergence"),
                tags$button(class = "dim-modal-close", 
                            onclick = "closeDimModal(event, 'convergence')", 
                            HTML("&times;"))
            ),
            div(class = "dim-modal-body",
                p("Assesses the degree to which GCC economies are converging in key macroeconomic 
                  variables. Convergence in growth rates, inflation, and fiscal positions is a 
                  prerequisite for deeper monetary integration and potential currency union."),
                h5(icon("search"), "Indicators"),
                tags$ul(
                  tags$li("Real Income Convergence"),
                  tags$li("Price Level Convergence"),
                  tags$li("Financial Depth Convergence"),
                  tags$li("LFPR Convergence"),
                  tags$li("Unemployment Convergence"),
                  tags$li("Non-oil GDP Convergence"),
                  tags$li("Manufacturing Convergence"),
                  tags$li("Oil Dependency Convergence")
                )
            )
        )
    )
  )
}
#' Landing Page Footer Section
#'
#' @return Shiny UI element
landing_footer_section <- function() {
  div(class = "landing-footer",
      p(
        "\uA9 2025 GCC Statistical Center (GCC-Stat) | ",
        tags$a(href = "https://gccstat.org", target = "_blank", "www.gccstat.org")
      )
  )
}

#' Carousel JavaScript
#'
#' @return Shiny tags$script element
carousel_js <- function() {
  tags$script(HTML("
    var quotes = [
      'Economic integration is a strategic priority for the GCC. All member states have a role in building a unified, diversified regional economy.',
      'The GCC common market represents one of the most ambitious regional integration projects, supporting Vision 2030 transformation goals across member states.',
      'Measuring integration helps identify progress and opportunities for deeper economic cooperation, guiding evidence-based policymaking.',
      'From trade and investment to labor mobility and infrastructure connectivity, the Index captures the full picture of regional integration.'
    ];

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
        currentQuote = (currentQuote + 1) % quotes.length;
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
  "))
}
