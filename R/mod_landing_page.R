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
  dimensions <- list(
    list(icon = "exchange-alt", title = "Trade Integration",
         desc = "Intra-GCC merchandise and services trade intensity"),
    list(icon = "university", title = "Financial Integration",
         desc = "Cross-border financial flows and monetary convergence"),
    list(icon = "users", title = "Labor Mobility",
         desc = "Movement of workers, tourists, and students"),
    list(icon = "road", title = "Infrastructure Connectivity",
         desc = "Transport, energy, and digital infrastructure"),
    list(icon = "leaf", title = "Sustainability",
         desc = "Economic diversification and green transition"),
    list(icon = "chart-line", title = "Economic Convergence",
         desc = "Macroeconomic alignment across member states")
  )

  div(class = "dimensions-section",
      h3("Six Integration Dimensions"),
      div(class = "dimension-grid",
          lapply(dimensions, function(d) {
            div(class = "dimension-card",
                icon(d$icon),
                div(class = "dim-text",
                    div(class = "dim-title", d$title),
                    div(class = "dim-desc", d$desc)
                )
            )
          })
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
  "))
}
