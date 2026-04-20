# Implementation Plan: Executive Summary & EII Narrative on Landing Page

**Date:** 2026-04-20  
**Scope:** Landing page additions — bilingual (EN/AR)  
**Affected files:** `R/mod_landing_page.R`, `R/translations.R`, `www/styles.css`, `www/rtl.css`, `app.R`, `reports/` (new Rmd), `www/reports/` (new HTML)

---

## Overview

Two features are added to the landing page:

| Feature | Presentation | Trigger |
|---|---|---|
| **Executive Summary** | Pop-up scorecard modal (same overlay pattern as dimension modals) | "Executive Summary" button in the About section |
| **EII Narrative** | Static HTML pages served from `www/reports/`, opened in a new browser tab | "Read Full Report" buttons inside the Executive Summary modal (one per language) |

---

## Feature 1 — Executive Summary Modal

### Design rationale

The app already has a polished, well-tested modal pattern for the six dimension modals. Reusing the same overlay/slide-in architecture keeps the UX consistent and avoids adding a new JS library. The Executive Summary modal is wider (max-width 860px) and has richer internal layout to distinguish it from the simpler dimension modals.

### Visual layout of the modal

```
┌──────────────────────────────────────────────────────────────────┐
│  [GCC green gradient header]                              [×]    │
│  GCC Economic Integration Index          Score: 58.4  ↑ +1.2   │
│  Latest year: 2024                                               │
├──────────────────────────────────────────────────────────────────┤
│  KEY FINDINGS                                                    │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │ 🔹 Finding 1 text …                                      │    │
│  │ 🔹 Finding 2 text …                                      │    │
│  │ 🔹 Finding 3 text …                                      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  DIMENSION SCORES (2024)                                         │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐                        │
│  │  Trade   │ │Financial │ │  Labor   │                        │
│  │  62.1 ↑  │ │  55.8 →  │ │  48.3 ↑  │                        │
│  └──────────┘ └──────────┘ └──────────┘                        │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐                        │
│  │  Infra   │ │  Sustain │ │Converge  │                        │
│  │  71.2 ↑  │ │  44.6 ↓  │ │  60.0 →  │                        │
│  └──────────┘ └──────────┘ └──────────┘                        │
│                                                                  │
│  [ 📄 Read Full Report (English) ]  [ 📄 اقرأ التقرير (عربي) ]  │
└──────────────────────────────────────────────────────────────────┘
```

In Arabic mode the header score and findings text are right-to-left; the dimension grid card order is reversed; the "read report" buttons swap sides.

### Data source

The scores come from `output/gcceii_scores.csv` which is already loaded at app startup. The latest-year composite score and each dimension score for the GCC average (or un-weighted mean across countries) are extracted once in `app.R` and passed into `landing_page_ui()`.

**New function signature:**
```r
landing_page_ui <- function(lang = "en", exec_summary_data = NULL)
```
`exec_summary_data` is a named list:
```r
list(
  year        = 2024,
  composite   = 58.4,
  delta       = +1.2,        # change vs previous year
  dimensions  = c(
    Trade          = 62.1,
    Financial      = 55.8,
    Labor          = 48.3,
    Infrastructure = 71.2,
    Sustainability = 44.6,
    Convergence    = 60.0
  ),
  dim_deltas  = c(...)        # change per dimension vs previous year
)
```

If `exec_summary_data` is `NULL` (e.g., data hasn't loaded yet), the modal shows a spinner or graceful "data unavailable" message.

---

## Feature 2 — EII Narrative HTML Pages

### English

`www/reports/GCC_EII_Narrative_Report.html` already exists and is compiled from `reports/GCC_EII_Narrative_Report.Rmd`. No changes needed to the English file — just wire it up to a button.

### Arabic

A new file `reports/GCC_EII_Narrative_AR.Rmd` needs to be created and knitted to `www/reports/GCC_EII_Narrative_AR.html`. The Arabic Rmd should:
- Set `lang: ar` and `dir: rtl` in the HTML output options
- Use the Cairo or Tajawal font (already loaded by the app's CSS via Google Fonts)
- Mirror the structure of the English narrative with Arabic text

The Arabic narrative can start as a placeholder stub and be filled in later — having the file wired up is the first priority.

### Opening mechanism

Both narrative pages open in a **new browser tab** via `window.open()`. This is simpler and more reliable than an iframe inside the modal (which would require CORS-safe serving and height management). Shiny automatically serves everything under `www/` as static files at the app root URL, so `window.open('reports/GCC_EII_Narrative_Report.html', '_blank')` just works.

---

## Implementation Steps

### Step 1 — `R/translations.R`: Add new keys

Add a new block after the existing landing-page keys:

```r
# ---------------------------------------------------------------------------
# Executive Summary modal
# ---------------------------------------------------------------------------
exec_summary_btn = list(
  en = "Executive Summary",
  ar = "\u0627\u0644\u0645\u0644\u062e\u0635 \u0627\u0644\u062a\u0646\u0641\u064a\u0630\u064a"
),
exec_summary_title = list(
  en = "GCC Economic Integration Index",
  ar = "\u0645\u0624\u0634\u0631 \u0627\u0644\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0627\u0642\u062a\u0635\u0627\u062f\u064a \u0644\u062f\u0648\u0644 \u0645\u062c\u0644\u0633 \u0627\u0644\u062a\u0639\u0627\u0648\u0646"
),
exec_summary_score_label = list(
  en = "Composite Score",
  ar = "\u0627\u0644\u0645\u0624\u0634\u0631 \u0627\u0644\u0645\u0631\u0643\u0651\u0628"
),
exec_summary_year_label = list(
  en = "Latest year",
  ar = "\u0622\u062e\u0631 \u0633\u0646\u0629"
),
exec_summary_findings_heading = list(
  en = "Key Findings",
  ar = "\u0623\u0628\u0631\u0632 \u0627\u0644\u0646\u062a\u0627\u0626\u062c"
),
exec_summary_findings = list(
  en = "Finding 1\nFinding 2\nFinding 3",   # newline-separated, like indicator lists
  ar = "\u0646\u062a\u064a\u062c\u0629 1\n\u0646\u062a\u064a\u062c\u0629 2\n\u0646\u062a\u064a\u062c\u0629 3"
),
exec_summary_dim_heading = list(
  en = "Dimension Scores",
  ar = "\u062f\u0631\u062c\u0627\u062a \u0627\u0644\u0623\u0628\u0639\u0627\u062f"
),
exec_summary_read_en = list(
  en = "Read Full Report (English)",
  ar = "\u0627\u0642\u0631\u0623 \u0627\u0644\u062a\u0642\u0631\u064a\u0631 \u0643\u0627\u0645\u0644\u0627\u064b (\u0625\u0646\u062c\u0644\u064a\u0632\u064a)"
),
exec_summary_read_ar = list(
  en = "Read Full Report (Arabic)",
  ar = "\u0627\u0642\u0631\u0623 \u0627\u0644\u062a\u0642\u0631\u064a\u0631 \u0643\u0627\u0645\u0644\u0627\u064b (\u0639\u0631\u0628\u064a)"
)
```

---

### Step 2 — `R/mod_landing_page.R`: Three additions

#### 2a. Executive Summary button in `landing_about_section()`

After the closing `div` of `.stats-row`, add:

```r
# Executive Summary button
div(class = "exec-summary-btn-row",
    tags$button(
      class = "exec-summary-open-btn",
      onclick = "openExecModal()",
      icon("file-alt"),
      span(t("exec_summary_btn", lang))
    )
)
```

#### 2b. New `exec_summary_modal()` function

This function builds the modal HTML using `exec_summary_data`. It follows the exact same overlay-over-content pattern as `.dim_modal()`:

```r
exec_summary_modal <- function(lang = "en", data = NULL) {
  # Helper: format a score with trend arrow
  fmt_score <- function(val, delta) {
    arrow <- if (delta > 0.5) "\u2191" else if (delta < -0.5) "\u2193" else "\u2192"
    colour <- if (delta > 0.5) "#2e7d32" else if (delta < -0.5) "#c62828" else "#666"
    HTML(paste0(
      '<span class="dim-score-val">', sprintf("%.1f", val), '</span>',
      '<span class="dim-score-arrow" style="color:', colour, '">', arrow, '</span>'
    ))
  }

  dims   <- if (!is.null(data)) data$dimensions else NULL
  deltas <- if (!is.null(data)) data$dim_deltas  else rep(0, 6)

  dim_ids <- c("Trade","Financial","Labor","Infrastructure","Sustainability","Convergence")
  dim_icon_map <- c(Trade="exchange-alt", Financial="university", Labor="users",
                    Infrastructure="road", Sustainability="leaf", Convergence="chart-line")

  dim_cards <- lapply(dim_ids, function(d) {
    score <- if (!is.null(dims)) dims[[d]] else NA
    delta <- if (!is.null(deltas)) deltas[[d]] else 0
    div(class = paste("exec-dim-card", paste0("exec-dim-", tolower(d))),
        div(class = "exec-dim-icon", icon(dim_icon_map[[d]])),
        div(class = "exec-dim-name", t(paste0("dim_", tolower(d)), lang)),
        if (!is.na(score)) fmt_score(score, delta) else span(class="dim-score-na", "—")
    )
  })

  div(class = "dim-modal-overlay", id = "modal-exec-summary",
      onclick = "closeExecModal(event)",
      div(class = "dim-modal exec-summary-modal",
          onclick = "event.stopPropagation()",

          # Header
          div(class = "dim-modal-header exec-summary-header",
              div(class = "exec-summary-header-left",
                  icon("chart-bar"),
                  div(
                    h4(t("exec_summary_title", lang)),
                    span(class = "exec-year-label",
                         t("exec_summary_year_label", lang), ": ",
                         if (!is.null(data)) data$year else "—")
                  )
              ),
              div(class = "exec-composite-score",
                  div(class = "exec-score-label", t("exec_summary_score_label", lang)),
                  div(class = "exec-score-value",
                      if (!is.null(data)) sprintf("%.1f", data$composite) else "—"),
                  if (!is.null(data))
                    div(class = "exec-score-delta",
                        if (data$delta > 0) paste0("▲ +", sprintf("%.1f", data$delta))
                        else if (data$delta < 0) paste0("▼ ", sprintf("%.1f", data$delta))
                        else "→ 0.0")
              ),
              tags$button(class = "dim-modal-close",
                          onclick = "closeExecModal(event)", HTML("&times;"))
          ),

          # Body
          div(class = "dim-modal-body exec-summary-body",

              # Key Findings
              h5(icon("lightbulb"), t("exec_summary_findings_heading", lang)),
              div(class = "exec-findings-list",
                  lapply(
                    strsplit(t("exec_summary_findings", lang), "\n")[[1]],
                    function(f) div(class = "exec-finding", icon("angle-right"), span(f))
                  )
              ),

              # Dimension grid
              h5(icon("th"), t("exec_summary_dim_heading", lang)),
              div(class = "exec-dim-grid", dim_cards),

              # Report buttons
              div(class = "exec-report-btns",
                  tags$button(class = "exec-report-btn exec-report-btn-en",
                    onclick = "window.open('reports/GCC_EII_Narrative_Report.html','_blank')",
                    icon("file-alt"), span(t("exec_summary_read_en", lang))
                  ),
                  tags$button(class = "exec-report-btn exec-report-btn-ar",
                    onclick = "window.open('reports/GCC_EII_Narrative_AR.html','_blank')",
                    icon("file-alt"), span(t("exec_summary_read_ar", lang))
                  )
              )
          )
      )
  )
}
```

#### 2c. Wire modal into `landing_page_ui()` and extend JS

Add `exec_summary_modal(lang, exec_summary_data)` call to the `tagList` at the end of `landing_about_section()` (or directly in `landing_page_ui()` alongside `dimension_modals()`).

Extend `carousel_js()` with two extra JS functions:

```javascript
function openExecModal() {
  document.getElementById('modal-exec-summary').classList.add('active');
  document.body.style.overflow = 'hidden';
}

function closeExecModal(event) {
  if (event.target === event.currentTarget ||
      event.target.classList.contains('dim-modal-close')) {
    document.getElementById('modal-exec-summary').classList.remove('active');
    document.body.style.overflow = 'auto';
  }
}
```

The existing Escape-key listener already handles all `.dim-modal-overlay.active` elements, so it will close this modal too with no changes.

---

### Step 3 — `app.R`: Compute scores and pass to UI

In `app.R`, wherever `gcceii_scores` is loaded (probably near the top), add a reactive or pre-computed object:

```r
# Pre-compute executive summary data (GCC average across countries, latest year)
exec_summary_data <- reactive({
  req(gcceii_scores)
  latest <- max(gcceii_scores$year, na.rm = TRUE)
  prev   <- latest - 1

  score_yr <- function(yr)
    gcceii_scores |>
      filter(year == yr) |>
      summarise(
        composite      = mean(composite_score, na.rm = TRUE),
        Trade          = mean(Trade,           na.rm = TRUE),
        Financial      = mean(Financial,       na.rm = TRUE),
        Labor          = mean(Labor,           na.rm = TRUE),
        Infrastructure = mean(Infrastructure,  na.rm = TRUE),
        Sustainability = mean(Sustainability,  na.rm = TRUE),
        Convergence    = mean(Convergence,     na.rm = TRUE)
      )

  curr <- score_yr(latest)
  prev_s <- score_yr(prev)

  list(
    year       = latest,
    composite  = curr$composite,
    delta      = curr$composite - prev_s$composite,
    dimensions = c(Trade=curr$Trade, Financial=curr$Financial,
                   Labor=curr$Labor, Infrastructure=curr$Infrastructure,
                   Sustainability=curr$Sustainability, Convergence=curr$Convergence),
    dim_deltas = c(Trade          = curr$Trade          - prev_s$Trade,
                   Financial      = curr$Financial      - prev_s$Financial,
                   Labor          = curr$Labor          - prev_s$Labor,
                   Infrastructure = curr$Infrastructure - prev_s$Infrastructure,
                   Sustainability = curr$Sustainability - prev_s$Sustainability,
                   Convergence    = curr$Convergence    - prev_s$Convergence)
  )
})
```

Then update the `landing_page_ui()` call:

```r
landing_page_ui(lang = current_lang(), exec_summary_data = exec_summary_data())
```

> **Note:** The exact column names in `gcceii_scores.csv` need to be confirmed before coding — use `names(gcceii_scores)` to check.

---

### Step 4 — `www/styles.css`: New CSS classes

Add a new block at the end of `styles.css`:

```css
/* ===== EXECUTIVE SUMMARY MODAL ===== */

/* Button in about section */
.exec-summary-btn-row {
  text-align: center;
  margin-top: 28px;
}
.exec-summary-open-btn {
  background: linear-gradient(135deg, #008035, #006028);
  color: white;
  border: none;
  border-radius: 30px;
  padding: 12px 32px;
  font-size: 1rem;
  font-weight: 600;
  cursor: pointer;
  transition: transform 0.2s, box-shadow 0.2s;
  letter-spacing: 0.3px;
}
.exec-summary-open-btn:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(0,128,53,0.35);
}
.exec-summary-open-btn .fa { margin-right: 8px; }

/* Wider modal override */
.exec-summary-modal {
  max-width: 860px !important;
}

/* Header layout */
.exec-summary-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  background: linear-gradient(135deg, #008035, #006028) !important;
}
.exec-summary-header-left {
  display: flex;
  align-items: center;
  gap: 14px;
}
.exec-year-label { font-size: 0.85rem; opacity: 0.85; }

/* Composite score block (top-right of header) */
.exec-composite-score {
  text-align: center;
  background: rgba(255,255,255,0.15);
  border-radius: 10px;
  padding: 8px 20px;
  margin-right: 48px;   /* clear the × button */
}
.exec-score-label { font-size: 0.75rem; opacity: 0.85; }
.exec-score-value { font-size: 2rem; font-weight: 700; line-height: 1.1; }
.exec-score-delta { font-size: 0.8rem; margin-top: 2px; }

/* Key findings */
.exec-findings-list { margin-bottom: 24px; }
.exec-finding {
  display: flex;
  align-items: flex-start;
  gap: 10px;
  padding: 8px 12px;
  border-left: 3px solid #008035;
  background: #f0f7f2;
  border-radius: 4px;
  margin-bottom: 8px;
  font-size: 0.92rem;
  line-height: 1.5;
}
.exec-finding .fa { color: #008035; margin-top: 2px; flex-shrink: 0; }

/* Dimension cards grid */
.exec-dim-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 12px;
  margin-bottom: 24px;
}
.exec-dim-card {
  border-radius: 10px;
  padding: 14px 10px;
  text-align: center;
  color: white;
  position: relative;
}
.exec-dim-icon { font-size: 1.4rem; margin-bottom: 6px; opacity: 0.9; }
.exec-dim-name { font-size: 0.78rem; font-weight: 600; text-transform: uppercase;
                 letter-spacing: 0.5px; margin-bottom: 4px; }
.dim-score-val  { font-size: 1.5rem; font-weight: 700; }
.dim-score-arrow{ font-size: 1rem; margin-left: 4px; }
.dim-score-na   { font-size: 1.2rem; color: rgba(255,255,255,0.6); }

/* Dimension card colors (reuse existing dimension palette) */
.exec-dim-trade          { background: linear-gradient(135deg,#1e88e5,#1565c0); }
.exec-dim-financial      { background: linear-gradient(135deg,#f9a825,#f57f17); }
.exec-dim-labor          { background: linear-gradient(135deg,#43a047,#2e7d32); }
.exec-dim-infrastructure { background: linear-gradient(135deg,#e53935,#c62828); }
.exec-dim-sustainability { background: linear-gradient(135deg,#8e24aa,#6a1b9a); }
.exec-dim-convergence    { background: linear-gradient(135deg,#00acc1,#00838f); }

/* Report buttons */
.exec-report-btns {
  display: flex;
  gap: 12px;
  justify-content: center;
  padding-top: 8px;
  border-top: 1px solid #eee;
}
.exec-report-btn {
  border: none;
  border-radius: 6px;
  padding: 10px 20px;
  font-size: 0.9rem;
  font-weight: 600;
  cursor: pointer;
  transition: transform 0.15s, opacity 0.15s;
  display: flex;
  align-items: center;
  gap: 8px;
}
.exec-report-btn:hover { transform: translateY(-1px); opacity: 0.9; }
.exec-report-btn-en { background: #003366; color: white; }
.exec-report-btn-ar { background: #008035; color: white; }
```

---

### Step 5 — `www/rtl.css`: Arabic RTL overrides

Add at the end of `rtl.css` under the existing `body.lang-ar` selector block:

```css
/* ===== EXECUTIVE SUMMARY MODAL – RTL ===== */
body.lang-ar .exec-summary-open-btn .fa { margin-right: 0; margin-left: 8px; }
body.lang-ar .exec-summary-header-left  { flex-direction: row-reverse; }
body.lang-ar .exec-composite-score      { margin-right: 0; margin-left: 48px; }
body.lang-ar .exec-finding              { flex-direction: row-reverse;
                                          border-left: none;
                                          border-right: 3px solid #008035;
                                          text-align: right; }
body.lang-ar .dim-score-arrow           { margin-left: 0; margin-right: 4px; }
body.lang-ar .exec-report-btns          { flex-direction: row-reverse; }
body.lang-ar .exec-report-btn           { flex-direction: row-reverse; }
```

---

### Step 6 — `reports/GCC_EII_Narrative_AR.Rmd` (new file)

Create a minimal Arabic stub (to be fully authored later):

```yaml
---
title: "مؤشر التكامل الاقتصادي لدول مجلس التعاون الخليجي — التقرير السردي"
subtitle: "GCC-Stat | 2024"
date: "`r format(Sys.Date(), '%d %B %Y')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
    number_sections: false
    css: "../www/styles.css"
lang: ar
---
```

Set `lang: ar` so the browser renders RTL. Include a short stub section for each of the 6 dimensions with placeholder Arabic text. Knit and save output to `www/reports/GCC_EII_Narrative_AR.html`.

> The English narrative at `www/reports/GCC_EII_Narrative_Report.html` already exists; no changes needed.

---

## File change summary

| File | Change type | Notes |
|---|---|---|
| `R/translations.R` | Add ~12 new key/value pairs | Executive Summary modal labels |
| `R/mod_landing_page.R` | Add button, new modal function, extend JS | ~120 new lines |
| `www/styles.css` | Append new CSS block | ~80 new lines |
| `www/rtl.css` | Append RTL overrides | ~15 new lines |
| `app.R` | Add reactive, update `landing_page_ui()` call | ~25 new lines |
| `reports/GCC_EII_Narrative_AR.Rmd` | New file | Arabic narrative stub |
| `www/reports/GCC_EII_Narrative_AR.html` | New file (knitted) | Served as static asset |

---

## Open questions / decisions needed

1. **Score aggregation method:** The modal shows a single "GCC composite score". Should this be the mean across all 6 countries, or a population-weighted average? (The existing Overview tab likely has a canonical value — match it.)

2. **Key Findings text:** These will be authored manually as translation keys. Who drafts them? They can start as placeholder text and be updated in `translations.R` without any code change.

3. **Arabic narrative content:** The stub Rmd needs actual Arabic text. This can be translated from the English narrative Rmd. Should this be a full translation or a condensed version?

4. **"Executive Summary" vs "Narrative" distinction:** The existing `www/reports/GCC_EII_Executive_Summary.html` is a compiled Rmd. Should the modal "Read Full Report" buttons link to the Narrative report, the Executive Summary HTML, or both? Currently the plan links both buttons to their respective Narrative reports. Adjust if needed.

5. **Placement of the "Executive Summary" button:** Currently proposed in the About section below the stats row. An alternative is placing it in the hero section alongside the "Enter Dashboard" button. The About section is preferable because it sits below the fold and creates a natural flow: see the overview → click for more detail.
