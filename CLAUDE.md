# CLAUDE.md — GCCEII Dashboard

> Auto-loaded by Claude Code at session start. Keep this file current — it is
> the primary context file for this repository. Sections marked **Invariants**
> are binding; do not change them without a deliberate decision.

---

## Project Overview

**Repository:** `GCCEII`
**Full name:** GCC Economic Integration Index Dashboard
**Organisation:** GCC-Stat, Economic Statistics Department
**Status:** Production dashboard under active revision (Phase 1 complete, Phases 2–5 in planning)
**Default language:** Arabic (`reactiveVal("ar")` in app.R)
**Sister project:** `GCCEDI` — diversification dashboard sharing the same design system
**Pipeline:** COINr R package that produces `output/gcceii_scores.csv` (not in this repo)

The dashboard is a standalone Shiny app. It reads pre-computed pipeline output and
does **not** run the COINr pipeline. All indicator calculation, normalisation, and
aggregation happen upstream; this repo only visualises the results.

---

## Architecture

```
GCCEII/
├── app.R                        # Main Shiny app — UI + server (~1,700 lines)
├── R/
│   ├── utils.R                  # Constants: DIMENSION_COLS, DIMENSION_WEIGHTS, COUNTRY_COLORS
│   ├── translations.R           # Bilingual dictionary — t() lookup + translate_*() helpers
│   ├── data_loading.R           # load_gcc_data() — reads CSV, reshapes into 5 data objects
│   ├── mod_landing_page.R       # Landing page hero, carousel, dimension modals
│   ├── mod_charts.R             # All plotly chart functions (hardcoded axes — Phase 3 target)
│   ├── mod_metadata.R           # Methodology & Metadata tab content
│   │
│   ├── theme.R                  # @portable Phase 1 — token registry + CSS generator
│   ├── axis_helpers.R           # @portable Phase 1 — autoscale_range(), format_delta()
│   ├── chart_defaults.R         # @portable Phase 1 — apply_gcc_theme() for plotly
│   └── mod_kpi_card.R           # @portable Phase 1 — reusable KPI card Shiny module
│
├── www/
│   ├── styles.css               # Existing app styles (Phase 3: migrate to tokens)
│   ├── rtl.css                  # Arabic RTL overrides (existing)
│   ├── design-tokens.css        # @portable Phase 1 — GENERATED from theme.R
│   ├── design-system.css        # @portable Phase 1 — component CSS
│   ├── fonts.css                # @portable Phase 1 — Inter load + type-scale classes
│   ├── theme.js                 # @portable Phase 1 — custom Shiny message handlers
│   └── images/                  # Hero images, GCC-Stat logo
│
├── output/                      # Not tracked in git — pipeline output lives here
│   └── gcceii_scores.csv        # Long-format, ~2,800 rows; produced by COINr pipeline
│
├── DESIGN_SYSTEM.md             # Design system documentation (Phase 1)
├── PORTABLE_MODULES.md          # Tracks which files are safe to copy to GCCEDI
├── PLAN_executive_summary_narrative.md  # Detailed Phase 3 chart specs
├── smoke_test_phase1.R          # Standalone smoke-test for Phase 1 modules
├── hfce.csv                     # Supplementary data (HFCE by country)
├── manifest.json                # shinyapps.io deployment manifest
└── CLAUDE.md                    # This file
```

---

## Data Flow

```
COINr pipeline (external)
  └── output/gcceii_scores.csv    (long-format, ~2,800 rows)
        └── load_gcc_data("output")  →  5 named objects consumed by app.R:
              ├── dimension_scores   wide; one row per country × year
              ├── gcc_ts             GCC aggregate time series (overall + 6 dim scores)
              ├── yoy_changes        year-on-year deltas per country × dimension
              ├── country_data       latest-year slice; used for country profiles
              └── indicator_detail   indicator-level normalised scores (may be NULL)
```

### Key column names in `gcc_ts` (GCC aggregate)

| Column | Content |
|--------|---------|
| `year` | Integer 2015–2024 |
| `overall` | GCC aggregate score [0, 100] |
| `trade_score` | Trade dimension score |
| `financial_score` | Financial dimension score |
| `labor_score` | Labor dimension score |
| `infrastructure_score` | Infrastructure dimension score |
| `sustainability_score` | Sustainability dimension score |
| `convergence_score` | Convergence dimension score |

### Key column names in `dimension_scores` (country × year)

| Column | Content |
|--------|---------|
| `country` | Full name (`UAE`, `Bahrain`, …) |
| `year` | Integer 2015–2024 |
| `overall_index` | Country composite score [0, 100] |
| `trade_score` … `convergence_score` | Six dimension scores |
| `integration_level` | `"Weak"` / `"Moderate"` / `"Good"` |
| `rank` | Within-year rank (1 = highest) |

---

## Index Structure — Invariants

```
GCC Economic Integration Index (GCCEII)
├── Trade Integration          (≈17%)  — intra-GCC trade, services trade, …
├── Financial Integration      (≈17%)  — intra-GCC FDI, banking depth, …
├── Labor Integration          (≈17%)  — GCC labor mobility, student mobility, …
├── Infrastructure Integration (≈17%)  — transport, energy, telecom connectivity, …
├── Sustainability             (≈17%)  — green convergence indicators, …
└── Convergence                (≈17%)  — macroeconomic convergence, non-oil GDP, …
```

Scores: **[0, 100]** scale. 32 indicators total. 6 member states.
Countries: **Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, UAE**.
Time series: **2015–2024**.

---

## Constants (utils.R) — Invariants

Do not change these without updating the upstream COINr pipeline to match.

```r
DIMENSION_COLS    # c("trade_score","financial_score","labor_score",
                  #   "infrastructure_score","sustainability_score","convergence_score")
DIMENSION_LABELS  # c("Trade","Financial","Labor","Infrastructure","Sustainability","Convergence")
DIMENSION_WEIGHTS # named numeric — each dimension's weight in the composite
DIMENSION_ICONS   # fontawesome icon names for each dimension
COUNTRY_COLORS    # flag-based hex values (dual-keyed by full name)
```

---

## Design System (Phase 1) — Conventions

**Source of truth:** `R/theme.R` → `gcc_theme` list.

Every new chart must end with `apply_gcc_theme(p, lang = input$lang)`.
Every new axis must use `autoscale_plotly_y()` — never hardcode `y_range = c(0,100)`.
Every new delta display must use `format_delta()` — never `paste0("+", round(x,1))`.

```r
# Minimal correct chart pattern
plot_ly(...) |>
  autoscale_plotly_y(values, pad = 0.08) |>
  apply_gcc_theme(lang = input$lang) |>
  add_source_annotation("Source: COINr / GCC-Stat")
```

### Token naming convention

R side uses `gcc_theme$brand$primary`, `gcc_theme$semantic$positive_soft`, etc.
CSS side uses `var(--gcc-brand-primary)`, `var(--gcc-semantic-positive_soft)` (underscore, not dash).
Utility classes: `.gcc-text-positive`, `.gcc-bg-positive-soft`, `.gcc-font-latin`, `.gcc-level-badge.good`.

Regenerate CSS whenever `R/theme.R` changes:
```r
source("R/theme.R")
write_design_tokens_css()
# then commit www/design-tokens.css
```

### Portable modules

Files marked `@portable: yes` can be copied verbatim into GCCEDI.
Full list: `PORTABLE_MODULES.md`.

---

## Bilingual System

All user-visible strings go through `t(key, lang)` in `R/translations.R`.

```r
t("dim_trade", "en")  # "Trade"
t("dim_trade", "ar")  # "التجارة"
```

- Default language: **Arabic** (`current_lang <- reactiveVal("ar")`)
- Toggle: `input$lang_toggle` → flips between `"en"` and `"ar"`
- RTL layout: `body.lang-ar` class toggled via `session$sendCustomMessage("setLang", lang)`
- `rtl.css` provides RTL overrides keyed on `.lang-ar`
- Arabic font stack: Cairo → Tajawal → Noto Sans Arabic (loaded in `rtl.css` + `fonts.css`)
- New plotly charts: pass `lang = input$lang` to `apply_gcc_theme()` — it switches the font stack

Helpers available in `translations.R`:
- `t(key, lang)` — single string lookup
- `translate_country(name, lang)` / `translate_countries(vec, lang)` — country names
- `translate_dimension(name, lang)` / `translate_dimensions(vec, lang)` — dimension names
- `translate_indicator(name, lang)` / `translate_indicators(vec, lang)` — indicator labels
- `translate_level(level, lang)` — Weak / Moderate / Good
- `translate_colnames(df, lang)` — renames a data frame's columns for display

---

## Dashboard Tabs (current)

| Tab | Key outputs |
|-----|-------------|
| Landing page | Hero carousel, dimension circles, executive summary modal |
| GCC Overall | Gauge, dimension bar chart, country ranking, info boxes |
| GCC Timeseries | Dimension selector → indicator line charts, small multiples |
| Country Profiles | Radar, value boxes, lollipop chart, trend vs GCC |
| Year-on-Year | Heatmap, biggest movers chart |
| Data Explorer | Searchable DT, download button |
| Methodology | Hierarchy diagram, weight table, per-dimension indicator DTs |

**Planned restructure (Phase 2):** five tabs — Executive Pulse, Country Profiles, Dimension Deep-Dive, Data Lab, Methodology.

---

## Dashboard Revision Plan

### Phase 1 — Design system and technical foundation ✅ COMPLETE (2026-04-23)

Delivered on branch `feature/phase1-design-system`:

- [x] GCC-Stat aligned palette with WCAG-AA contrast (`R/theme.R`)
- [x] Typography scale — Inter (Latin) + Cairo/Tajawal (Arabic) (`www/fonts.css`)
- [x] Unified chart library: **plotly** (no new ggplot static outputs)
- [x] `autoscale_range()` / `autoscale_plotly_y()` helper — 8% pad, no forced 0 (`R/axis_helpers.R`)
- [x] `format_delta()` — says "no change" instead of "+0.0" (`R/axis_helpers.R`)
- [x] `apply_gcc_theme()` — stamps font, grid, hover, legend onto every plotly chart (`R/chart_defaults.R`)
- [x] `kpi_card_ui()` / `kpi_card_server()` — reusable KPI card module with sparkline and delta pill (`R/mod_kpi_card.R`)
- [x] CSS token pipeline: `write_design_tokens_css()` generates `www/design-tokens.css` from R side
- [x] Bilingual scaffolding: existing `translations.R` + `rtl.css` preserved; `theme.js` adds portable lang-class handler
- [x] Smoke test: `smoke_test_phase1.R`
- [x] Portability docs: `DESIGN_SYSTEM.md`, `PORTABLE_MODULES.md`

**Known deferred to Phase 3:** `mod_charts.R` still uses hardcoded `y_range = c(0,100)` — will be migrated when charts are rebuilt. `styles.css` not yet consuming tokens.

---

### Phase 2 — Information architecture and navigation 🔲 NOT STARTED (~1–2 weeks)

Goals:
- [ ] Restructure into five tabs: Executive Pulse, Country Profiles, Dimension Deep-Dive, Data Lab, Methodology
- [ ] Persistent year selector and country selector that survive tab switches
- [ ] One-line narrative subtitle under each tab header answering "what question does this tab answer?"
- [ ] Retire duplicated charts (overall-score-trend currently appears in 3+ places)
- [ ] Write tab-level narrative questions as design specs before implementation

---

### Phase 3 — Visualisation overhaul 🔲 NOT STARTED (~3–4 weeks)

Goals:
- [ ] Rebuild Executive Pulse headline components (gauge → hero KPI row)
- [ ] Gap-to-average radar chart
- [ ] Ranked dot plot replacing traffic-light bars
- [ ] Annotated dimension trajectory charts
- [ ] Migrate all `mod_charts.R` axes to `autoscale_plotly_y()` — remove every `y_range = c(0,100)`
- [ ] Migrate `styles.css` component selectors to consume `design-tokens.css` vars
- [ ] Rich tooltips everywhere: indicator name, latest value, YoY change, source, weight
- [ ] "Download data" and "Download image" affordance on every chart

Reference: `PLAN_executive_summary_narrative.md` has detailed chart specs.

---

### Phase 4 — Interactivity and narrative layer 🔲 NOT STARTED (~2–3 weeks)

Goals:
- [ ] Cross-filtering: click a country in the heatmap → other panels update
- [ ] Auto-generated insight text per tab (rule-based templates over data)
- [ ] Annotated timeline events on trend charts
- [ ] One-click country-profile export (PDF or Word brief)

---

### Phase 5 — Bilingual polish, accessibility, and launch 🔲 NOT STARTED (~1–2 weeks)

Goals:
- [ ] Full Arabic review with native speaker
- [ ] RTL layout verification: axis tick order, legend alignment, number formatting
- [ ] Keyboard navigation and screen reader testing
- [ ] `bindCache()` on expensive reactives
- [ ] Bookmarkable state (shareable URLs with country + tab + year)
- [ ] Basic usage analytics on deployment
- [ ] Deploy to production server

---

## Deployment

Target: internal RShiny Server (GCC-Stat).

```r
# Regenerate manifest before deploying:
rsconnect::writeManifest()

# Deploy:
rsconnect::deployApp(appDir = ".", appName = "GCCEII")
```

**Pre-deployment checklist:**
- [ ] `output/gcceii_scores.csv` present (not tracked in git — must be copied from pipeline)
- [ ] Run `source("R/theme.R"); write_design_tokens_css()` if theme was edited
- [ ] Test EN/AR toggle locally
- [ ] Smoke test passes: `app <- shiny::shinyAppFile("smoke_test_phase1.R"); shiny::runApp(app)`

---

## Branch Strategy

| Branch | Purpose |
|--------|---------|
| `main` | Production-stable code |
| `feature/phase1-design-system` | Phase 1 complete — awaiting merge to main |
| `claude/review-phase1-design-BuISl` | Claude Code working branch |

Claude Code development branches are prefixed `claude/`. Feature branches are prefixed `feature/`.

---

## Task Routing

- **Claude Code (this session)** — code edits, file creation, GitHub commits, R and CSS implementation
- **Phase planning** — use the phase checklist above; update status after each merge
- **GCCEDI portability** — when copying Phase 1 modules to GCCEDI, follow `PORTABLE_MODULES.md` step-by-step

---

*Last updated: 2026-04-23 — Phase 1 complete*
