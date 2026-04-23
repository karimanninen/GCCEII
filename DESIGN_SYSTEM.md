# GCC Dashboard Design System

_Phase 1 — Foundation, 2026-04-23_

This document describes the shared design system used by the GCC Economic
Integration Index (GCCEII) dashboard and intended for reuse in the sister
GCC Economic Diversification Index (GCCEDI) dashboard.

---

## Why this exists

Before Phase 1, visual tokens (colors, fonts, spacing, shadows) were
scattered across `www/styles.css` (inline hex values), `R/utils.R` (named
palette constants), and individual chart helper arguments (`y_range =
c(0, 100)` repeated everywhere). The same dimension appeared in two
different palettes depending on which file you read. When Kari asked to
make the dashboard more "modern, relevant, interactive and visually
impactful", the prerequisite was a single source of truth that code on
both the R side and the CSS side can read from.

This system is deliberately lightweight — it is a set of tokens and a
handful of helper functions, not a framework. It imposes consistency
without taking control away from the chart code.

---

## Structure

```
GCCEII/
├── R/
│   ├── theme.R              # @portable  — token registry + CSS generator
│   ├── axis_helpers.R       # @portable  — autoscale_range(), format_delta()
│   ├── chart_defaults.R     # @portable  — apply_gcc_theme() for plotly
│   └── mod_kpi_card.R       # @portable  — reusable KPI card module
├── www/
│   ├── fonts.css            # @portable  — Inter load, type scale classes
│   ├── design-tokens.css    # @portable  — GENERATED from theme.R
│   ├── design-system.css    # @portable  — component CSS (KPI card, etc.)
│   └── theme.js             # @portable  — custom Shiny message handlers
├── PORTABLE_MODULES.md      # tracking file: what can move to GCCEDI
└── DESIGN_SYSTEM.md         # this file
```

Portable files are marked with `@portable: yes` in their header so a
grep finds every file safe to copy into GCCEDI.

---

## How tokens flow

```
   R/theme.R (source of truth)
        │
        ├──────────  R code reads gcc_theme$brand$primary  ──► plotly, modules
        │
        └──► write_design_tokens_css()  ──► www/design-tokens.css
                                                    │
                                                    └──► CSS uses var(--gcc-brand-primary)
```

Because both sides are generated from the same R list, they cannot drift.
Whenever `R/theme.R` changes, run

```r
source("R/theme.R")
write_design_tokens_css()
```

to regenerate the CSS. The output is committed to the repo so production
Shiny does not need to regenerate at startup.

---

## Token reference (summary)

**Brand:** `primary`, `primary_light`, `primary_dark`, `accent`,
`accent_light`, `accent_dark`.
Deep navy `#003366` + GCC green `#008035`. Preserved from the existing
identity so the new system does not break visual continuity.

**Surface:** `background`, `canvas`, `subtle`, `border`, `border_strong`.
Very light so charts carry the color.

**Ink (text):** `primary`, `secondary`, `muted`, `inverse`, `brand`.
WCAG AA contrast at body sizes.

**Semantic:** `positive` / `negative` / `neutral` (score direction) plus
`success` / `warning` / `danger` / `info` (generic UI state). Each
positive/negative/neutral has a `*_soft` background variant for delta
pills and highlight fills.

**Level:** `weak`, `moderate`, `good` — muted versions of the existing
"Weak / Moderate / Good" badge colors. The bright red / yellow / green in
the old stylesheet was replaced because ministerial audiences read
saturated reds as alarms, which misrepresents a "Weak" classification as
a crisis signal.

**Dimension:** eight-color base palette. Assign at runtime via
`gcc_dimension_palette(c("Trade", "Labor", ...))`. The first six match
the existing GCCEII CSS palette so returning users see visual continuity.
Slots 7–8 are reserved for GCCEDI (which may have different dimensions).

**Country:** flag-based colors. Oman darkened from `#a3a3a3` to `#7a7a7a`
for AA contrast against white backgrounds.

**Typography:** Inter (Latin) paired with Cairo / Tajawal (Arabic), both
humanist sans-serifs with compatible x-heights. Type scale uses a ~1.25
ratio from 11px (micro) to 48px (display). Tabular figures enabled on
all KPI numerals via `font-variant-numeric: tabular-nums`.

**Spacing / radius / shadow:** 4-px base spacing scale, matching radius
scale, four shadow depths. Shadows are softer than the existing CSS
drops because ministerial UIs should feel calm, not floaty.

**Chart defaults:** margins, gridline color, axis color, hover styling.
Applied via `apply_gcc_theme(p)` at the end of every plotly pipeline.

For exact hex values see `R/theme.R` sections 1.1–1.13.

---

## Axis auto-scaling

**Problem the old code had:** every chart helper took `y_range = c(0, 100)`
as a default. The GCC integration score moved from 47.4 in 2023 to 47.4
in 2024 — on a 0-100 axis this change is invisible and the headline card
shows "▲0" which confuses ministerial viewers ("the dashboard says it
went up by zero?").

**Fix:** `autoscale_range(values, pad = 0.08)` from `R/axis_helpers.R`.
Pads the observed range by a configurable fraction, with optional
`include` set (to force the GCC average reference line inside the range)
and optional `anchor_zero` (for non-negative quantities like proportions).

Three wrappers around this core function cover typical cases:

- `autoscale_plotly_y(p, values)` — standard score axis.
- `autoscale_plotly_y_symmetric(p, values)` — for delta / change axes,
  keeps the axis symmetric around zero so a `+2` and a `-2` look equal
  in magnitude.
- `format_delta(delta)` — returns `"+2.3 pts"`, `"-0.8 pts"`, or
  `"no change"`. Critically, it says `"no change"` rather than `"+0.0"`
  when the delta rounds to zero at the requested precision — fixing
  the exact bug visible on the current Executive Pulse tab.

---

## Using a KPI card

```r
# --- UI ---------------------------------------------------------------
kpi_card_ui("trade_card")

# --- Server -----------------------------------------------------------
kpi_card_server(
  id      = "trade_card",
  label   = reactive(t("dim_trade", lang())),
  value   = reactive(latest$trade_score),
  delta   = reactive(latest$trade_score - prev$trade_score),
  history = reactive(gcc_ts[, c("year", "trade_score")]),
  accent  = gcceii_dimension_palette()[["Trade"]],
  unit    = "/100",
  digits  = 1,
  lang    = lang
)
```

The card handles its own sparkline (auto-scaled), delta pill (colored
by sign, with soft background), and accent stripe (colored by dimension).
Arabic rendering is automatic when `lang == "ar"`.

---

## Using auto-scaled plotly charts

```r
library(plotly)

p <- plot_ly(df, x = ~year, y = ~overall, type = "scatter", mode = "lines+markers",
             line = list(color = gcc_theme$brand$primary, width = 3)) |>
  autoscale_plotly_y(df$overall, pad = 0.08) |>
  apply_gcc_theme(lang = input$lang) |>
  add_source_annotation("Source: COINr / GCC-Stat")
```

Every chart should end with `apply_gcc_theme()` so fonts, gridlines,
hover styling, and legend placement stay consistent.

---

## Bilingual rendering

Phase 1 does not change the existing bilingual plumbing — `translations.R`
and `rtl.css` remain the source of truth for strings and RTL layout.
What this phase adds:

1. `fonts.css` sets Inter as the default Latin font, with the Arabic
   stack kicking in automatically when `body.lang-ar` is present (set by
   the existing shinyjs-based toggle).
2. `apply_gcc_theme(p, lang = "ar")` substitutes the Arabic font stack
   into plotly charts so axis ticks and legend labels render correctly
   in RTL runs.
3. Cairo and Tajawal continue to load from `rtl.css` (existing `@import`),
   so nothing breaks in current deployments.

No translation work required in Phase 1; translation updates come in
Phase 5.

---

## Extending for GCCEDI

Because GCCEDI has a different set of dimensions, the dimension palette
is indexed by name at runtime rather than being hard-wired. To set up
GCCEDI:

```r
# In GCCEDI's setup:
source("R/theme.R")

gccedi_dims <- c("Economic Complexity", "Export Basket", ...)  # whatever they are
edi_palette <- gcc_dimension_palette(gccedi_dims)
# edi_palette is now a named character vector; pass it to chart code.
```

Base palette has 8 slots (6 used by GCCEII, 2 reserved). If GCCEDI ends
up needing more, extend `gcc_theme$dimension_base` in theme.R and
re-run `write_design_tokens_css()`.

Files safe to copy into GCCEDI verbatim: see `PORTABLE_MODULES.md`.

---

## What Phase 1 deliberately does NOT do

- **Does not touch existing `styles.css`** — it continues to work. Phase
  3 will migrate the existing modules to consume tokens; Phase 1 is
  additive.
- **Does not touch `mod_charts.R`** — the hardcoded `y_range = c(0, 100)`
  pattern still exists there. Phase 3 swaps those calls for
  `autoscale_plotly_y()`.
- **Does not restructure tabs** — that is Phase 2.
- **Does not add scenario sliders, shift-share analyses, or event
  annotations** — those are Phase 4 / 5.
- **Does not introduce a new chart library** — plotly stays.
