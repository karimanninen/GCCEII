# =============================================================================
# build_reports.R
# Knit all four bilingual GCCEII reports to HTML.
# Output filenames match the expectations in R/mod_landing_page.R
# (exec_summary_modal function).
# =============================================================================
# Usage: from the GCCEII project root (where app.R and output/ live):
#   source("reports/build_reports.R")
# =============================================================================

library(rmarkdown)

# ── Ensure we're running from the project root ──────────────────────────────
if (basename(getwd()) == "reports") setwd("..")

# Capture project root as absolute path — knit_root_dir needs this so that
# relative paths like "output/gcceii_scores.csv" resolve correctly regardless
# of where the Rmd file lives.
project_root <- normalizePath(getwd())

out_dir <- file.path(project_root, "www", "reports")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── Check Arabic font packages (used by ggplot for AR reports) ──────────────
for (pkg in c("showtext", "sysfonts")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, " for Arabic chart rendering ...")
    install.packages(pkg)
  }
}

# ── Report list ─────────────────────────────────────────────────────────────
# Output filenames chosen to match the hardcoded references in
# R/mod_landing_page.R → exec_summary_modal()
reports <- list(
  list(rmd = "reports/GCC_EII_Narrative_Report.Rmd",
       out = "GCC_EII_Narrative_Report.html"),
  list(rmd = "reports/GCC_EII_Narrative_Report_AR.Rmd",
       out = "GCC_EII_Narrative_Report_AR.html"),
  list(rmd = "reports/GCC_EII_Executive_Summary.Rmd",
       out = "GCC_EII_Executive_Summary.html"),
  list(rmd = "reports/GCC_EII_Executive_Summary_AR.Rmd",
       out = "GCC_EII_Executive_Summary_AR.html")
)

# ── Render each report to HTML only ─────────────────────────────────────────
for (r in reports) {
  if (!file.exists(r$rmd)) {
    warning("Skipping missing file: ", r$rmd)
    next
  }

  message("\n▶ Rendering ", r$rmd, " ...")

  tryCatch({
    render(r$rmd,
           output_format = "html_document",
           output_file   = r$out,
           output_dir    = out_dir,
           knit_root_dir = project_root,
           quiet         = TRUE,
           envir         = new.env())
    message("  ✓ ", out_dir, "/", r$out)
  }, error = function(e) {
    message("  ✗ Failed: ", e$message)
  })
}

message("\n✓ Done. Outputs in: ", out_dir)
