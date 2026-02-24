# =============================================================================
# R/translations.R
# GCCEII Dashboard – Bilingual Translation Dictionary (English / Arabic)
# =============================================================================
# Usage:
#   source("R/translations.R")
#   t("app_title", lang)                     # → scalar string
#   translate_country("Saudi Arabia", lang)   # → translated country name
#   translate_dimension("Trade", lang)        # → translated dimension name
#   get_direction(lang)                       # → "ltr" | "rtl"
#   get_font(lang)                            # → CSS font-family string
# =============================================================================

# ── Core lookup function ──────────────────────────────────────────────────────

.translations <- list(

  # ---------------------------------------------------------------------------
  # App-level
  # ---------------------------------------------------------------------------
  app_title = list(
    en = "GCC Economic Integration Index",
    ar = "مؤشر التكامل الاقتصادي لدول مجلس التعاون الخليجي"
  ),
  app_subtitle = list(
    en = "Measuring Regional Integration Across the Gulf Cooperation Council",
    ar = "قياس التكامل الإقليمي عبر دول مجلس التعاون الخليجي"
  ),

  # ---------------------------------------------------------------------------
  # Language toggle
  # ---------------------------------------------------------------------------
  lang_toggle_en = list(en = "English", ar = "English"),
  lang_toggle_ar = list(en = "عربي",    ar = "عربي"),

  # ---------------------------------------------------------------------------
  # Sidebar menu items
  # ---------------------------------------------------------------------------
  menu_overview = list(
    en = "Overview",
    ar = "نظرة عامة"
  ),
  menu_gcc_overall = list(
    en = "GCC Overall",
    ar = "المستوى الإجمالي لدول المجلس"
  ),
  menu_gcc_timeseries = list(
    en = "GCC Timeseries",
    ar = "السلاسل الزمنية للمجلس"
  ),
  menu_country_profiles = list(
    en = "Country Profiles",
    ar = "ملفات الدول"
  ),
  menu_country_heatmap = list(
    en = "Country Heatmap",
    ar = "خريطة حرارية للدول"
  ),
  menu_gcc_analytics = list(
    en = "GCC Analytics",
    ar = "التحليلات الاقتصادية"
  ),
  menu_data_explorer = list(
    en = "Data Explorer",
    ar = "مستكشف البيانات"
  ),
  menu_metadata = list(
    en = "Metadata",
    ar = "البيانات الوصفية"
  ),

  # ---------------------------------------------------------------------------
  # Landing page
  # ---------------------------------------------------------------------------
  landing_title = list(
    en = "GCC Economic Integration Index",
    ar = "مؤشر التكامل الاقتصادي لدول مجلس التعاون الخليجي"
  ),
  landing_subtitle = list(
    en = "A comprehensive framework for measuring economic integration\nacross the six GCC member states (2015–2023)",
    ar = "إطار شامل لقياس التكامل الاقتصادي\nعبر الدول الأعضاء الست في مجلس التعاون الخليجي (2015–2023)"
  ),
  landing_enter_dashboard = list(
    en = "Enter Dashboard",
    ar = "الدخول إلى لوحة المعلومات"
  ),
  landing_about_title = list(
    en = "About the Index",
    ar = "عن المؤشر"
  ),
  landing_about_body = list(
    en = paste(
      "The GCC Economic Integration Index (GCCEII) is a composite indicator developed by GCC-Stat",
      "to monitor and evaluate the depth of economic integration among Bahrain, Kuwait, Oman,",
      "Qatar, Saudi Arabia, and the UAE. The index draws on internationally recognised methodologies",
      "(JRC/OECD) and covers six integration dimensions with annual data from 2015 to 2023."
    ),
    ar = paste(
      "مؤشر التكامل الاقتصادي لدول مجلس التعاون الخليجي (GCCEII) مؤشر مركّب طوّرته الأمانة العامة",
      "لمجلس التعاون (GCC-Stat) لرصد وتقييم عمق التكامل الاقتصادي بين البحرين والكويت وعُمان",
      "وقطر والمملكة العربية السعودية والإمارات العربية المتحدة. يستند المؤشر إلى منهجيات معترف بها دولياً",
      "(JRC/OECD) ويغطي ستة أبعاد للتكامل ببيانات سنوية من 2015 إلى 2023."
    )
  ),

  # Stat boxes on landing page
  stat_countries = list(en = "Member States",   ar = "الدول الأعضاء"),
  stat_dimensions = list(en = "Dimensions",      ar = "الأبعاد"),
  stat_years      = list(en = "Years of Data",   ar = "سنوات البيانات"),
  stat_indicators = list(en = "Indicators",      ar = "المؤشرات"),

  # Carousel / pull-quotes (4 quotes)
  # Arabic versions are written in idiomatic Gulf/official-register Arabic,
  # not literal translations — suitable for GCC Secretariat / Vision documents.
  quote_1 = list(
    en = "\"Regional integration is the cornerstone of sustainable development across the Gulf.\"",
    ar = "\"التكامل الإقليمي دعامة لا غنى عنها لمسيرة التنمية المستدامة في دول الخليج العربي.\""
  ),
  quote_2 = list(
    en = "\"Evidence-based policy requires robust, comparable, and timely statistics.\"",
    ar = "\"لا ترسيخ لسياسات رشيدة من غير إحصاءات دقيقة وموثوقة وقابلة للمقارنة.\""
  ),
  quote_3 = list(
    en = "\"The GCC vision of economic unity demands rigorous measurement of progress.\"",
    ar = "\"تحقيق الوحدة الاقتصادية الخليجية رهينٌ بقياس دقيق وصارم لمسارات التقدم.\""
  ),
  quote_4 = list(
    en = "\"Deeper integration unlocks the full potential of the Gulf economy.\"",
    ar = "\"كلما ترسّخ التكامل وتعمّق، اتّسعت آفاق الاقتصاد الخليجي وتضاعفت إمكاناته.\""
  ),

  # Dimension modal labels
  modal_learn_more = list(en = "Learn More", ar = "اعرف المزيد"),
  modal_indicators_label = list(en = "Key Indicators", ar = "المؤشرات الرئيسية"),
  modal_close = list(en = "Close", ar = "إغلاق"),

  # Footer
  footer_produced_by = list(
    en = "Produced by GCC-Stat | Statistical Centre for the Cooperation Council for the Arab States of the Gulf",
    ar = "من إنتاج الجهاز الإحصائي لمجلس التعاون لدول الخليج العربية (GCC-Stat)"
  ),
  footer_methodology = list(
    en = "Methodology follows JRC/OECD Handbook on Composite Indicators",
    ar = "المنهجية مستندة إلى دليل JRC/OECD للمؤشرات المركّبة"
  ),

  # ---------------------------------------------------------------------------
  # Overview tab
  # ---------------------------------------------------------------------------
  overview_box_dashboard = list(
    en = "GCC Economic Integration Index Dashboard",
    ar = "لوحة مؤشر التكامل الاقتصادي لدول مجلس التعاون الخليجي"
  ),
  overview_box_aggregate = list(
    en = "GCC Aggregate Integration Performance",
    ar = "الأداء الإجمالي للتكامل في دول المجلس"
  ),
  overview_box_dimension_scores = list(
    en = "Current Dimension Scores",
    ar = "درجات الأبعاد الحالية"
  ),
  overview_box_score_cards = list(
    en = "Dimension Score Cards",
    ar = "بطاقات درجات الأبعاد"
  ),

  # Value boxes
  vb_gcc_index   = list(en = "Latest GCC Overall Index", ar = "آخر مؤشر إجمالي للمجلس"),
  vb_latest_year = list(en = "Latest Year",              ar = "آخر سنة"),
  vb_members     = list(en = "GCC Member States",        ar = "الدول الأعضاء"),
  vb_overall_index    = list(en = "Overall Index (Latest)", ar = "المؤشر الإجمالي (أحدث)"),
  vb_country_rank     = list(en = "Country Rank",           ar = "ترتيب الدولة"),
  vb_integration_lvl  = list(en = "Integration Level",      ar = "مستوى التكامل"),
  vb_yoy_overall      = list(en = "YoY Change",             ar = "التغير السنوي"),

  # ---------------------------------------------------------------------------
  # GCC Overall tab
  # ---------------------------------------------------------------------------
  gcc_box_trend = list(
    en = "Overall Integration Trend (2015–2023)",
    ar = "اتجاه التكامل الإجمالي (2015–2023)"
  ),
  gcc_box_dimensions_time = list(
    en = "GCC Integration Dimensions Over Time",
    ar = "أبعاد التكامل في دول المجلس عبر الزمن"
  ),

  # ---------------------------------------------------------------------------
  # GCC Timeseries tab
  # ---------------------------------------------------------------------------
  ts_box_dimension_ts = list(
    en = "Dimension Timeseries",
    ar = "السلاسل الزمنية للأبعاد"
  ),
  ts_box_dimension_compare = list(
    en = "Dimension Comparison (Latest Year)",
    ar = "مقارنة الأبعاد (آخر سنة)"
  ),
  ts_box_correlation = list(
    en = "Dimension Correlation Matrix",
    ar = "مصفوفة الارتباط بين الأبعاد"
  ),
  ts_summary_bar_label = list(
    en = "Average Score",
    ar = "متوسط الدرجة"
  ),
  ts_pills_label = list(
    en = "Select Dimension",
    ar = "اختر البُعد"
  ),

  # ---------------------------------------------------------------------------
  # Country Profiles tab
  # ---------------------------------------------------------------------------
  cp_box_select = list(en = "Country Selection",           ar = "اختيار الدولة"),
  cp_label_select = list(en = "Select Country",            ar = "اختر الدولة"),
  cp_box_trend   = list(en = "Overall Integration Index Trend", ar = "اتجاه مؤشر التكامل الإجمالي"),
  cp_box_radar   = list(en = "Dimension Scores (Latest Year)",  ar = "درجات الأبعاد (آخر سنة)"),
  cp_box_all_dim = list(en = "All Dimensions Over Time",        ar = "جميع الأبعاد عبر الزمن"),
  cp_box_vs_gcc  = list(en = "Country vs GCC Average",          ar = "الدولة مقابل متوسط المجلس"),

  # ---------------------------------------------------------------------------
  # Country Heatmap tab
  # ---------------------------------------------------------------------------
  hm_box_controls    = list(en = "Heatmap Controls",          ar = "ضوابط الخريطة الحرارية"),
  hm_box_heatmap     = list(en = "Country-Dimension Heatmap", ar = "الخريطة الحرارية: الدول والأبعاد"),
  hm_box_rankings    = list(en = "Country Rankings by Dimension", ar = "ترتيب الدول حسب البُعد"),
  hm_label_year      = list(en = "Select Year",               ar = "اختر السنة"),
  hm_label_metric    = list(en = "Select Metric",             ar = "اختر المقياس"),
  hm_metric_score    = list(en = "Score",                     ar = "الدرجة"),
  hm_metric_rank     = list(en = "Rank",                      ar = "الترتيب"),
  hm_metric_change   = list(en = "Year-over-Year Change",     ar = "التغير السنوي"),

  # Integration level labels
  level_good     = list(en = "Good",     ar = "جيد"),
  level_moderate = list(en = "Moderate", ar = "متوسط"),
  level_weak     = list(en = "Weak",     ar = "ضعيف"),

  # ---------------------------------------------------------------------------
  # GCC Analytics tab
  # ---------------------------------------------------------------------------
  an_box_year_select  = list(en = "Year Selection",                        ar = "اختيار السنة"),
  an_label_year       = list(en = "Select Year",                           ar = "اختر السنة"),
  an_box_yoy_overall  = list(en = "Year-over-Year Changes in Overall Index", ar = "التغيرات السنوية في المؤشر الإجمالي"),
  an_box_yoy_dim      = list(en = "Year-over-Year Changes by Dimension",   ar = "التغيرات السنوية حسب البُعد"),
  an_box_contribution = list(en = "Contribution to Total GCC Index Change",ar = "الإسهام في إجمالي تغير مؤشر المجلس"),
  an_box_trends       = list(en = "Annual Change Trends (All Years)",      ar = "اتجاهات التغير السنوي (جميع السنوات)"),
  an_box_change_heatmap = list(en = "Dimension Change Heatmap",            ar = "الخريطة الحرارية لتغيرات الأبعاد"),
  an_info_yoy = list(
    en = "Year-over-year changes capture annual momentum in regional integration.",
    ar = "تعكس التغيرات السنوية الزخم السنوي للتكامل الإقليمي."
  ),
  an_info_contribution = list(
    en = "Contribution analysis decomposes total index change by country and dimension.",
    ar = "يُحلّل تحليل الإسهام التغير الكلي في المؤشر حسب الدولة والبُعد."
  ),

  # ---------------------------------------------------------------------------
  # Data Explorer tab
  # ---------------------------------------------------------------------------
  de_box_explorer    = list(en = "Data Explorer",          ar = "مستكشف البيانات"),
  de_box_table       = list(en = "Data Table",             ar = "جدول البيانات"),
  de_label_dataset   = list(en = "Select Dataset",         ar = "اختر مجموعة البيانات"),
  de_dataset_overall = list(en = "Overall Index",          ar = "المؤشر الإجمالي"),
  de_dataset_trade   = list(en = "Trade Integration",      ar = "تكامل التجارة"),
  de_dataset_financial = list(en = "Financial Integration", ar = "التكامل المالي"),
  de_dataset_labor   = list(en = "Labor Integration",      ar = "تكامل العمالة"),
  de_dataset_infra   = list(en = "Infrastructure",         ar = "البنية التحتية"),
  de_dataset_sustain = list(en = "Sustainability",         ar = "الاستدامة"),
  de_dataset_convergence = list(en = "Macroeconomic Convergence", ar = "التقارب الاقتصادي الكلي"),
  de_btn_download    = list(en = "Download CSV",           ar = "تحميل CSV"),

  # Column headers for the data table
  col_country    = list(en = "Country",        ar = "الدولة"),
  col_year       = list(en = "Year",           ar = "السنة"),
  col_score      = list(en = "Score",          ar = "الدرجة"),
  col_rank       = list(en = "Rank",           ar = "الترتيب"),
  col_level      = list(en = "Level",          ar = "المستوى"),
  col_trade      = list(en = "Trade",          ar = "التجارة"),
  col_financial  = list(en = "Financial",      ar = "المالي"),
  col_labor      = list(en = "Labor",          ar = "العمالة"),
  col_infra      = list(en = "Infrastructure", ar = "البنية التحتية"),
  col_sustain    = list(en = "Sustainability", ar = "الاستدامة"),
  col_convergence = list(en = "Convergence",   ar = "التقارب"),
  col_overall    = list(en = "Overall Index",  ar = "المؤشر الإجمالي"),

  # ---------------------------------------------------------------------------
  # Metadata tab
  # ---------------------------------------------------------------------------
  meta_tab_title = list(en = "Metadata & Methodology", ar = "البيانات الوصفية والمنهجية"),
  meta_section_overview = list(en = "Index Overview",  ar = "نظرة عامة على المؤشر"),
  meta_section_methodology = list(en = "Methodology",  ar = "المنهجية"),
  meta_section_dimensions  = list(en = "Dimensions",   ar = "الأبعاد"),
  meta_section_data_sources = list(en = "Data Sources", ar = "مصادر البيانات"),
  meta_section_normalisation = list(en = "Normalisation", ar = "التطبيع"),
  meta_section_weighting    = list(en = "Weighting",    ar = "الأوزان"),
  meta_section_aggregation  = list(en = "Aggregation",  ar = "التجميع"),

  meta_overview_p1 = list(
    en = paste(
      "The GCC Economic Integration Index (GCCEII) is a composite indicator designed to measure",
      "the depth and breadth of economic integration among the six GCC member states:",
      "Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, and the United Arab Emirates.",
      "The index covers annual data from 2015 to 2023."
    ),
    ar = paste(
      "مؤشر التكامل الاقتصادي لدول مجلس التعاون الخليجي (GCCEII) مؤشر مركّب صُمِّم لقياس",
      "عمق واتساع التكامل الاقتصادي بين الدول الأعضاء الست في مجلس التعاون الخليجي:",
      "البحرين والكويت وعُمان وقطر والمملكة العربية السعودية والإمارات العربية المتحدة.",
      "يغطي المؤشر بيانات سنوية من 2015 إلى 2023."
    )
  ),
  meta_methodology_p1 = list(
    en = paste(
      "The GCCEII follows the JRC/OECD Handbook on Constructing Composite Indicators (2008).",
      "The methodology involves four main steps: (1) theoretical framework development,",
      "(2) data selection and quality assessment, (3) normalisation and missing data imputation,",
      "and (4) weighting and aggregation."
    ),
    ar = paste(
      "يتبع GCCEII دليل JRC/OECD لبناء المؤشرات المركّبة (2008).",
      "تتضمن المنهجية أربع خطوات رئيسية: (1) تطوير الإطار النظري،",
      "(2) اختيار البيانات وتقييم جودتها، (3) التطبيع واحتساب البيانات المفقودة،",
      "و(4) الأوزان والتجميع."
    )
  ),
  meta_normalisation_p1 = list(
    en = paste(
      "All indicators are normalised to a 0–100 scale using min-max normalisation.",
      "For positive indicators (higher = better integration), the formula is:",
      "(x - min) / (max - min) × 100.",
      "For negative indicators, the direction is reversed before normalisation."
    ),
    ar = paste(
      "تُطبَّع جميع المؤشرات على مقياس من 0 إلى 100 باستخدام التطبيع min-max.",
      "للمؤشرات الإيجابية (كلما ارتفعت القيمة كان التكامل أفضل)، تكون الصيغة:",
      "(س - الحد الأدنى) / (الحد الأقصى - الحد الأدنى) × 100.",
      "بالنسبة للمؤشرات السلبية، يُعكس الاتجاه قبل التطبيع."
    )
  ),
  meta_weighting_p1 = list(
    en = paste(
      "Each dimension receives an equal weight of 1/6 (~16.7%) in the overall index.",
      "Within each dimension, indicators are weighted based on data availability,",
      "theoretical relevance, and principal component analysis (PCA) guidance.",
      "Weights are reviewed annually as new data become available."
    ),
    ar = paste(
      "يحصل كل بُعد على وزن متساوٍ بمقدار 1/6 (~16.7%) في المؤشر الإجمالي.",
      "داخل كل بُعد، تُحدَّد أوزان المؤشرات بناءً على توافر البيانات",
      "والأهمية النظرية وتوجيهات تحليل المكونات الرئيسية (PCA).",
      "تُراجَع الأوزان سنوياً مع توافر بيانات جديدة."
    )
  ),
  meta_aggregation_p1 = list(
    en = paste(
      "The overall index is computed as the weighted arithmetic mean of the six dimension scores.",
      "Dimension scores are themselves the weighted arithmetic mean of their constituent indicators.",
      "The arithmetic mean is preferred for its transparency and interpretability."
    ),
    ar = paste(
      "يُحسَب المؤشر الإجمالي بوصفه المتوسط الحسابي الموزّون لدرجات الأبعاد الستة.",
      "تُمثّل درجات الأبعاد بدورها المتوسط الحسابي الموزّون للمؤشرات المكوِّنة لها.",
      "يُفضَّل المتوسط الحسابي لشفافيته وسهولة تفسيره."
    )
  ),

  # Table headers for dimension metadata table
  meta_table_dimension   = list(en = "Dimension",         ar = "البُعد"),
  meta_table_weight      = list(en = "Weight",            ar = "الوزن"),
  meta_table_indicators  = list(en = "No. of Indicators", ar = "عدد المؤشرات"),
  meta_table_source      = list(en = "Primary Source",    ar = "المصدر الرئيسي"),
  meta_table_frequency   = list(en = "Frequency",         ar = "التكرار"),
  meta_table_coverage    = list(en = "Coverage",          ar = "التغطية"),
  meta_freq_annual       = list(en = "Annual",            ar = "سنوي"),
  meta_src_comtrade      = list(en = "UN Comtrade",       ar = "UN Comtrade"),
  meta_src_imf           = list(en = "IMF / World Bank",  ar = "صندوق النقد الدولي / البنك الدولي"),
  meta_src_ilo           = list(en = "ILO / GCC-Stat",    ar = "منظمة العمل الدولية / GCC-Stat"),
  meta_src_gccstat       = list(en = "GCC-Stat / MARSA",  ar = "GCC-Stat / MARSA"),
  meta_coverage_gcc6     = list(en = "6 GCC States",      ar = "6 دول خليجية"),

  # ---------------------------------------------------------------------------
  # Chart axis / label strings
  # ---------------------------------------------------------------------------
  axis_score       = list(en = "Score",                      ar = "الدرجة"),
  axis_year        = list(en = "Year",                       ar = "السنة"),
  axis_country     = list(en = "Country",                    ar = "الدولة"),
  axis_dimension   = list(en = "Dimension",                  ar = "البُعد"),
  axis_rank        = list(en = "Rank",                       ar = "الترتيب"),
  axis_change      = list(en = "Change",                     ar = "التغير"),
  axis_change_overall = list(en = "Change in Overall Index", ar = "التغير في المؤشر الإجمالي"),
  axis_yoy_change  = list(en = "Year-over-Year Change",      ar = "التغير السنوي"),
  axis_contribution = list(en = "Contribution to Change",    ar = "الإسهام في التغير"),
  axis_overall_index = list(en = "Overall Integration Index", ar = "مؤشر التكامل الإجمالي"),
  axis_colorbar_change = list(en = "Change",                 ar = "التغير"),

  # Generic / shared
  label_gcc_average = list(en = "GCC Average",   ar = "متوسط المجلس"),
  label_all         = list(en = "All",            ar = "الكل"),
  label_select_all  = list(en = "Select All",     ar = "تحديد الكل"),
  label_loading     = list(en = "Loading…",       ar = "جارٍ التحميل…"),
  label_no_data     = list(en = "No data available", ar = "لا توجد بيانات متاحة")
)

# ── Public API ────────────────────────────────────────────────────────────────

#' Translate a single key
#' @param key  Character. Key from .translations list.
#' @param lang Character. "en" or "ar".
#' @return     Character string in the requested language.
t <- function(key, lang = "en") {
  lang <- match.arg(lang, c("en", "ar"))
  entry <- .translations[[key]]
  if (is.null(entry)) {
    warning(sprintf("[translations] Key '%s' not found. Returning key as fallback.", key))
    return(key)
  }
  entry[[lang]]
}

# ── Country names ─────────────────────────────────────────────────────────────

.country_translations <- c(
  "Bahrain"      = "البحرين",
  "Kuwait"       = "الكويت",
  "Oman"         = "عُمان",
  "Qatar"        = "قطر",
  "Saudi Arabia" = "المملكة العربية السعودية",
  "UAE"          = "الإمارات العربية المتحدة",
  "GCC"          = "دول مجلس التعاون الخليجي"
)

#' Translate a country name
#' @param name Character. English country name.
#' @param lang Character. "en" or "ar".
#' @return     Character.
translate_country <- function(name, lang = "en") {
  if (lang == "en") return(name)
  ar <- .country_translations[name]
  if (is.na(ar)) {
    warning(sprintf("[translations] Country '%s' not in dictionary.", name))
    return(name)
  }
  unname(ar)
}

#' Translate a vector of country names
translate_countries <- function(names, lang = "en") {
  vapply(names, translate_country, character(1), lang = lang)
}

# ── Dimension names ───────────────────────────────────────────────────────────

.dimension_translations <- c(
  "Trade"          = "التجارة",
  "Financial"      = "المالي",
  "Labor"          = "العمالة",
  "Infrastructure" = "البنية التحتية",
  "Sustainability" = "الاستدامة",
  "Convergence"    = "التقارب"
)

# Full dimension names used in some contexts
.dimension_full_translations <- c(
  "Trade Integration"             = "تكامل التجارة",
  "Financial & Monetary Integration" = "التكامل المالي والنقدي",
  "Labor & Human Capital Integration" = "تكامل العمالة ورأس المال البشري",
  "Infrastructure Connectivity"  = "ترابط البنية التحتية",
  "Sustainability & Diversification" = "الاستدامة والتنويع",
  "Macroeconomic Convergence"    = "التقارب الاقتصادي الكلي"
)

#' Translate a dimension label (short form)
#' @param name Character. E.g. "Trade", "Financial".
#' @param lang Character. "en" or "ar".
#' @param form Character. "short" (default) or "full".
translate_dimension <- function(name, lang = "en", form = "short") {
  if (lang == "en") return(name)
  dict <- if (form == "full") .dimension_full_translations else .dimension_translations
  ar <- dict[name]
  if (is.na(ar)) {
    warning(sprintf("[translations] Dimension '%s' not in dictionary.", name))
    return(name)
  }
  unname(ar)
}

#' Translate a vector of dimension labels
translate_dimensions <- function(names, lang = "en", form = "short") {
  vapply(names, translate_dimension, character(1), lang = lang, form = form)
}

# ── Integration level ─────────────────────────────────────────────────────────

#' Translate integration level string
#' @param level Character. "Good", "Moderate", or "Weak".
#' @param lang  Character. "en" or "ar".
translate_level <- function(level, lang = "en") {
  if (lang == "en") return(level)
  map <- c(Good = "جيد", Moderate = "متوسط", Weak = "ضعيف")
  unname(map[level])
}

# ── Layout helpers ────────────────────────────────────────────────────────────

#' Return the text direction for CSS
#' @param lang Character. "en" or "ar".
#' @return "ltr" or "rtl"
get_direction <- function(lang = "en") {
  if (lang == "ar") "rtl" else "ltr"
}

#' Return the CSS font-family string appropriate for the language
#' @param lang Character. "en" or "ar".
#' @return Character CSS font-family string.
get_font <- function(lang = "en") {
  if (lang == "ar") {
    "'Noto Sans Arabic', 'Cairo', 'Tajawal', Arial, sans-serif"
  } else {
    "'Source Sans Pro', 'Lato', Arial, sans-serif"
  }
}

#' Return the text-align value for the language
get_text_align <- function(lang = "en") {
  if (lang == "ar") "right" else "left"
}
