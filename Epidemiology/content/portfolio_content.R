# ==============================================================================
#  PAHO Core Indicators — Americas Health Dashboard 2025
#  Portfolio Page Content Generator
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Version : 1.0 | 2026-04-01
# ==============================================================================
# Output: content/PAHO_Portfolio_Content.docx
# ==============================================================================

library(officer)

OUTPUT_FILE <- "content/PAHO_Portfolio_Content.docx"

add_h1    <- function(doc, text) body_add_par(doc, text, style = "heading 1")
add_h2    <- function(doc, text) body_add_par(doc, text, style = "heading 2")
add_h3    <- function(doc, text) body_add_par(doc, text, style = "heading 3")
add_p     <- function(doc, text) body_add_par(doc, text, style = "Normal")
add_blank <- function(doc)       body_add_par(doc, "",   style = "Normal")

doc <- read_docx()

# ==============================================================================
# COVER
# ==============================================================================
doc <- doc |>
  add_blank() |>
  add_h1("Americas Health Core Indicators Dashboard 2025") |>
  add_h2("Portfolio Page Content — Framer Reference Document") |>
  add_blank() |>
  add_p("Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist") |>
  add_p("Date    : April 1, 2026") |>
  add_p("Purpose : Copy-ready content blocks for ezequielbassa.com portfolio page") |>
  body_add_break()

# ==============================================================================
# SECTION 1 — HERO / HEADLINE BLOCK
# ==============================================================================
doc <- doc |>
  add_h1("1. Hero Block") |>
  add_p("[ Use at the top of the project page, above the embedded dashboard ]") |>
  add_blank() |>
  add_h2("Project Title") |>
  add_p("Americas Health Core Indicators Dashboard") |>
  add_blank() |>
  add_h2("Tagline (1 line)") |>
  add_p(paste0(
    "An interactive epidemiological intelligence platform transforming PAHO open data ",
    "into actionable public health insights across 49 countries."
  )) |>
  add_blank() |>
  add_h2("Short Description (2-3 sentences — for card or preview)") |>
  add_p(paste0(
    "Built with R Shiny, this dashboard transforms 337,970 records from the Pan American ",
    "Health Organization into an interactive platform covering 299 health indicators ",
    "across 49 countries in the Americas. It enables multi-country trend comparison, ",
    "country-level health profiles, disease burden analysis, and health system benchmarking ",
    "through six thematic tabs and a live choropleth map."
  )) |>
  add_blank() |>
  add_h2("Key Metrics — highlight numbers (use as visual badges or KPI chips)") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Metric = c("Countries covered","Health indicators","Records processed",
                 "Dashboard tabs","Unit tests passed","Deployment"),
      Value  = c("49","299","337,970","6","25 / 25","shinyapps.io (live)"),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 2 — PROJECT DESCRIPTION (long form)
# ==============================================================================
doc <- doc |>
  add_h1("2. Project Description (Long Form)") |>
  add_p("[ Use as the main body text below the embedded dashboard ]") |>
  add_blank() |>
  add_h2("Overview") |>
  add_p(paste0(
    "The Americas Health Core Indicators Dashboard is a fully interactive epidemiological ",
    "tool developed to make PAHO's Core Indicators dataset — one of the most comprehensive ",
    "public health open data sources in the Western Hemisphere — accessible to researchers, ",
    "policymakers, and public health professionals without requiring technical expertise."
  )) |>
  add_blank() |>
  add_p(paste0(
    "The dashboard covers the full spectrum of population health: from demographic ",
    "fundamentals and mortality rates to communicable disease surveillance, maternal and ",
    "child health outcomes, non-communicable disease burden, and health system capacity. ",
    "All data is sourced directly from PAHO/EIH Open Data (Core Indicators 2025) and ",
    "processed through a reproducible R data pipeline."
  )) |>
  add_blank() |>
  add_h2("The Challenge") |>
  add_p(paste0(
    "Raw PAHO data is distributed as a 155MB CSV file containing hundreds of indicators ",
    "across multiple decades, dozens of countries, and inconsistent encoding conventions. ",
    "Transforming this into a responsive, intuitive dashboard required building a robust ",
    "data engineering pipeline, a reactive UI architecture capable of handling complex ",
    "multi-country filtering, and a rigorous QA process to ensure data fidelity."
  )) |>
  add_blank() |>
  add_h2("The Solution") |>
  add_p(paste0(
    "A single-file R Shiny application processes, cleans, and caches the PAHO dataset as ",
    "an optimized RDS file at startup. Six thematic tabs provide layered analytical depth: ",
    "from high-level KPI summaries and geographic mapping to granular country profiles and ",
    "time-series trend exploration. All charts are built with Plotly for interactive ",
    "tooltips, zoom, and pan. The dark epidemiological theme (bslib Bootstrap 5) ensures ",
    "professional visual identity consistent with academic and institutional standards."
  )) |>
  add_blank() |>
  add_h2("Quality Assurance") |>
  add_p(paste0(
    "The dashboard underwent a five-pillar professional QA audit covering data integrity ",
    "validation (11 programmatic rules across 337,970 records), 25 automated unit tests ",
    "with 100% pass rate, performance benchmarking of all reactive computations, and a ",
    "26-item User Acceptance Testing checklist against the live production environment. ",
    "One data defect was identified and resolved during the audit: 722 negative coverage ",
    "values originating from PAHO source encoding were treated as missing data and removed ",
    "from all visualizations."
  )) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 3 — TECHNICAL SPECIFICATIONS
# ==============================================================================
doc <- doc |>
  add_h1("3. Technical Specifications") |>
  add_p("[ Use as a specs table or bullet list in the project detail section ]") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Category    = c("Language","Framework","UI Components","Visualization",
                      "Data Tables","Data Processing","Deployment Platform",
                      "Version Control","Data Source","Dataset Size",
                      "Performance","QA Framework"),
      Detail      = c(
        "R",
        "Shiny (reactive programming) + bslib (Bootstrap 5 theming)",
        "shinyWidgets (pickerInput, grouped country selector)",
        "Plotly (choropleth maps, line charts, bar charts, scatter plots)",
        "DT (searchable, sortable, paginated data tables)",
        "dplyr, tidyr, scales, glue",
        "shinyapps.io (Posit Cloud)",
        "Git + GitHub (github.com/ezequielbassa/Portfolio-Dashboards)",
        "PAHO/EIH Open Data — Core Indicators 2025",
        "155MB raw CSV → 337,970 records × 6 columns (optimized RDS)",
        "All reactive computations < 500ms | RDS load ~2s (startup only)",
        "validate · testthat · shinytest2 · profvis · openxlsx · officer"
      ),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 4 — DASHBOARD FEATURES
# ==============================================================================
doc <- doc |>
  add_h1("4. Dashboard Features") |>
  add_p("[ Use as a feature list with icons in Framer — one block per tab ]") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Tab           = c("Overview","Trend Explorer","Country Profile",
                        "Disease Burden","Maternal & Child","Health System"),
      Icon_Suggestion = c("tachometer-alt","chart-line","flag",
                           "virus","baby","hospital"),
      Key_Features  = c(
        paste0("6 KPI cards (Life Expectancy, Infant Mortality, Maternal Mortality, NCD, TB, Doctors). ",
               "Interactive Americas choropleth map with year selector and indicator switcher. ",
               "Top 5 / Bottom 5 country ranking bar chart."),
        paste0("Multi-country time-series line chart for any of 299 indicators. ",
               "Sex-disaggregated trend toggle (female/male breakdown). ",
               "Country comparison bar chart for most recent available year. ",
               "Searchable data table with latest values per country."),
        paste0("Individual country health profile with 6 KPI boxes. ",
               "Four mini trend charts: Life Expectancy & Mortality, Maternal & Infant, ",
               "Disease Burden (Communicable vs NCD), Health System indicators. ",
               "Country selector covering all 49 Americas territories."),
        paste0("Communicable vs NCD mortality grouped bar chart. ",
               "Disease-specific trend lines (Dengue, Malaria, Cholera, Measles, TB). ",
               "HIV/AIDS new diagnoses rate trends. ",
               "Tuberculosis incidence time series."),
        paste0("Under-5, Infant, and Neonatal mortality multi-line chart with line style differentiation. ",
               "Maternal mortality ratio trends. ",
               "DTP3, BCG, and Polio immunization coverage. ",
               "Births attended by skilled personnel and antenatal care coverage."),
        paste0("Medical doctors density per 10,000 population. ",
               "Public health expenditure vs out-of-pocket spending comparison. ",
               "Hospital beds ratio by country. ",
               "GDP per capita (PPP) vs Life Expectancy correlation scatter plot.")
      ),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 5 — SKILLS DEMONSTRATED
# ==============================================================================
doc <- doc |>
  add_h1("5. Skills Demonstrated") |>
  add_p("[ Use as skill tags/chips or a categorized list in Framer ]") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Category = c(
        "Programming & Engineering",
        "Programming & Engineering",
        "Programming & Engineering",
        "Programming & Engineering",
        "Data Science & Analysis",
        "Data Science & Analysis",
        "Data Science & Analysis",
        "Data Science & Analysis",
        "Visualization & Design",
        "Visualization & Design",
        "Visualization & Design",
        "Quality Assurance",
        "Quality Assurance",
        "Quality Assurance",
        "Quality Assurance",
        "DevOps & Deployment",
        "DevOps & Deployment",
        "Domain Knowledge"
      ),
      Skill = c(
        "R (Advanced)",
        "Shiny Reactive Programming",
        "Data Engineering & ETL Pipelines",
        "Functional Programming",
        "Epidemiological Data Analysis",
        "Statistical Cross-Referencing",
        "Outlier Detection & Data Validation",
        "Public Health Indicators & PAHO Standards",
        "Interactive Data Visualization (Plotly)",
        "Dashboard UX/UI Design",
        "Choropleth Mapping",
        "Automated Unit Testing",
        "Data Integrity Auditing",
        "Performance Profiling & Benchmarking",
        "User Acceptance Testing (UAT)",
        "Cloud Deployment (shinyapps.io)",
        "Version Control (Git & GitHub)",
        "Epidemiology & Public Health"
      ),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 6 — OUTCOME PRODUCTS
# ==============================================================================
doc <- doc |>
  add_h1("6. Outcome Products") |>
  add_p("[ Use as a deliverables list in the project detail section ]") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Product         = c(
        "Live Interactive Dashboard",
        "Cleaned Dataset",
        "Data Pipeline Script",
        "QA Audit Script",
        "QA Audit Report",
        "Excel Cross-Reference Report",
        "Portfolio Content Document",
        "GitHub Repository"
      ),
      Description     = c(
        "Fully deployed R Shiny dashboard at shinyapps.io — accessible from any browser, no installation required",
        "paho_clean.rds — 337,970 records, 299 indicators, 49 countries, reproducible from raw PAHO CSV",
        "app.R — single-file Shiny app with automated CSV parsing, cleaning, RDS caching, and full UI/server logic",
        "qa_audit.R — 5-pillar QA audit: data validation, 25 unit tests, shinytest2 UI tests, profvis profiling, UAT checklist",
        "PAHO_Dashboard_QA_Report.docx — professional audit report covering all findings, bug fixes, and sprint backlog",
        "paho_qa_report.xlsx — 12-tab workbook with cross-reference data, validation results, outliers, and UAT checklist",
        "PAHO_Portfolio_Content.docx — structured portfolio copy for ezequielbassa.com Framer page",
        "github.com/ezequielbassa/Portfolio-Dashboards — versioned source code and documentation"
      ),
      Format          = c("Web app (URL)","RDS file","R script","R script",
                          "Word (.docx)","Excel (.xlsx)","Word (.docx)","GitHub"),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 7 — DATA SOURCE ATTRIBUTION
# ==============================================================================
doc <- doc |>
  add_h1("7. Data Source Attribution") |>
  add_p("[ Include in footer or methodology section of the portfolio page ]") |>
  add_blank() |>
  add_p("Source:      Pan American Health Organization (PAHO)") |>
  add_p("Dataset:     PAHO/EIH Core Indicators 2025") |>
  add_p("URL:         https://www.paho.org/en/data-and-statistics") |>
  add_p("License:     PAHO Open Data") |>
  add_p("Coverage:    49 countries and territories across the Americas") |>
  add_p("Indicators:  299 health indicators across 6 thematic domains") |>
  add_p("Time span:   Multi-decade (primary analysis 2000-2023)") |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 8 — SEO / META TAGS
# ==============================================================================
doc <- doc |>
  add_h1("8. SEO and Meta Tags") |>
  add_p("[ Use in Framer page settings under SEO ]") |>
  add_blank() |>
  add_h2("Page Title") |>
  add_p("Americas Health Dashboard | Ezequiel Bassa — Data Scientist") |>
  add_blank() |>
  add_h2("Meta Description (155 characters max)") |>
  add_p(paste0(
    "Interactive R Shiny dashboard visualizing PAHO health indicators for 49 Americas countries. ",
    "Built by Ezequiel Bassa, Senior Data Scientist."
  )) |>
  add_blank() |>
  add_h2("Keywords") |>
  add_p(paste0(
    "R Shiny dashboard, PAHO health indicators, Americas epidemiology, ",
    "interactive data visualization, public health analytics, plotly R, ",
    "epidemiological dashboard, health data science, Ezequiel Bassa"
  )) |>
  add_blank() |>
  body_add_break()

# ==============================================================================
# SECTION 9 — FRAMER LAYOUT SUGGESTION
# ==============================================================================
doc <- doc |>
  add_h1("9. Suggested Framer Page Layout") |>
  add_p("[ Use as a blueprint when building the portfolio page in Framer ]") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Block_Order = paste0("Block ", 1:10),
      Component   = c(
        "Hero — Title + Tagline + 6 metric badges",
        "Embedded dashboard iframe (full width, ~700px height)",
        "Short description (2-3 sentences)",
        "Feature cards — 6 cards, one per dashboard tab",
        "Technical specifications table",
        "Skills — tag chips grouped by category",
        "QA badge section — '25/25 unit tests', '337k records audited', 'Live QA report'",
        "Outcome products list",
        "Data source attribution + GitHub link button",
        "Footer — author name, ezequielbassa.com, PAHO credit"
      ),
      Content_Source = c(
        "Section 1 of this document",
        "https://ezequielbassa.shinyapps.io/3Epidemiology/",
        "Section 2 — Short Description",
        "Section 4 — Dashboard Features",
        "Section 3 — Technical Specifications",
        "Section 5 — Skills Demonstrated",
        "QA Audit Report highlights",
        "Section 6 — Outcome Products",
        "Section 7 — Data Source Attribution",
        "Standard footer"
      ),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank()

# ==============================================================================
# SAVE
# ==============================================================================
print(doc, target = OUTPUT_FILE)
cat("\nPortfolio content document saved to:", OUTPUT_FILE, "\n")
