# ==============================================================================
#  Workplace Climate Survey — Maritime Logistics Americas 2026
#  Portfolio Page Content Generator
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Version : 1.0 | 2026-04-02
# ==============================================================================
# Output: qa/qa_output/Maritime_Portfolio_Content.docx
# ==============================================================================

library(officer)

OUTPUT_FILE <- "qa/qa_output/Maritime_Portfolio_Content.docx"

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
  add_h1("Maritime Workplace Climate Dashboard 2026") |>
  add_h2("Portfolio Page Content — Framer Reference Document") |>
  add_blank() |>
  add_p("Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist") |>
  add_p("Date    : April 2, 2026") |>
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
  add_p("Maritime Workplace Climate Dashboard") |>
  add_blank() |>
  add_h2("Tagline (1 line)") |>
  add_p(paste0(
    "An interactive organizational intelligence platform exploring workplace climate ",
    "across 10 countries and 20 indicators in the maritime logistics industry."
  )) |>
  add_blank() |>
  add_h2("Short Description (2-3 sentences — for card or preview)") |>
  add_p(paste0(
    "Built with R Shiny, this dashboard transforms a 12,500-respondent synthetic workplace ",
    "climate survey into an interactive analytics platform covering 20 organizational health ",
    "indicators across 10 Americas countries in the maritime logistics industry. ",
    "It enables department-level comparison, regional workforce profiling, open comment ",
    "exploration, and geographic KPI mapping through seven thematic tabs."
  )) |>
  add_blank() |>
  add_h2("Key Metrics — highlight numbers (use as visual badges or KPI chips)") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Metric = c(
        "Survey respondents", "Climate indicators", "Countries covered",
        "Dashboard tabs", "Unit tests passed", "Deployment"
      ),
      Value  = c(
        "12,500", "20", "10",
        "7", "28 / 28", "shinyapps.io (live)"
      ),
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
    "The Maritime Workplace Climate Dashboard is a fully interactive organizational ",
    "analytics tool designed to help HR professionals, executives, and organizational ",
    "researchers explore workforce sentiment data with the depth and flexibility of a ",
    "professional analytics platform — without requiring any technical expertise."
  )) |>
  add_blank() |>
  add_p(paste0(
    "The dashboard covers 20 Likert-scale climate dimensions spanning employee engagement, ",
    "safety culture, work-life balance, career growth, leadership trust, inclusion and ",
    "diversity, retention intent, and more. All data originates from a reproducible ",
    "synthetic survey generation pipeline (Python, seed=42) representing 12,500 employees ",
    "across 10 countries and multiple departments in the Americas maritime logistics sector."
  )) |>
  add_blank() |>
  add_h2("The Challenge") |>
  add_p(paste0(
    "Workplace survey data is inherently multidimensional — it contains dozens of indicators, ",
    "multiple organizational hierarchies (department, region, country, tenure, salary band), ",
    "and qualitative open responses alongside quantitative scores. Presenting this data in a ",
    "way that is simultaneously explorable, honest about its complexity, and immediately ",
    "actionable for non-technical stakeholders required both thoughtful UX design and a ",
    "robust reactive architecture."
  )) |>
  add_blank() |>
  add_h2("The Solution") |>
  add_p(paste0(
    "A single-file R Shiny application with a persistent global sidebar allows any ",
    "combination of filters (region, department, gender, salary band, work type, tenure ",
    "range) to cascade across all seven tabs simultaneously. Seven thematic views provide ",
    "layered analytical depth: from executive KPI summaries and cross-country benchmarking ",
    "to individual department heatmaps, radar comparisons, and verbatim open comment ",
    "browsing. The dark maritime theme (bslib Bootstrap 5) and Plotly interactive charts ",
    "create a professional, immersive experience consistent with modern BI tools."
  )) |>
  add_blank() |>
  add_h2("Quality Assurance") |>
  add_p(paste0(
    "The dashboard underwent a comprehensive 5-pillar professional QA audit. 28 automated ",
    "unit tests achieved a 100% pass rate, covering helper functions, derived column logic, ",
    "Likert range validation, filter behavior, and KPI plausibility. Data integrity was ",
    "verified across 21 programmatic rules with zero violations across all 12,500 records. ",
    "Performance benchmarking confirmed all 8 reactive computations complete within ",
    "acceptable thresholds. No data defects were identified."
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
      Category    = c(
        "Language", "Framework", "UI Theme", "Visualization",
        "Data Tables", "Data Processing", "Data Generation",
        "Deployment Platform", "Version Control", "QA Framework"
      ),
      Detail      = c(
        "R",
        "Shiny (reactive programming) + bslib (Bootstrap 5 theming)",
        "Dark maritime theme — #0d1b2a background, Inter + Barlow Condensed fonts, gold accents",
        "Plotly (choropleth map, lollipop chart, heatmap, radar, scatter, box, violin, histogram)",
        "DT (searchable, sortable, paginated with custom dark CSS)",
        "dplyr, tidyr, scales, glue",
        "Python (generate_maritime_survey.py, seed=42) — 12,500 rows × 30 cols reproducible dataset",
        "shinyapps.io (Posit Cloud)",
        "Git + GitHub (github.com/ezequielbassa/Portfolio-Dashboards)",
        "validate · openxlsx · shinytest2 · profvis · officer"
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
      Tab             = c(
        "Overview", "Climate Indicators", "Department Analysis",
        "Regional View", "Workforce Profile", "Open Comments", "Americas Map"
      ),
      Icon_Suggestion = c(
        "tachometer-alt", "thermometer-half", "building",
        "globe-americas", "users", "comments", "map"
      ),
      Key_Features    = c(
        paste0(
          "4 KPI cards: Total Employees, Avg Engagement, Avg Retention Intent, Avg Operational Stress. ",
          "Department headcount bar chart. Work type donut chart. ",
          "Regional split pie, gender bar, and salary band distribution."
        ),
        paste0(
          "Lollipop chart of all 20 climate indicators ranked low-to-high with color-coded dots. ",
          "Department × Indicator heatmap (red-amber-green). ",
          "Radar chart with configurable group variable and multi-group overlay."
        ),
        paste0(
          "Work-Life Balance vs Operational Stress scatter plot (bubble = headcount per department). ",
          "Boxplot distribution of any indicator by department. ",
          "Engagement and Retention Intent across tenure bands with mid-career crisis zone annotation."
        ),
        paste0(
          "Country composite score bar chart ranked by average climate score. ",
          "Indicator by region bar chart with dynamic indicator selector. ",
          "Full score table: all 10 countries × 20 indicators, scrollable."
        ),
        paste0(
          "Tenure distribution histogram. Age distribution histogram. ",
          "Avg Engagement by tenure band with 95% confidence intervals. ",
          "Environmental Pride by age group violin plot."
        ),
        paste0(
          "Browsable open-response table filtered by minimum avg score, department, and region. ",
          "Employee_ID excluded for respondent privacy. ",
          "Color-bar formatting on avg score column. Comment count indicator."
        ),
        paste0(
          "Americas choropleth map of all 10 countries — color scale from red (critical) to green (good). ",
          "KPI selector: Overall Composite Score or any of 20 individual indicators. ",
          "Hover tooltip showing country name, score, and sample size."
        )
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
        "Visualization & Design",
        "Quality Assurance",
        "Quality Assurance",
        "Quality Assurance",
        "Quality Assurance",
        "DevOps & Deployment",
        "DevOps & Deployment",
        "Domain Knowledge",
        "Domain Knowledge"
      ),
      Skill = c(
        "R (Advanced)",
        "Shiny Reactive Programming",
        "Python (synthetic data generation)",
        "Functional Programming",
        "Organizational Survey Analysis",
        "Likert Scale Interpretation",
        "Derived Metric Engineering",
        "Multi-dimensional Filtering Architecture",
        "Interactive Data Visualization (Plotly)",
        "Dashboard UX/UI Design",
        "Choropleth Mapping",
        "Heatmap & Radar Chart Design",
        "Automated Unit Testing (28 tests)",
        "Data Integrity Auditing (validate)",
        "Performance Profiling (profvis)",
        "User Acceptance Testing (UAT)",
        "Cloud Deployment (shinyapps.io)",
        "Version Control (Git & GitHub)",
        "Human Resources Analytics",
        "Organizational Behavior & Sociology"
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
      Product     = c(
        "Live Interactive Dashboard",
        "Synthetic Survey Dataset",
        "Data Generation Script",
        "Dashboard Application Script",
        "QA Audit Script",
        "QA Audit Report",
        "Excel QA Workbook",
        "Portfolio Content Document",
        "GitHub Repository"
      ),
      Description = c(
        "Fully deployed R Shiny dashboard at shinyapps.io — accessible from any browser",
        "workplace_climate_maritime_2026.csv — 12,500 rows × 30 columns, reproducible from Python script",
        "generate_maritime_survey.py — synthetic data generator (seed=42, 10 countries, 20 indicators)",
        "app.R — single-file Shiny app with global sidebar, 7 tabs, full reactive server logic",
        "qa/qa_audit.R — 5-pillar audit: 21 validate rules, 28 unit tests, UI tests, 8 benchmarks, UAT",
        "Maritime_Dashboard_QA_Report.docx — stakeholder QA report covering all findings and methodology",
        "maritime_qa_report.xlsx — 12-tab workbook with validation results, cross-reference data, UAT checklist",
        "Maritime_Portfolio_Content.docx — structured portfolio copy for ezequielbassa.com Framer page",
        "github.com/ezequielbassa/Portfolio-Dashboards — versioned source code"
      ),
      Format = c(
        "Web app (URL)", "CSV", "Python script", "R script",
        "R script", "Word (.docx)", "Excel (.xlsx)", "Word (.docx)", "GitHub"
      ),
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
  add_p("Source:      Synthetic dataset generated by Ezequiel Bassa") |>
  add_p("Generator:   generate_maritime_survey.py (Python, seed=42, reproducible)") |>
  add_p("Respondents: 12,500 simulated employees") |>
  add_p("Scope:       10 countries in the Americas — maritime logistics sector") |>
  add_p("Indicators:  20 Likert-scale workplace climate dimensions") |>
  add_p("Purpose:     Portfolio demonstration of HR analytics and dashboard design") |>
  add_p("Note:        All data is synthetic and does not represent real individuals or organizations") |>
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
  add_p("Maritime Climate Dashboard | Ezequiel Bassa — Data Scientist") |>
  add_blank() |>
  add_h2("Meta Description (155 characters max)") |>
  add_p(paste0(
    "Interactive R Shiny dashboard exploring workplace climate across 10 Americas countries. ",
    "20 indicators, 12,500 respondents. Built by Ezequiel Bassa."
  )) |>
  add_blank() |>
  add_h2("Keywords") |>
  add_p(paste0(
    "R Shiny dashboard, workplace climate survey, HR analytics, employee engagement, ",
    "organizational analytics, maritime logistics, interactive data visualization, ",
    "plotly R, choropleth map, Likert scale analysis, Ezequiel Bassa"
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
      Block_Order    = paste0("Block ", 1:10),
      Component      = c(
        "Hero — Title + Tagline + 6 metric badges",
        "Embedded dashboard iframe (full width, ~700px height)",
        "Short description (2-3 sentences)",
        "Feature cards — 7 cards, one per dashboard tab",
        "Technical specifications table",
        "Skills — tag chips grouped by category",
        "QA badge section — '28/28 unit tests', '12,500 records audited', 'Live QA report'",
        "Outcome products list",
        "Data source attribution + GitHub link button",
        "Footer — author name, ezequielbassa.com, synthetic data notice"
      ),
      Content_Source = c(
        "Section 1 of this document",
        "https://ezequielbassa.shinyapps.io/maritime-climate-dashboard",
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
