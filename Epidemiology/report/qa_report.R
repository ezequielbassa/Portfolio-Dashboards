# ==============================================================================
#  PAHO Core Indicators — Americas Health Dashboard 2025
#  QA Audit Report Generator
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Version : 1.0 | 2026-04-01
# ==============================================================================
# Run from the project directory after qa_audit.R has been executed.
# Output: qa_output/PAHO_Dashboard_QA_Report.docx
# ==============================================================================

library(officer)
library(dplyr)

OUTPUT_FILE <- "qa/qa_output/PAHO_Dashboard_QA_Report.docx"

# ── Formatting helpers ─────────────────────────────────────────────────────────
tbl_style  <- "Normal Table"
pass_label <- "PASS"
fail_label <- "FAIL"

add_h1 <- function(doc, text) {
  body_add_par(doc, text, style = "heading 1")
}
add_h2 <- function(doc, text) {
  body_add_par(doc, text, style = "heading 2")
}
add_h3 <- function(doc, text) {
  body_add_par(doc, text, style = "heading 3")
}
add_p <- function(doc, text) {
  body_add_par(doc, text, style = "Normal")
}
add_blank <- function(doc) {
  body_add_par(doc, "", style = "Normal")
}

# ── Data for the report ────────────────────────────────────────────────────────

# Section 1 — Data Integrity
schema_df <- data.frame(
  Column        = c("indicator_id","indicator","iso3","country","year","value"),
  Expected_Type = c("character","character","character","character","integer","numeric"),
  Actual_Type   = c("integer","character","character","character","integer","numeric"),
  Status        = c("Note","PASS","PASS","PASS","PASS","PASS"),
  stringsAsFactors = FALSE
)

validation_df <- data.frame(
  Rule                    = c("No NA — indicator","No NA — country","No NA — iso3",
                               "No NA — year","No NA — value","Value is finite",
                               "Year in range 1990-2030","ISO3 length = 3 chars",
                               "Country name not empty","Value non-negative",
                               "Percentage indicators ≤ 105"),
  Records_Checked         = rep("337,970", 11),
  Passes                  = c("337,970","337,970","337,970","337,970","337,970",
                               "337,970","286,030","337,970","337,970","336,707","337,967"),
  Fails                   = c("0","0","0","0","0","0","51,940","0","0","1,263","3"),
  Result                  = c("PASS","PASS","PASS","PASS","PASS","PASS",
                               "INFO","PASS","PASS","RESOLVED","RESOLVED"),
  stringsAsFactors = FALSE
)

neg_df <- data.frame(
  Indicator     = c("BCG Immunization coverage (%)","PCV3 Immunization coverage (%)",
                    "Annual GDP growth (%)","Annual population growth rate (%)",
                    "Rotavirus Immunization coverage (%)","Inflation (%)",
                    "Mortality under-registration (%)","Polio 3 Immunization coverage (%)"),
  Records       = c("352","275","244","244","90","53","4","1"),
  Classification = c("PAHO encoding — fixed","PAHO encoding — fixed",
                     "Legitimate (recessions)","Legitimate (declining pop.)",
                     "PAHO encoding — fixed","Legitimate (deflation/hyperinflation)",
                     "PAHO encoding — fixed","PAHO encoding — fixed"),
  Action        = c("Treated as NA","Treated as NA","Accepted","Accepted",
                    "Treated as NA","Accepted","Treated as NA","Treated as NA"),
  stringsAsFactors = FALSE
)

pct_df <- data.frame(
  Indicator = rep("Inflation (%)", 3),
  Country   = c("Suriname","Venezuela (Bolivarian Republic of)",
                "Venezuela (Bolivarian Republic of)"),
  Year      = c(1995, 2015, 2016),
  Value     = c(235.6, 121.7, 254.9),
  Assessment = rep("Valid — documented hyperinflation", 3),
  stringsAsFactors = FALSE
)

# Section 2 — Unit Tests
unit_df <- data.frame(
  Test_ID  = paste0("T", sprintf("%02d", 1:25)),
  Function = c(rep("latest_val()",6), rep("latest_year()",3),
               rep("latest_country()",4), rep("Data pipeline",7),
               rep("Country groups",2), rep("KPI logic",3) ),
  Description = c(
    "Returns numeric scalar for valid indicator",
    "country_filter returns country-specific value",
    "Unknown indicator returns empty (length 0)",
    "Unknown country returns empty (length 0)",
    "Boundary: single-row dataframe",
    "Boundary: empty dataframe returns length 0",
    "Returns plausible year integer",
    "Unknown indicator returns -Inf or NA",
    "All-NA values returns -Inf or NA",
    "One row per country (no duplicates)",
    "Required columns present",
    "Boundary: empty input returns 0 rows",
    "Values correspond to maximum year per country",
    "No negative values in non-economic indicators",
    "Year column is integer type",
    "Value column is numeric",
    "ISO3 codes are exactly 3 characters",
    "No whitespace-only country names",
    "Non-economic percentage indicators ≤ 105",
    "All 6 featured KPIs exist in dataset",
    "No country assigned to multiple regions",
    "Covers all 4 expected regions",
    "Median life expectancy between 50 and 90 years",
    "Infant mortality median is positive",
    "Doctor density median > 0"
  ),
  Result = rep("PASS", 25),
  stringsAsFactors = FALSE
)

# Section 4 — Performance
perf_df <- data.frame(
  Operation = c(
    "readRDS(paho_clean.rds)",
    "filter + slice_max: Life Expectancy latest per country",
    "filter + slice_max: Infant Mortality latest per country",
    "inner_join: GDP per capita vs Life Expectancy",
    "KPI median — all 6 featured indicators",
    "df_f() simulation: filter countries + year range"
  ),
  Elapsed_Secs = c(2.08, 0.06, 0.04, 0.15, 0.40, 0.09),
  Assessment   = c("Acceptable — runs once at startup",
                   "Excellent","Excellent","Good","Acceptable","Good"),
  stringsAsFactors = FALSE
)

# Section 5 — UAT
uat_df <- data.frame(
  ID     = paste0("UAT-", sprintf("%02d", c(1:9, 23:26))),
  Tab    = c(rep("Overview",5), rep("Trend Explorer",4), rep("Health System",4)),
  Test_Case = c(
    "KPI cards display correct median values for all 6 featured indicators",
    "Choropleth map renders for all Americas countries with red-amber-green scale",
    "Changing Map Year selector visibly updates the map",
    "Top/Bottom bar shows exactly 5 green and 5 red countries",
    "Sidebar Countries filter updates all KPI cards to reflect selected subset",
    "Line chart displays one distinct color line per selected country",
    "Sex split toggle adds female/male suffix lines to chart",
    "Data table shows selected countries with latest year values sorted desc",
    "Country Comparison bar chart reorders correctly by value",
    "Medical Doctors density bar matches Excel cross-reference within ±0.1",
    "Health expenditure grouped bar shows Public vs OOP for all countries",
    "Hospital beds chart sorted ascending, correct y-axis label",
    "GDP vs Life Expectancy scatter tooltip shows country, GDP, and LE on hover"
  ),
  Priority = c(rep("High",5), rep("High",4), rep("High",4)),
  Status   = c(rep("PASS",5), "FAIL", rep("PASS",3), rep("PASS",4)),
  Notes    = c(rep("",5),
               "Color differentiation improvement needed with many countries",
               rep("",3), rep("",4)),
  stringsAsFactors = FALSE
)

uat_pending_df <- data.frame(
  Tab      = c(rep("Country Profile",4), rep("Disease Burden",4),
               rep("Maternal & Child",5)),
  Test_IDs = c("UAT-10","UAT-11","UAT-12","UAT-13",
               "UAT-14","UAT-15","UAT-16","UAT-17",
               "UAT-18","UAT-19","UAT-20","UAT-21","UAT-22"),
  Priority = rep("Medium", 13),
  Status   = rep("Pending", 13),
  stringsAsFactors = FALSE
)

sprint_df <- data.frame(
  ID       = paste0("QA-", sprintf("%02d", 1:10)),
  Finding  = c(
    "Confirm pct indicators >105 are valid — confirmed (Inflation only)",
    "Investigate extreme outliers flagged by 3xIQR vs PAHO official source",
    "Validate iso3 codes against ISO 3166-1 alpha-3 standard",
    "Measure app load time with profvis full report",
    "Improve line color differentiation in Trend Explorer (UAT-06 fix)",
    "Cross-browser test: verify map renders in Firefox and Safari",
    "Record shinytest2 snapshot baselines for regression detection",
    "Extend unit tests to cover profile_trend() and mch_trend() helpers",
    "Add boundary test: empty country selection in sidebar",
    "Complete Medium priority UAT items (UAT-10 to UAT-22)"
  ),
  Priority = c("Low","High","Medium","Medium","High",
               "Low","High","Medium","High","Medium"),
  Sprint   = c(1,1,2,2,1,2,2,3,1,2),
  Status   = rep("Open", 10),
  stringsAsFactors = FALSE
)

# ==============================================================================
# BUILD DOCUMENT
# ==============================================================================
doc <- read_docx()

# ── COVER ──────────────────────────────────────────────────────────────────────
doc <- doc |>
  add_blank() |>
  add_blank() |>
  add_h1("Americas Health Core Indicators Dashboard 2025") |>
  add_h2("Quality Assurance Audit Report") |>
  add_blank() |>
  add_p("Author:       Ezequiel Bassa | Senior Data Scientist & Sociologist") |>
  add_p("Date:         April 1, 2026") |>
  add_p("Version:      1.0") |>
  add_p("Dashboard:    https://ezequielbassa.shinyapps.io/3Epidemiology/") |>
  add_p("Data Source:  PAHO/EIH Open Data — Core Indicators 2025") |>
  add_p("Stack:        R · Shiny · bslib · plotly · DT") |>
  body_add_break()

# ── 1. EXECUTIVE SUMMARY ───────────────────────────────────────────────────────
doc <- doc |>
  add_h1("1. Executive Summary") |>
  add_p(paste0(
    "This report presents the results of a comprehensive Quality Assurance audit ",
    "conducted on the Americas Health Core Indicators Dashboard, an interactive ",
    "epidemiological tool built with R Shiny and powered by PAHO/EIH Open Data. ",
    "The audit covered five pillars: Data Integrity, Automated Unit Testing, ",
    "Shiny UI Reactive Testing, Performance Profiling, and User Acceptance Testing (UAT)."
  )) |>
  add_blank() |>
  add_p(paste0(
    "The dashboard passed all critical quality checks. One data defect was identified ",
    "and resolved during the audit: 722 negative coverage values originating from ",
    "PAHO source encoding were incorrectly treated as valid data. These have been ",
    "corrected and the fix deployed to production. One UI improvement was flagged ",
    "(UAT-06: line color differentiation in the Trend Explorer) for a future sprint."
  )) |>
  add_blank()

# Summary table
summary_tbl <- data.frame(
  Pillar          = c("Data Integrity","Unit Testing","Shiny UI Testing",
                      "Performance","UAT — High Priority"),
  Result          = c("10/11 rules PASS (1 informational)",
                      "25/25 PASS (after 2 threshold fixes)",
                      "App loads correctly; browser automation pending",
                      "All reactive ops < 0.5s",
                      "12/13 PASS | 1 FAIL (UI improvement)"),
  Status          = c("PASS","PASS","INFO","PASS","PASS"),
  stringsAsFactors = FALSE
)
doc <- doc |>
  body_add_table(summary_tbl, style = tbl_style) |>
  add_blank() |>
  body_add_break()

# ── 2. SCOPE AND METHODOLOGY ───────────────────────────────────────────────────
doc <- doc |>
  add_h1("2. Scope and Methodology") |>
  add_p(paste0(
    "The QA audit was conducted programmatically using a dedicated R script ",
    "(qa_audit.R) executed against the project files located at the WSL Ubuntu ",
    "environment. The five-pillar methodology follows industry best practices ",
    "for data product quality assurance."
  )) |>
  add_blank()

scope_tbl <- data.frame(
  Pillar      = c("1 — Data Integrity","2 — Unit Testing",
                  "3 — UI / Reactive Testing","4 — Performance",
                  "5 — Stakeholder Alignment"),
  Tools_Used  = c("validate, openxlsx","Custom test runner (testthat-compatible)",
                  "shinytest2, Chromote","profvis, system.time()",
                  "UAT checklist, Sprint backlog"),
  Scope       = c("337,970 records × 6 columns × 11 rules",
                  "25 unit tests across 3 helper functions + data pipeline",
                  "11 headless browser interaction tests",
                  "6 key reactive operations benchmarked",
                  "26 UAT items across 6 dashboard tabs"),
  stringsAsFactors = FALSE
)
doc <- doc |>
  body_add_table(scope_tbl, style = tbl_style) |>
  add_blank() |>
  body_add_break()

# ── 3. DATA INTEGRITY ──────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("3. Data Integrity and Validation") |>
  add_h2("3.1 Dataset Overview") |>
  add_blank()

overview_tbl <- data.frame(
  Metric    = c("Total records","Countries","Indicators","Year range",
                "Duplicate records","Missing values (NAs)"),
  Value     = c("337,970","49","299","1990 — 2023","0","0"),
  stringsAsFactors = FALSE
)
doc <- doc |>
  body_add_table(overview_tbl, style = tbl_style) |>
  add_blank() |>
  add_h2("3.2 Schema and Type Checking") |>
  add_p(paste0(
    "All 6 required columns are present. One minor type note was identified: ",
    "indicator_id is stored as integer in the source data rather than character. ",
    "This has no functional impact as the column is not used in display logic or ",
    "reactive filtering within app.R."
  )) |>
  add_blank() |>
  body_add_table(schema_df, style = tbl_style) |>
  add_blank() |>
  add_h2("3.3 Programmatic Validation Rules") |>
  add_p(paste0(
    "Eleven validation rules were applied using the validate package. ",
    "The year range rule (1990-2030) flagged 51,940 records with years predating 1990, ",
    "which are legitimate historical PAHO records. These are informational only as the ",
    "dashboard year slider defaults to 2000 and filters them from all visualizations."
  )) |>
  add_blank() |>
  body_add_table(validation_df, style = tbl_style) |>
  add_blank() |>
  add_h2("3.4 Negative Values Investigation") |>
  add_p(paste0(
    "1,263 records with negative values were detected and investigated. ",
    "These split into two distinct groups: legitimate economic/demographic indicators ",
    "where negatives are scientifically valid (GDP growth, population growth, inflation), ",
    "and PAHO source encoding artifacts in coverage indicators where negative values ",
    "represent suppressed or unreliable data."
  )) |>
  add_blank() |>
  body_add_table(neg_df, style = tbl_style) |>
  add_blank() |>
  add_p(paste0(
    "Fix applied in app.R: negative values in non-economic indicators are now treated as ",
    "NA during the data cleaning pipeline and excluded from all chart computations. ",
    "The fix was deployed to shinyapps.io on April 1, 2026."
  )) |>
  add_blank() |>
  add_h2("3.5 Percentage Values Exceeding 100") |>
  add_p(paste0(
    "Three records with percentage values above 105 were flagged and investigated. ",
    "All three belong to Inflation (%) and represent documented hyperinflationary ",
    "episodes. These are valid data points and require no correction."
  )) |>
  add_blank() |>
  body_add_table(pct_df, style = tbl_style) |>
  add_blank() |>
  add_h2("3.6 Outlier Detection") |>
  add_p(paste0(
    "15,964 records were flagged as extreme outliers using a 3×IQR threshold ",
    "applied per indicator. This is expected in epidemiological data where indicators ",
    "such as Dengue cases, Cholera cases, and Malaria cases vary by orders of magnitude ",
    "across countries and years. A full outlier register is available in the accompanying ",
    "Excel report (paho_qa_report.xlsx — tab: Outliers)."
  )) |>
  add_blank() |>
  body_add_break()

# ── 4. UNIT TESTING ────────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("4. Automated Unit Testing") |>
  add_p(paste0(
    "25 unit tests were executed covering three core helper functions (latest_val(), ",
    "latest_year(), latest_country()), the data pipeline integrity, country group ",
    "consistency, and KPI logic validation. All 25 tests pass after two test threshold ",
    "corrections identified during the audit."
  )) |>
  add_blank() |>
  add_h2("4.1 Threshold Corrections") |>
  add_p(paste0(
    "Test T14 ('no negative values') was updated to exclude legitimate economic indicators ",
    "(GDP growth, population growth, inflation) from the non-negative constraint. ",
    "Test T19 ('percentage indicators ≤ 105') was updated to exclude Inflation (%) ",
    "which can legitimately exceed 100 during hyperinflationary periods. ",
    "Both corrections are documented with reasoning in qa_audit.R."
  )) |>
  add_blank() |>
  add_h2("4.2 Results") |>
  add_blank() |>
  body_add_table(unit_df, style = tbl_style) |>
  add_blank() |>
  body_add_break()

# ── 5. SHINY UI TESTING ────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("5. Shiny Reactive Logic and UI Testing") |>
  add_p(paste0(
    "Automated UI testing was attempted using the shinytest2 package with a headless ",
    "Chromium browser. The Shiny application launched successfully and all R packages ",
    "loaded without errors (confirmed in shinytest2 logs). However, browser automation ",
    "timed out due to a known WSL-to-Windows localhost port bridging limitation. "
  )) |>
  add_p(paste0(
    "The 11 interaction tests (year slider, country filter, map year, indicator ",
    "switches, sex split toggle, country profile, disease selector) will be completed ",
    "in a native Windows R environment in Sprint 2. Manual UAT (Section 7) covered ",
    "the critical reactive logic paths in the interim."
  )) |>
  add_blank() |>
  body_add_break()

# ── 6. PERFORMANCE ─────────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("6. Performance and Load Testing") |>
  add_p(paste0(
    "Six key reactive operations were benchmarked using system.time(). ",
    "All dashboard reactive computations complete well under 500ms, ensuring ",
    "a responsive user experience. The only operation exceeding 1 second is the ",
    "initial RDS file load (2.08s), which runs once at application startup and ",
    "is attributed to the WSL-to-Windows filesystem overhead. On a native Linux ",
    "server (shinyapps.io) this time is significantly lower."
  )) |>
  add_blank() |>
  body_add_table(perf_df, style = tbl_style) |>
  add_blank() |>
  add_p(paste0(
    "Recommendation: A profvis flame-graph profile was generated ",
    "(qa_output/profvis_reactive.html) for detailed function-level analysis. ",
    "No critical bottlenecks were identified in the reactive computation pipeline."
  )) |>
  add_blank() |>
  body_add_break()

# ── 7. UAT ─────────────────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("7. User Acceptance Testing (UAT)") |>
  add_p(paste0(
    "UAT was conducted against the live production dashboard at ",
    "https://ezequielbassa.shinyapps.io/3Epidemiology/. ",
    "All High priority items were tested manually across the Overview, ",
    "Trend Explorer, and Health System tabs. Medium priority items are ",
    "scheduled for Sprint 2."
  )) |>
  add_blank() |>
  add_h2("7.1 High Priority Results (13 items)") |>
  add_blank() |>
  body_add_table(uat_df, style = tbl_style) |>
  add_blank() |>
  add_h2("7.2 UAT-06 Defect — Trend Explorer Color Differentiation") |>
  add_p(paste0(
    "When multiple countries are selected simultaneously in the Trend Explorer, ",
    "the color palette cycling (8 colors repeating) produces visually similar lines, ",
    "making it difficult to distinguish individual country trends. This is a UI ",
    "improvement item, not a data or logic defect. Proposed fix: expand the palette ",
    "or add line dash patterns as a secondary differentiator. Assigned to Sprint 1."
  )) |>
  add_blank() |>
  add_h2("7.3 Medium Priority — Pending (13 items)") |>
  add_blank() |>
  body_add_table(uat_pending_df, style = tbl_style) |>
  add_blank() |>
  body_add_break()

# ── 8. SPRINT BACKLOG ──────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("8. Sprint Backlog and Recommendations") |>
  add_p(paste0(
    "The following items were identified during the audit and added to the sprint ",
    "backlog for prioritized resolution. Items are ordered by sprint and priority."
  )) |>
  add_blank() |>
  body_add_table(sprint_df, style = tbl_style) |>
  add_blank() |>
  body_add_break()

# ── 9. CONCLUSION ──────────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("9. Conclusion") |>
  add_p(paste0(
    "The Americas Health Core Indicators Dashboard passed all critical quality checks ",
    "across data integrity, automated testing, and user acceptance criteria. The dashboard ",
    "accurately transforms PAHO epidemiological data into clear, actionable visualizations ",
    "covering 49 countries, 299 indicators, and over 337,000 data records across six ",
    "thematic tabs."
  )) |>
  add_blank() |>
  add_p(paste0(
    "One data defect (negative PAHO encoding values in coverage indicators) was identified, ",
    "corrected, and deployed to production during the audit cycle. One UI improvement ",
    "(Trend Explorer color differentiation) was flagged for Sprint 1. Thirteen Medium ",
    "priority UAT items remain pending for Sprint 2 completion."
  )) |>
  add_blank() |>
  add_p(paste0(
    "The dashboard is fit for stakeholder presentation and portfolio publication ",
    "in its current state. The QA process has established a reproducible audit ",
    "baseline — qa_audit.R can be re-executed after any future data update or ",
    "feature change to ensure continued quality."
  )) |>
  add_blank() |>
  add_h2("Sign-off") |>
  add_blank() |>
  add_p("QA conducted by:  Ezequiel Bassa — Senior Data Scientist & Sociologist") |>
  add_p("Date:             April 1, 2026") |>
  add_p("Dashboard URL:    https://ezequielbassa.shinyapps.io/3Epidemiology/") |>
  add_p("GitHub:           https://github.com/ezequielbassa/Portfolio-Dashboards")

# ── SAVE ───────────────────────────────────────────────────────────────────────
print(doc, target = OUTPUT_FILE)
cat("\nReport saved to:", OUTPUT_FILE, "\n")
