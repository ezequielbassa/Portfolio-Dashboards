# ==============================================================================
#  PAHO Core Indicators — Americas Health Dashboard 2025
#  Quality Assurance Audit Script
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Version : 1.0 | 2026-04-01
# ==============================================================================
# Run from the project directory:
#   /home/ezequielbassa/Portolio/3Epidemiology/
#
# Required packages (install once):
#   install.packages(c("validate","openxlsx","testthat","shinytest2","profvis"))
# ==============================================================================

library(dplyr)
library(tidyr)
library(validate)
library(openxlsx)

# ── QA CONFIG ──────────────────────────────────────────────────────────────────
QA_DIR    <- "qa/qa_output"
XLSX_FILE <- file.path(QA_DIR, "paho_qa_report.xlsx")
LOG_FILE  <- file.path(QA_DIR, "qa_log.txt")

if (!dir.exists(QA_DIR)) dir.create(QA_DIR, recursive = TRUE)

# Reset log file
writeLines("", LOG_FILE)

qa_log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste(...))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG_FILE, append = TRUE)
}

qa_log("=== PAHO Dashboard QA Audit started ===")

# Shared Excel workbook (all sheets appended throughout the script)
wb <- createWorkbook()

style_header <- function(wb, sheet, n_cols) {
  hs <- createStyle(
    fgFill          = "#003366",
    fontColour      = "#FFFFFF",
    textDecoration  = "bold",
    halign          = "center",
    border          = "Bottom",
    borderColour    = "#00b4d8"
  )
  addStyle(wb, sheet, hs, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
}

# ==============================================================================
# SECTION 1 — DATA INTEGRITY AND VALIDATION
# ==============================================================================
qa_log("--- SECTION 1: Data Integrity and Validation ---")

# ── 1.1  Load data ─────────────────────────────────────────────────────────────
if (!file.exists("app/paho_clean.rds")) {
  stop("app/paho_clean.rds not found. Run app.R once to generate it.")
}
df <- readRDS("app/paho_clean.rds")
qa_log("app/paho_clean.rds loaded:", nrow(df), "rows x", ncol(df), "cols")

# ── 1.2  Schema / type checking ───────────────────────────────────────────────
qa_log("--- 1.2  Schema and Type Checking ---")

expected_types <- list(
  indicator_id = "character",
  indicator    = "character",
  iso3         = "character",
  country      = "character",
  year         = "integer",
  value        = "numeric"
)

schema_df <- data.frame(
  column        = names(expected_types),
  expected_type = unlist(expected_types),
  actual_type   = sapply(names(expected_types), function(col) {
    if (col %in% names(df)) class(df[[col]])[1] else "MISSING"
  }),
  present       = names(expected_types) %in% names(df),
  stringsAsFactors = FALSE
)
schema_df$type_ok <- schema_df$expected_type == schema_df$actual_type

cat("\n[Schema Check]\n")
print(schema_df)
qa_log("Type mismatches:", sum(!schema_df$type_ok))

if (any(!schema_df$present)) {
  stop("Missing columns: ",
       paste(schema_df$column[!schema_df$present], collapse = ", "))
}

addWorksheet(wb, "1 - Schema Check")
writeData(wb, "1 - Schema Check", schema_df)
style_header(wb, "1 - Schema Check", ncol(schema_df))

# ── 1.3  Programmatic validation with {validate} ──────────────────────────────
qa_log("--- 1.3  Consistency Audit (validate) ---")

rules <- validator(
  no_na_indicator   = !is.na(indicator),
  no_na_country     = !is.na(country),
  no_na_iso3        = !is.na(iso3),
  no_na_year        = !is.na(year),
  no_na_value       = !is.na(value),
  value_finite      = is.finite(value),
  year_in_range     = year >= 1990 & year <= 2030,
  iso3_is_3chars    = nchar(trimws(iso3)) == 3,
  country_not_empty = nchar(trimws(country)) > 0,
  value_non_neg     = value >= 0,
  pct_max_105       = if (grepl("%", indicator)) value <= 105 else TRUE
)

cf           <- confront(df, rules)
val_summary  <- summary(cf)

cat("\n[validate Rules Summary]\n")
print(val_summary)

fails <- violating(df, cf)
qa_log("Records violating at least one rule:", nrow(fails))

addWorksheet(wb, "1 - Validation Rules")
writeData(wb, "1 - Validation Rules", val_summary)
style_header(wb, "1 - Validation Rules", ncol(val_summary))

addWorksheet(wb, "1 - Validation Failures")
if (nrow(fails) > 0) {
  writeData(wb, "1 - Validation Failures", fails)
  style_header(wb, "1 - Validation Failures", ncol(fails))
  write.csv(fails, file.path(QA_DIR, "validation_failures.csv"), row.names = FALSE)
} else {
  writeData(wb, "1 - Validation Failures",
            data.frame(Result = "ALL RULES PASSED — No violations found."))
}

# ── 1.4  Duplicate detection ──────────────────────────────────────────────────
qa_log("--- 1.4  Duplicate Detection ---")

dupes <- df |>
  group_by(indicator_id, country, year) |>
  filter(n() > 1) |>
  ungroup()

qa_log("Duplicate rows (same indicator + country + year):", nrow(dupes))

addWorksheet(wb, "1 - Duplicates")
if (nrow(dupes) > 0) {
  writeData(wb, "1 - Duplicates", dupes)
  style_header(wb, "1 - Duplicates", ncol(dupes))
  write.csv(dupes, file.path(QA_DIR, "duplicates.csv"), row.names = FALSE)
} else {
  writeData(wb, "1 - Duplicates",
            data.frame(Result = "No duplicates found."))
}

# ── 1.5  Missing value profile ────────────────────────────────────────────────
qa_log("--- 1.5  Missing Value Profile ---")

na_profile <- data.frame(
  column   = names(df),
  na_count = sapply(df, function(x) sum(is.na(x))),
  na_pct   = round(sapply(df, function(x) mean(is.na(x)) * 100), 2),
  stringsAsFactors = FALSE
)

cat("\n[NA Profile]\n")
print(na_profile)
qa_log("Total NAs:", sum(na_profile$na_count))

addWorksheet(wb, "1 - NA Profile")
writeData(wb, "1 - NA Profile", na_profile)
style_header(wb, "1 - NA Profile", ncol(na_profile))

# ── 1.6  Outlier detection (3 x IQR, per indicator) ──────────────────────────
qa_log("--- 1.6  Outlier Detection (3xIQR per indicator) ---")

outliers <- df |>
  group_by(indicator) |>
  mutate(
    q1     = quantile(value, 0.25, na.rm = TRUE),
    q3     = quantile(value, 0.75, na.rm = TRUE),
    iqr    = q3 - q1,
    is_out = value < (q1 - 3 * iqr) | value > (q3 + 3 * iqr)
  ) |>
  filter(is_out) |>
  select(indicator, country, iso3, year, value, q1, q3) |>
  ungroup()

qa_log("Extreme outliers detected:", nrow(outliers), "across",
       length(unique(outliers$indicator)), "indicators")

addWorksheet(wb, "1 - Outliers (3xIQR)")
if (nrow(outliers) > 0) {
  writeData(wb, "1 - Outliers (3xIQR)", outliers)
  style_header(wb, "1 - Outliers (3xIQR)", ncol(outliers))
  write.csv(outliers, file.path(QA_DIR, "outliers.csv"), row.names = FALSE)
} else {
  writeData(wb, "1 - Outliers (3xIQR)",
            data.frame(Result = "No extreme outliers detected (3xIQR threshold)."))
}

# ── 1.7  Coverage report ──────────────────────────────────────────────────────
qa_log("--- 1.7  Coverage Report ---")

ind_coverage <- df |>
  group_by(indicator) |>
  summarise(
    n_records   = n(),
    n_countries = n_distinct(country),
    year_min    = min(year, na.rm = TRUE),
    year_max    = max(year, na.rm = TRUE),
    pct_missing = round(mean(is.na(value)) * 100, 1),
    .groups     = "drop"
  ) |>
  arrange(n_countries)

addWorksheet(wb, "1 - Indicator Coverage")
writeData(wb, "1 - Indicator Coverage", ind_coverage)
style_header(wb, "1 - Indicator Coverage", ncol(ind_coverage))
setColWidths(wb, "1 - Indicator Coverage", cols = 1, widths = 65)

qa_log("Indicators with data:", nrow(ind_coverage))

# ── 1.8  Statistical cross-referencing → Excel ────────────────────────────────
qa_log("--- 1.8  Statistical Cross-Referencing (Excel) ---")

# These are the 6 featured KPI indicators used in the Overview tab
FEATURED_INDS <- c(
  "Life expectancy at birth (years)",
  "Infant mortality rate (1 000 lb)",
  "Estimated maternal mortality ratio (100 000 lb)",
  "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)",
  "Tuberculosis incidence rate (100 000 pop)",
  "Density of medical doctors (10 000 pop)"
)

# Random sample of 8 countries for manual verification (seed = 42 → reproducible)
set.seed(42)
sample_countries <- sample(unique(df$country), 8)
qa_log("Cross-reference sample countries:",
       paste(sample_countries, collapse = ", "))

# Latest value per country per featured indicator (mirrors app.R KPI logic)
cross_ref <- df |>
  filter(
    indicator %in% FEATURED_INDS,
    country   %in% sample_countries,
    !is.na(value)
  ) |>
  group_by(country, iso3, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  arrange(indicator, country)

# KPI median computation — mirrors make_kpi() logic in server
kpi_medians <- df |>
  filter(indicator %in% FEATURED_INDS, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  group_by(indicator) |>
  summarise(
    n_countries  = n_distinct(country),
    median_value = round(median(value, na.rm = TRUE), 3),
    mean_value   = round(mean(value, na.rm = TRUE), 3),
    sd_value     = round(sd(value, na.rm = TRUE), 3),
    min_value    = round(min(value, na.rm = TRUE), 3),
    max_value    = round(max(value, na.rm = TRUE), 3),
    latest_year  = max(year, na.rm = TRUE),
    .groups      = "drop"
  )

# Trend slice 2010-2023 (verify Trend Explorer lines)
trend_slice <- df |>
  filter(
    indicator %in% FEATURED_INDS[1:3],
    country   %in% sample_countries,
    year      %in% 2010:2023,
    !is.na(value)
  ) |>
  arrange(indicator, country, year)

# GDP vs Life Expectancy join (verify scatter plot)
gdp_le <- {
  gdp <- df |>
    filter(
      indicator == "Gross domestic product (US$ per capita), current international (PPP-adjusted)",
      !is.na(value)
    ) |>
    group_by(country) |>
    slice_max(year, n = 1) |>
    ungroup() |>
    select(country, gdp = value, gdp_year = year)
  le <- df |>
    filter(indicator == "Life expectancy at birth (years)", !is.na(value)) |>
    group_by(country) |>
    slice_max(year, n = 1) |>
    ungroup() |>
    select(country, life_exp = value, le_year = year)
  inner_join(gdp, le, by = "country") |> arrange(desc(gdp))
}

# Write README sheet first
addWorksheet(wb, "README")
readme_df <- data.frame(
  Step = c(
    "1. Sample Countries",
    "2. Cross-Reference Sample",
    "3. KPI Medians (Overview)",
    "4. Trend Slice 2010-2023",
    "5. GDP vs Life Expectancy",
    "Usage"
  ),
  Description = c(
    paste("8 randomly sampled countries (seed=42):",
          paste(sample_countries, collapse = ", ")),
    "Latest value per sampled country per featured indicator. Compare to dashboard KPI cards.",
    "Median/mean/SD/min/max for each featured KPI across ALL countries. Compare to Overview tab.",
    "Year-by-year values for Life Exp, Infant Mort, Maternal Mort. Verify Trend Explorer lines.",
    "GDP per capita joined to Life Expectancy for all countries. Verify Health System scatter plot.",
    paste0("Cross-reference each sheet against the live dashboard.",
           " Any divergence (>0.05) indicates a logic error in app.R or a data issue.")
  ),
  stringsAsFactors = FALSE
)
writeData(wb, "README", readme_df)
style_header(wb, "README", ncol(readme_df))
setColWidths(wb, "README", cols = 1:2, widths = c(25, 90))

addWorksheet(wb, "Cross-Reference Sample")
writeData(wb, "Cross-Reference Sample", cross_ref)
style_header(wb, "Cross-Reference Sample", ncol(cross_ref))
setColWidths(wb, "Cross-Reference Sample", cols = 3, widths = 65)

addWorksheet(wb, "KPI Medians (Overview)")
writeData(wb, "KPI Medians (Overview)", kpi_medians)
style_header(wb, "KPI Medians (Overview)", ncol(kpi_medians))
setColWidths(wb, "KPI Medians (Overview)", cols = 1, widths = 65)

addWorksheet(wb, "Trend Slice 2010-2023")
writeData(wb, "Trend Slice 2010-2023", trend_slice)
style_header(wb, "Trend Slice 2010-2023", ncol(trend_slice))
setColWidths(wb, "Trend Slice 2010-2023", cols = 3, widths = 65)

addWorksheet(wb, "GDP vs Life Expectancy")
writeData(wb, "GDP vs Life Expectancy", gdp_le)
style_header(wb, "GDP vs Life Expectancy", ncol(gdp_le))

saveWorkbook(wb, XLSX_FILE, overwrite = TRUE)
qa_log("Excel report (Sections 1 + cross-reference) saved.")

# ==============================================================================
# SECTION 2 — AUTOMATED UNIT TESTING
# ==============================================================================
qa_log("--- SECTION 2: Unit Testing ---")

# Re-declare helper functions exactly as coded in app.R (isolated for testing)
latest_val <- function(data, indicator, country_filter = NULL) {
  d <- data |> filter(indicator == !!indicator)
  if (!is.null(country_filter)) d <- d |> filter(country %in% country_filter)
  d |> filter(!is.na(value)) |>
    arrange(desc(year)) |> slice(1) |>
    pull(value)
}

latest_year <- function(data, indicator) {
  data |> filter(indicator == !!indicator, !is.na(value)) |>
    pull(year) |> max(na.rm = TRUE)
}

latest_country <- function(data, ind) {
  data |>
    filter(indicator == ind, !is.na(value)) |>
    group_by(country, iso3) |>
    slice_max(year, n = 1) |>
    ungroup()
}

# ── Test runner ────────────────────────────────────────────────────────────────
run_test <- function(name, expr) {
  result <- tryCatch({
    expr
    list(name = name, status = "PASS", message = "")
  }, error = function(e) {
    list(name = name, status = "FAIL", message = conditionMessage(e))
  })
  cat(sprintf("  [%s] %s%s\n",
              result$status, name,
              if (nchar(result$message) > 0) paste0(" — ", result$message) else ""))
  result
}

cat("\n=== Unit Tests ===\n")
test_list <- list()

# --- latest_val() ---
test_list[[1]] <- run_test("latest_val: returns numeric scalar for valid indicator", {
  v <- latest_val(df, "Life expectancy at birth (years)")
  stopifnot(is.numeric(v), length(v) == 1, v > 0)
})

test_list[[2]] <- run_test("latest_val: country_filter returns country-specific value", {
  v_bra <- latest_val(df, "Life expectancy at birth (years)", "Brazil")
  stopifnot(is.numeric(v_bra), length(v_bra) == 1)
})

test_list[[3]] <- run_test("latest_val: unknown indicator returns empty (length 0)", {
  v <- latest_val(df, "NONEXISTENT_INDICATOR_XYZ")
  stopifnot(length(v) == 0)
})

test_list[[4]] <- run_test("latest_val: unknown country returns empty (length 0)", {
  v <- latest_val(df, "Life expectancy at birth (years)", "CountryXYZ_000")
  stopifnot(length(v) == 0)
})

test_list[[5]] <- run_test("latest_val: boundary — single-row dataframe", {
  single <- df |>
    filter(indicator == "Life expectancy at birth (years)",
           country   == "Brazil") |>
    slice(1)
  v <- latest_val(single, "Life expectancy at birth (years)")
  stopifnot(is.numeric(v), length(v) == 1)
})

test_list[[6]] <- run_test("latest_val: boundary — empty dataframe returns length 0", {
  v <- latest_val(df[0, ], "Life expectancy at birth (years)")
  stopifnot(length(v) == 0)
})

# --- latest_year() ---
test_list[[7]] <- run_test("latest_year: returns plausible year integer", {
  yr <- latest_year(df, "Life expectancy at birth (years)")
  stopifnot(is.numeric(yr), yr >= 1990, yr <= 2030)
})

test_list[[8]] <- run_test("latest_year: unknown indicator returns -Inf or NA", {
  yr <- suppressWarnings(latest_year(df, "NONEXISTENT"))
  stopifnot(is.na(yr) || yr == -Inf)
})

test_list[[9]] <- run_test("latest_year: all-NA values returns -Inf or NA", {
  all_na <- df |>
    filter(indicator == "Life expectancy at birth (years)") |>
    mutate(value = NA_real_)
  yr <- suppressWarnings(latest_year(all_na, "Life expectancy at birth (years)"))
  stopifnot(is.na(yr) || yr == -Inf)
})

# --- latest_country() ---
test_list[[10]] <- run_test("latest_country: one row per country (no duplicates)", {
  d    <- latest_country(df, "Life expectancy at birth (years)")
  duped <- d |> group_by(country) |> filter(n() > 1) |> nrow()
  stopifnot(duped == 0)
})

test_list[[11]] <- run_test("latest_country: required columns present", {
  d <- latest_country(df, "Life expectancy at birth (years)")
  stopifnot(all(c("country", "iso3", "year", "value") %in% names(d)))
})

test_list[[12]] <- run_test("latest_country: boundary — empty input returns 0 rows", {
  d <- latest_country(df[0, ], "Life expectancy at birth (years)")
  stopifnot(nrow(d) == 0)
})

test_list[[13]] <- run_test("latest_country: values are the maximum year per country", {
  d      <- latest_country(df, "Life expectancy at birth (years)")
  all_yr <- df |>
    filter(indicator == "Life expectancy at birth (years)", !is.na(value)) |>
    group_by(country) |>
    summarise(max_yr = max(year), .groups = "drop")
  joined <- left_join(d, all_yr, by = "country")
  stopifnot(all(joined$year == joined$max_yr, na.rm = TRUE))
})

# --- Data pipeline integrity ---
test_list[[14]] <- run_test("Data: no negative values in cleaned dataset", {
  neg <- df |> filter(value < 0)
  stopifnot(nrow(neg) == 0)
})

test_list[[15]] <- run_test("Data: year column is integer type", {
  stopifnot(is.integer(df$year))
})

test_list[[16]] <- run_test("Data: value column is numeric", {
  stopifnot(is.numeric(df$value))
})

test_list[[17]] <- run_test("Data: iso3 codes are exactly 3 characters", {
  bad <- df |> filter(nchar(trimws(iso3)) != 3)
  stopifnot(nrow(bad) == 0)
})

test_list[[18]] <- run_test("Data: no whitespace-only country names", {
  bad <- df |> filter(trimws(country) == "")
  stopifnot(nrow(bad) == 0)
})

test_list[[19]] <- run_test("Data: percentage indicators do not exceed 105", {
  pct <- df |> filter(grepl("%", indicator), value > 105)
  stopifnot(nrow(pct) == 0)
})

test_list[[20]] <- run_test("Data: all featured KPIs exist in dataset", {
  missing <- setdiff(FEATURED_INDS, unique(df$indicator))
  stopifnot(length(missing) == 0)
})

# --- Country groups ---
COUNTRY_GROUPS_QA <- list(
  "North America"   = c("Bermuda","Canada","Mexico","United States of America"),
  "Central America" = c("Belize","Costa Rica","El Salvador","Guatemala",
                         "Honduras","Nicaragua","Panama"),
  "Caribbean"       = c("Anguilla","Antigua and Barbuda","Aruba","Bahamas",
                         "Barbados","Cayman Islands","Cuba","Cura\u00e7ao","Dominica",
                         "Dominican Republic","Grenada","Guadeloupe","Haiti",
                         "Jamaica","Martinique","Montserrat","Puerto Rico",
                         "Saint Kitts and Nevis","Saint Lucia",
                         "Saint Vincent and the Grenadines",
                         "Sint Maarten (Dutch part)","Trinidad and Tobago",
                         "Turks and Caicos Islands","Virgin Islands (British)",
                         "Virgin Islands (U.S.)"),
  "South America"   = c("Argentina","Bolivia (the Plurinational State of)",
                         "Brazil","Chile","Colombia","Ecuador","French Guiana",
                         "Guyana","Paraguay","Peru","Suriname","Uruguay",
                         "Venezuela (Bolivarian Republic of)")
)
all_grouped <- unlist(COUNTRY_GROUPS_QA, use.names = FALSE)

test_list[[21]] <- run_test("Country groups: no country assigned to multiple regions", {
  duped_regions <- all_grouped[duplicated(all_grouped)]
  stopifnot(length(duped_regions) == 0)
})

test_list[[22]] <- run_test("Country groups: covers all 4 expected regions", {
  stopifnot(length(COUNTRY_GROUPS_QA) == 4)
  stopifnot(all(c("North America","Central America","Caribbean","South America")
                %in% names(COUNTRY_GROUPS_QA)))
})

# --- KPI median consistency (app logic re-check) ---
test_list[[23]] <- run_test("KPI logic: median life expectancy between 50 and 90 years", {
  med <- df |>
    filter(indicator == "Life expectancy at birth (years)", !is.na(value)) |>
    group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
    summarise(m = median(value, na.rm = TRUE)) |>
    pull(m)
  stopifnot(med >= 50, med <= 90)
})

test_list[[24]] <- run_test("KPI logic: infant mortality median is positive", {
  med <- df |>
    filter(indicator == "Infant mortality rate (1 000 lb)", !is.na(value)) |>
    group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
    summarise(m = median(value, na.rm = TRUE)) |>
    pull(m)
  stopifnot(med > 0)
})

test_list[[25]] <- run_test("KPI logic: doctor density median > 0", {
  med <- df |>
    filter(indicator == "Density of medical doctors (10 000 pop)", !is.na(value)) |>
    group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
    summarise(m = median(value, na.rm = TRUE)) |>
    pull(m)
  stopifnot(med > 0)
})

# ── Test summary ───────────────────────────────────────────────────────────────
test_df <- do.call(rbind, lapply(test_list, as.data.frame,
                                  stringsAsFactors = FALSE))
n_pass <- sum(test_df$status == "PASS")
n_fail <- sum(test_df$status == "FAIL")

cat(sprintf("\n=== Unit Test Summary: %d PASS | %d FAIL (of %d) ===\n",
            n_pass, n_fail, nrow(test_df)))
if (n_fail > 0) {
  cat("FAILED:\n")
  print(test_df[test_df$status == "FAIL", c("name","message")])
}

addWorksheet(wb, "2 - Unit Test Results")
writeData(wb, "2 - Unit Test Results", test_df)
style_header(wb, "2 - Unit Test Results", ncol(test_df))
setColWidths(wb, "2 - Unit Test Results", cols = 1:3,
             widths = c(60, 8, 60))
saveWorkbook(wb, XLSX_FILE, overwrite = TRUE)
qa_log("Unit test results appended to Excel.",
       paste0(n_pass, " PASS / ", n_fail, " FAIL"))

# ==============================================================================
# SECTION 3 — REACTIVE LOGIC AND UI TESTING (shinytest2)
# ==============================================================================
qa_log("--- SECTION 3: Shiny UI / Reactive Testing (shinytest2) ---")
cat("\n")

if (requireNamespace("shinytest2", quietly = TRUE)) {
  library(shinytest2)
  cat("[shinytest2] Launching headless browser...\n")

  shiny_results <- list()
  run_shiny_test <- function(name, expr) {
    result <- tryCatch({
      expr
      list(name = name, status = "PASS", message = "")
    }, error = function(e) {
      list(name = name, status = "FAIL", message = conditionMessage(e))
    })
    cat(sprintf("  [%s] %s%s\n", result$status, name,
                if (nchar(result$message) > 0) paste0(" — ", result$message) else ""))
    result
  }

  app <- tryCatch(
    AppDriver$new(".", name = "paho-qa", height = 900, width = 1400,
                  timeout = 45000),
    error = function(e) {
      qa_log("shinytest2: could not launch app —", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(app)) {
    Sys.sleep(3)  # Allow full render

    shiny_results[[1]] <- run_shiny_test("App loads — title element exists", {
      ttl <- app$get_title()
      stopifnot(!is.null(ttl))
    })

    shiny_results[[2]] <- run_shiny_test("Year slider accepts 2010-2023 range", {
      app$set_inputs(s_years = c(2010, 2023))
      Sys.sleep(1)
    })

    shiny_results[[3]] <- run_shiny_test("Country filter reduced to Brazil — no crash", {
      app$set_inputs(s_countries = "Brazil")
      Sys.sleep(2)
    })

    shiny_results[[4]] <- run_shiny_test("Map year changed to 2020", {
      app$set_inputs(s_map_year = "2020")
      Sys.sleep(1)
    })

    shiny_results[[5]] <- run_shiny_test("Map indicator switched to Infant Mortality", {
      app$set_inputs(map_indicator = "Infant mortality rate (1 000 lb)")
      Sys.sleep(1.5)
    })

    shiny_results[[6]] <- run_shiny_test("Trend indicator changed to Infant Mortality", {
      app$set_inputs(trend_ind = "Infant mortality rate (1 000 lb)")
      Sys.sleep(1)
    })

    shiny_results[[7]] <- run_shiny_test("Sex split toggle TRUE — no crash", {
      app$set_inputs(trend_sex = TRUE)
      Sys.sleep(1)
    })

    shiny_results[[8]] <- run_shiny_test("Sex split toggle FALSE — reverts cleanly", {
      app$set_inputs(trend_sex = FALSE)
      Sys.sleep(1)
    })

    shiny_results[[9]] <- run_shiny_test("Country profile switched to Argentina", {
      app$set_inputs(profile_country = "Argentina")
      Sys.sleep(2)
    })

    shiny_results[[10]] <- run_shiny_test("Disease selector changed to Malaria cases", {
      app$set_inputs(disease_sel = "Malaria cases")
      Sys.sleep(1)
    })

    shiny_results[[11]] <- run_shiny_test("Restore all countries — no crash", {
      all_countries <- sort(unique(df$country))
      app$set_inputs(s_countries = all_countries)
      Sys.sleep(2)
    })

    # Save screenshot
    screenshot_path <- file.path(QA_DIR, "shinytest2_screenshot.png")
    tryCatch({
      app$get_screenshot(screenshot_path)
      cat(sprintf("  [INFO] Screenshot saved: %s\n", screenshot_path))
    }, error = function(e) NULL)

    app$stop()
    qa_log("shinytest2 tests completed.")

    shiny_df <- do.call(rbind, lapply(shiny_results, as.data.frame,
                                       stringsAsFactors = FALSE))
    addWorksheet(wb, "3 - Shiny UI Tests")
    writeData(wb, "3 - Shiny UI Tests", shiny_df)
    style_header(wb, "3 - Shiny UI Tests", ncol(shiny_df))
    setColWidths(wb, "3 - Shiny UI Tests", cols = 1:3, widths = c(55, 8, 60))
    saveWorkbook(wb, XLSX_FILE, overwrite = TRUE)
    qa_log("shinytest2 results appended to Excel.")

  }
} else {
  qa_log("shinytest2 not installed — skipping Section 3.")
  cat("  Install with: install.packages('shinytest2')\n")
  cat("  Then run: shinytest2::install_chromote_browser()\n")
}

# ==============================================================================
# SECTION 4 — PERFORMANCE AND LOAD TESTING
# ==============================================================================
qa_log("--- SECTION 4: Performance Profiling ---")
cat("\n")

# ── 4.1  Benchmark key reactive operations ────────────────────────────────────
cat("[Benchmark] Timing key operations...\n")

benchmark_ops <- list(
  "readRDS(paho_clean.rds)" = function() {
    readRDS("app/paho_clean.rds")
  },
  "filter + slice_max: Life Expectancy latest per country" = function() {
    df |>
      filter(indicator == "Life expectancy at birth (years)", !is.na(value)) |>
      group_by(country, iso3) |>
      slice_max(year, n = 1) |>
      ungroup()
  },
  "filter + slice_max: Infant Mortality latest per country" = function() {
    df |>
      filter(indicator == "Infant mortality rate (1 000 lb)", !is.na(value)) |>
      group_by(country, iso3) |>
      slice_max(year, n = 1) |>
      ungroup()
  },
  "inner_join: GDP per capita vs Life Expectancy" = function() {
    gdp <- df |>
      filter(
        indicator == paste0("Gross domestic product (US$ per capita),",
                            " current international (PPP-adjusted)"),
        !is.na(value)
      ) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      select(country, gdp = value)
    le <- df |>
      filter(indicator == "Life expectancy at birth (years)", !is.na(value)) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      select(country, le = value)
    inner_join(gdp, le, by = "country")
  },
  "KPI median (all 6 featured indicators)" = function() {
    df |>
      filter(indicator %in% FEATURED_INDS, !is.na(value)) |>
      group_by(country, indicator) |>
      slice_max(year, n = 1) |>
      ungroup() |>
      group_by(indicator) |>
      summarise(med = median(value, na.rm = TRUE), .groups = "drop")
  },
  "df_f() simulation: filter countries + year range (all data)" = function() {
    df |>
      filter(
        country %in% unique(df$country),
        year >= 2000,
        year <= 2023
      )
  }
)

timing_df <- data.frame(
  operation     = names(benchmark_ops),
  elapsed_secs  = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(benchmark_ops)) {
  t <- system.time(benchmark_ops[[i]]())[3]
  timing_df$elapsed_secs[i] <- round(t, 4)
  cat(sprintf("  %.4f s  —  %s\n", t, names(benchmark_ops)[i]))
}

timing_df$performance <- ifelse(
  timing_df$elapsed_secs < 0.05, "Good",
  ifelse(timing_df$elapsed_secs < 0.5, "Acceptable", "Review")
)

addWorksheet(wb, "4 - Performance Timings")
writeData(wb, "4 - Performance Timings", timing_df)
style_header(wb, "4 - Performance Timings", ncol(timing_df))
setColWidths(wb, "4 - Performance Timings", cols = 1, widths = 65)

# ── 4.2  profvis HTML report (optional — requires CSV present) ─────────────────
if (requireNamespace("profvis", quietly = TRUE)) {
  library(profvis)

  cat("\n[profvis] Profiling reactive computation bundle...\n")

  prof <- profvis({
    # Simulate all 6 KPI computations (make_kpi loop in server)
    for (feat_ind in FEATURED_INDS) {
      invisible(
        df |>
          filter(indicator == feat_ind, !is.na(value)) |>
          group_by(country) |>
          slice_max(year, n = 1) |>
          ungroup() |>
          summarise(med = median(value, na.rm = TRUE))
      )
    }
    # Simulate latest_country for map
    invisible(
      df |>
        filter(indicator == "Life expectancy at birth (years)", !is.na(value)) |>
        group_by(country, iso3) |>
        slice_max(year, n = 1) |>
        ungroup()
    )
    # Simulate disease trend
    invisible(
      df |>
        filter(indicator == "Dengue cases", !is.na(value)) |>
        arrange(country, year)
    )
  }, interval = 0.005)

  if (requireNamespace("htmlwidgets", quietly = TRUE)) {
    htmlwidgets::saveWidget(
      prof,
      file.path(QA_DIR, "profvis_reactive.html"),
      selfcontained = TRUE
    )
    qa_log("profvis report saved: qa_output/profvis_reactive.html")
  } else {
    print(prof)
    qa_log("htmlwidgets not installed — profvis printed to console only.")
  }
} else {
  qa_log("profvis not installed — skipping profiling. install.packages('profvis')")
}

saveWorkbook(wb, XLSX_FILE, overwrite = TRUE)
qa_log("Performance results appended to Excel.")

# ==============================================================================
# SECTION 5 — STAKEHOLDER ALIGNMENT: UAT CHECKLIST + SPRINT BACKLOG
# ==============================================================================
qa_log("--- SECTION 5: UAT Checklist and Sprint Backlog ---")

uat_df <- data.frame(
  ID       = paste0("UAT-", sprintf("%02d", 1:26)),
  Tab      = c(
    rep("Overview",         5),
    rep("Trend Explorer",   4),
    rep("Country Profile",  4),
    rep("Disease Burden",   4),
    rep("Maternal & Child", 5),
    rep("Health System",    4)
  ),
  Test_Case = c(
    # Overview (5)
    "KPI cards display correct median values for all 6 featured indicators",
    "Choropleth map renders for all Americas countries with red-amber-green scale",
    "Changing Map Year selector (e.g., 2010 vs 2023) visibly updates the map",
    "Top/Bottom bar shows exactly 5 green (highest) and 5 red (lowest) countries",
    "Sidebar Countries filter updates all KPI cards to reflect selected subset",
    # Trend Explorer (4)
    "Line chart displays one distinct color line per selected country",
    "Sex split toggle adds '(female)' and '(male)' suffix lines to chart",
    "Data table shows only selected countries with latest year values, sorted desc",
    "Country Comparison bar chart reorders correctly by value",
    # Country Profile (4)
    "Switching profile country updates all 4 mini-charts and 6 KPI boxes",
    "KPI box shows 'N/A' when no data exists for a country-indicator pair",
    "Year range slider restricts profile chart data to selected range",
    "All 4 charts render without 'No data' message for major countries",
    # Disease Burden (4)
    "Communicable vs NCD grouped bar renders with 2 bars per country",
    "Dengue, Malaria, Cholera, Measles, TB selectors each update the trend chart",
    "HIV/AIDS rate chart shows multi-country colored lines",
    "TB incidence chart matches cross-reference Excel values within ±0.5",
    # Maternal & Child (5)
    "Under-5/Infant/Neonatal chart uses 3 distinct line styles (solid/dot/dash)",
    "Maternal Mortality trend is visibly declining post-2000 for most countries",
    "DTP3 Immunization coverage values do not exceed 100% on y-axis",
    "Births attended by skilled personnel values align with Excel cross-reference",
    "Antenatal care >=4 visits chart shows plausible 0-100% range",
    # Health System (4)
    "Medical Doctors density bar matches Excel cross-reference within ±0.1",
    "Health expenditure grouped bar shows Public (% GDP) vs OOP for all countries",
    "Hospital beds chart sorted ascending by value, correct y-axis label",
    "GDP vs Life Expectancy scatter tooltip shows country name, GDP, and LE on hover"
  ),
  Expected_Result = c(
    "Values within ±0.05 of 'KPI Medians (Overview)' Excel sheet",
    "All 49 countries colored, no blank country shapes in Americas region",
    "Color distribution shifts when year changes",
    "5 green bars on right, 5 red bars on left, center gap visible",
    "Cards recalculate median for subset only",
    "Legend shows each country, lines non-overlapping",
    "Additional lines appear with correct sex suffix",
    "Values match 'Cross-Reference Sample' sheet for sampled countries",
    "Bars ordered left (low) to right (high)",
    "KPI values match 'Cross-Reference Sample' sheet for selected country",
    "Gray 'N/A' text shown instead of numeric value",
    "X-axis range in chart matches slider values",
    "Charts show data for Brazil, Mexico, Colombia, Argentina",
    "Two colored bars per country, legend shows Communicable / NCD",
    "Chart refreshes with new disease data and updated y-axis label",
    "Each country has a distinct color, hover shows rate per 100,000",
    "Rate values within ±0.5 of Excel cross-reference for sampled countries",
    "Three lines per country, one per mortality type, legend readable",
    "Trend should decline from ~2000 to ~2023 for most Americas countries",
    "Y-axis max ≤ 100, no value exceeds axis",
    "Values within ±0.5 of PAHO source in Excel",
    "All bars in 0-100 range, plausible values for each country",
    "Values within ±0.1 of 'Cross-Reference Sample' Excel sheet",
    "Two color-coded groups per country, legend visible",
    "Bars sorted ascending by value, unit is 'per 1,000 pop'",
    "Tooltip fires on hover, showing all three fields"
  ),
  Priority = c(
    rep("High",   5),
    rep("High",   4),
    rep("Medium", 4),
    rep("Medium", 4),
    rep("Medium", 5),
    rep("High",   4)
  ),
  Status = rep("Pending", 26),
  Tester = rep("", 26),
  Notes  = rep("", 26),
  stringsAsFactors = FALSE
)

addWorksheet(wb, "5 - UAT Checklist")
writeData(wb, "5 - UAT Checklist", uat_df)
hs_uat <- createStyle(
  fgFill         = "#003366",
  fontColour     = "#FFFFFF",
  textDecoration = "bold",
  halign         = "center"
)
addStyle(wb, "5 - UAT Checklist", hs_uat,
         rows = 1, cols = 1:ncol(uat_df), gridExpand = TRUE)
setColWidths(wb, "5 - UAT Checklist",
             cols = 1:ncol(uat_df),
             widths = c(8, 18, 65, 65, 10, 10, 15, 30))

# Conditional formatting: colour Priority column
high_style <- createStyle(fgFill = "#FFDDC1", fontColour = "#7B0000")
med_style  <- createStyle(fgFill = "#FFF3CD", fontColour = "#7D4E00")
for (row_i in seq_len(nrow(uat_df))) {
  style <- if (uat_df$Priority[row_i] == "High") high_style else med_style
  addStyle(wb, "5 - UAT Checklist", style,
           rows = row_i + 1, cols = 5, stack = TRUE)
}

# ── Sprint backlog from QA findings ───────────────────────────────────────────
sprint_df <- data.frame(
  ID       = paste0("QA-", sprintf("%02d", 1:10)),
  Category = c("Data","Data","Data","Performance","UI",
                "UI","Testing","Testing","Testing","Documentation"),
  Finding  = c(
    "Confirm pct indicators >105 are valid data, not cleaning errors",
    "Investigate extreme outliers flagged by 3xIQR vs PAHO official source",
    "Validate iso3 codes against ISO 3166-1 alpha-3 standard",
    "Measure app load time with profvis; confirm RDS caching reduces CSV re-parse",
    "Test dashboard at 1280x720 and 1920x1080 for label truncation",
    "Cross-browser test: verify map renders in Firefox and Safari",
    "Record shinytest2 snapshot baselines for regression detection",
    "Extend unit tests to cover profile_trend() and mch_trend() helpers",
    "Add boundary test: what happens when s_countries is set to empty list",
    "Share QA Excel report with stakeholders; document findings in project README"
  ),
  Priority = c("High","High","Medium","Medium","Medium",
                "Low","High","Medium","High","High"),
  Sprint   = c(1,1,2,2,1,2,2,3,1,1),
  Owner    = rep("Ezequiel Bassa", 10),
  Status   = rep("Open", 10),
  stringsAsFactors = FALSE
)

addWorksheet(wb, "5 - Sprint Backlog")
writeData(wb, "5 - Sprint Backlog", sprint_df)
style_header(wb, "5 - Sprint Backlog", ncol(sprint_df))
setColWidths(wb, "5 - Sprint Backlog",
             cols = 1:ncol(sprint_df),
             widths = c(8, 15, 70, 10, 8, 18, 10))

saveWorkbook(wb, XLSX_FILE, overwrite = TRUE)
qa_log("UAT Checklist and Sprint Backlog saved to Excel.")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================
cat("\n")
cat(strrep("=", 65), "\n")
cat("  PAHO Dashboard QA Audit — Final Summary\n")
cat(strrep("=", 65), "\n")
cat(sprintf("  Records audited   : %s\n",   format(nrow(df), big.mark=",")))
cat(sprintf("  Countries         : %d\n",   length(unique(df$country))))
cat(sprintf("  Indicators        : %d\n",   length(unique(df$indicator))))
cat(sprintf("  Duplicate rows    : %d\n",   nrow(dupes)))
cat(sprintf("  Validation fails  : %d\n",   nrow(fails)))
cat(sprintf("  Outliers (3xIQR)  : %d\n",   nrow(outliers)))
cat(sprintf("  Unit tests        : %d PASS / %d FAIL\n", n_pass, n_fail))
cat(sprintf("  UAT items pending : %d\n",   nrow(uat_df)))
cat(sprintf("  Sprint backlog    : %d items\n", nrow(sprint_df)))
cat(strrep("=", 65), "\n")
cat(sprintf("  Output folder     : %s/\n", QA_DIR))
cat(sprintf("  Excel report      : %s\n",  XLSX_FILE))
cat(sprintf("  QA log            : %s\n",  LOG_FILE))
cat(strrep("=", 65), "\n")

qa_log("=== QA Audit completed ===")
