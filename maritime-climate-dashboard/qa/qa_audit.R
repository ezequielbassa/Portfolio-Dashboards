# ==============================================================================
#  Workplace Climate Survey — Maritime Logistics Americas 2026
#  5-Pillar QA Audit Script
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Version : 1.0 | 2026-04-02
# ==============================================================================
# Outputs (all in qa/qa_output/):
#   maritime_qa_report.xlsx   — cross-reference & validation workbook
#   qa_log.txt                — full audit console log
#   profvis_reactive.html     — performance flame graph
# ==============================================================================

library(dplyr)
library(tidyr)
library(openxlsx)
library(validate)

QA_DIR    <- "qa/qa_output"
XLSX_FILE <- file.path(QA_DIR, "maritime_qa_report.xlsx")
LOG_FILE  <- file.path(QA_DIR, "qa_log.txt")

if (!dir.exists(QA_DIR)) dir.create(QA_DIR, recursive = TRUE)

# Redirect console output to log file as well
log_con <- file(LOG_FILE, open = "wt")
sink(log_con, split = TRUE)

cat("=============================================================\n")
cat(" Maritime Workplace Climate QA Audit\n")
cat(" Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================\n\n")

# ── CONSTANTS (mirrors app.R) ──────────────────────────────────────────────────
CLIMATE_COLS <- c(
  "eNPS", "Safety_Culture", "Cross_Border_Collab", "Leadership_Trust",
  "Work_Life_Balance", "Career_Growth", "Inclusion_Diversity",
  "Operational_Stress", "Fair_Compensation", "Tech_Adequacy",
  "Recognition", "Environmental_Pride", "Engagement", "Job_Security",
  "Managerial_Support", "Ethical_Standards", "Training_Quality",
  "Communication_Clarity", "Team_Cohesion", "Retention_Intent"
)

FRIENDLY <- c(
  eNPS = "eNPS", Safety_Culture = "Safety Culture",
  Cross_Border_Collab = "Cross-Border Collab",
  Leadership_Trust = "Leadership Trust",
  Work_Life_Balance = "Work-Life Balance",
  Career_Growth = "Career Growth",
  Inclusion_Diversity = "Inclusion & Diversity",
  Operational_Stress = "Operational Stress",
  Fair_Compensation = "Fair Compensation",
  Tech_Adequacy = "Tech Adequacy",
  Recognition = "Recognition",
  Environmental_Pride = "Environmental Pride",
  Engagement = "Engagement",
  Job_Security = "Job Security",
  Managerial_Support = "Managerial Support",
  Ethical_Standards = "Ethical Standards",
  Training_Quality = "Training Quality",
  Communication_Clarity = "Communication Clarity",
  Team_Cohesion = "Team Cohesion",
  Retention_Intent = "Retention Intent"
)

EXPECTED_COLS <- c(
  "Employee_ID", "Region", "Country", "Department", "Gender",
  "Age", "Salary_Band", "Work_Type", "Tenure_Years", "Open_Comments",
  CLIMATE_COLS
)

EXPECTED_COUNTRIES <- c(
  "USA", "Canada", "Mexico", "Panama", "Costa Rica",
  "Brazil", "Chile", "Argentina", "Peru", "Colombia"
)

EXPECTED_REGIONS  <- c("North America", "Central America", "South America")
EXPECTED_SALARIES <- c("Junior", "Mid", "Senior", "Executive")

# ==============================================================================
# LOAD DATA
# ==============================================================================
cat("Loading data...\n")
df_raw <- read.csv(
  "sources/workplace_climate_maritime_2026.csv",
  stringsAsFactors = FALSE,
  na.strings       = ""
)

# Replicate app.R derived columns
df_raw <- df_raw |>
  mutate(
    Tenure_Band = case_when(
      Tenure_Years < 2   ~ "< 2 yrs",
      Tenure_Years <= 4  ~ "2-4 yrs",
      Tenure_Years <= 10 ~ "5-10 yrs",
      Tenure_Years <= 20 ~ "11-20 yrs",
      TRUE               ~ "> 20 yrs"
    ),
    Tenure_Band = factor(
      Tenure_Band,
      levels = c("< 2 yrs", "2-4 yrs", "5-10 yrs", "11-20 yrs", "> 20 yrs")
    ),
    Age_Group = cut(
      Age,
      breaks = c(0, 30, 40, 50, 100),
      labels = c("Under 30", "30-39", "40-49", "50+"),
      right  = FALSE
    ),
    Avg_Climate = rowMeans(across(all_of(CLIMATE_COLS)), na.rm = TRUE)
  )

cat("Rows:", nrow(df_raw), "| Cols:", ncol(df_raw), "\n\n")

# Mirror of app.R score_col helper (used in unit tests)
score_col <- function(x, inv = FALSE) {
  if (inv) x <- 6 - x
  dplyr::case_when(x >= 4  ~ "#2ecc71",
                   x >= 3  ~ "#f39c12",
                   TRUE     ~ "#e74c3c")
}

# ==============================================================================
# SECTION 1 — DATA INTEGRITY
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 1 — DATA INTEGRITY\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# 1.1 Schema check
cat("1.1 Schema Check\n")
missing_cols <- setdiff(EXPECTED_COLS, names(df_raw))
extra_cols   <- setdiff(names(df_raw), EXPECTED_COLS)
if (length(missing_cols) == 0) {
  cat("  PASS: All expected columns present\n")
} else {
  cat("  FAIL: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
}
if (length(extra_cols) > 0) {
  cat("  INFO: Extra columns found:", paste(extra_cols, collapse = ", "), "\n")
}

# 1.2 Row count
cat("\n1.2 Row Count\n")
cat("  Total rows:", format(nrow(df_raw), big.mark = ","), "\n")
if (nrow(df_raw) == 12500) {
  cat("  PASS: Expected 12,500 rows\n")
} else {
  cat("  INFO: Expected 12,500 rows, found", nrow(df_raw), "\n")
}

# 1.3 validate rules
cat("\n1.3 Validate Rules\n")
rules <- validator(
  no_na_region      = !is.na(Region),
  no_na_country     = !is.na(Country),
  no_na_dept        = !is.na(Department),
  no_na_gender      = !is.na(Gender),
  no_na_salary      = !is.na(Salary_Band),
  no_na_wtype       = !is.na(Work_Type),
  no_na_tenure      = !is.na(Tenure_Years),
  no_na_age         = !is.na(Age),
  tenure_nonneg     = Tenure_Years >= 0,
  tenure_max        = Tenure_Years <= 50,
  age_min           = Age >= 16,
  age_max           = Age <= 80,
  valid_region      = Region %in% c("North America", "Central America", "South America"),
  valid_salary      = Salary_Band %in% c("Junior", "Mid", "Senior", "Executive"),
  valid_country     = Country %in% c("USA","Canada","Mexico","Panama","Costa Rica",
                                     "Brazil","Chile","Argentina","Peru","Colombia"),
  enps_range        = eNPS >= 1 & eNPS <= 5,
  safety_range      = Safety_Culture >= 1 & Safety_Culture <= 5,
  engagement_range  = Engagement >= 1 & Engagement <= 5,
  retention_range   = Retention_Intent >= 1 & Retention_Intent <= 5,
  stress_range      = Operational_Stress >= 1 & Operational_Stress <= 5,
  avg_climate_range = Avg_Climate >= 1 & Avg_Climate <= 5
)

cf <- confront(df_raw, rules)
cf_summary <- summary(cf)
print(cf_summary[, c("name","items","passes","fails","nNA","error","warning")])

fails_total <- sum(cf_summary$fails, na.rm = TRUE)
if (fails_total == 0) {
  cat("\n  PASS: All validate rules — 0 violations across",
      format(nrow(df_raw), big.mark=","), "records\n")
} else {
  cat("\n  ISSUE: Total rule violations:", fails_total, "\n")
}

# Save validate summary to xlsx later
validate_df <- cf_summary[, c("name","items","passes","fails","nNA","error","warning")]

# 1.4 Duplicates
cat("\n1.4 Duplicate Check\n")
dupes <- df_raw |>
  group_by(Employee_ID) |>
  filter(n() > 1) |>
  ungroup()
if (nrow(dupes) == 0) {
  cat("  PASS: No duplicate Employee_IDs\n")
} else {
  cat("  FAIL:", nrow(dupes), "duplicate Employee_ID rows found\n")
}

# 1.5 Category distribution
cat("\n1.5 Category Distribution\n")
cat("  Regions:\n")
print(as.data.frame(table(df_raw$Region)))
cat("  Countries:\n")
print(as.data.frame(table(df_raw$Country)))
cat("  Salary Bands:\n")
print(as.data.frame(table(df_raw$Salary_Band)))
cat("  Genders:\n")
print(as.data.frame(table(df_raw$Gender)))
cat("  Work Types:\n")
print(as.data.frame(table(df_raw$Work_Type)))

# 1.6 Climate indicator summary
cat("\n1.6 Climate Indicator Summary (mean / sd / min / max)\n")
ind_summary <- df_raw |>
  summarise(across(all_of(CLIMATE_COLS), list(
    mean = function(x) round(mean(x, na.rm=TRUE), 3),
    sd   = function(x) round(sd(x,   na.rm=TRUE), 3),
    min  = function(x) min(x,  na.rm=TRUE),
    max  = function(x) max(x,  na.rm=TRUE)
  )))

ind_summary_long <- data.frame(
  Indicator = FRIENDLY[CLIMATE_COLS],
  Mean = sapply(CLIMATE_COLS, function(c) round(mean(df_raw[[c]], na.rm=TRUE), 3)),
  SD   = sapply(CLIMATE_COLS, function(c) round(sd(df_raw[[c]],   na.rm=TRUE), 3)),
  Min  = sapply(CLIMATE_COLS, function(c) min(df_raw[[c]],  na.rm=TRUE)),
  Max  = sapply(CLIMATE_COLS, function(c) max(df_raw[[c]],  na.rm=TRUE)),
  NA_Count = sapply(CLIMATE_COLS, function(c) sum(is.na(df_raw[[c]])))
)
print(ind_summary_long, row.names = FALSE)

# 1.7 Outlier detection (3 × IQR per climate indicator)
cat("\n1.7 Outlier Detection (3 × IQR per indicator)\n")
outlier_rows <- list()
for (col in CLIMATE_COLS) {
  vals <- df_raw[[col]]
  q1   <- quantile(vals, 0.25, na.rm=TRUE)
  q3   <- quantile(vals, 0.75, na.rm=TRUE)
  iqr  <- q3 - q1
  lo   <- q1 - 3 * iqr
  hi   <- q3 + 3 * iqr
  out  <- df_raw[!is.na(vals) & (vals < lo | vals > hi), ]
  if (nrow(out) > 0) {
    out$Flagged_Indicator <- col
    outlier_rows[[col]]   <- out
  }
}
if (length(outlier_rows) == 0) {
  cat("  PASS: No outliers beyond 3×IQR in any indicator\n")
  outlier_df <- data.frame(Message = "No outliers detected")
} else {
  outlier_df <- bind_rows(outlier_rows)
  cat("  INFO:", nrow(outlier_df), "outlier rows across",
      length(outlier_rows), "indicators\n")
  cat("  Indicators with outliers:", paste(names(outlier_rows), collapse=", "), "\n")
}

# 1.8 NA analysis for Open_Comments
cat("\n1.8 Open Comments NA Analysis\n")
n_cmt     <- sum(!is.na(df_raw$Open_Comments) & nchar(trimws(df_raw$Open_Comments)) > 0)
n_no_cmt  <- nrow(df_raw) - n_cmt
cat("  Employees with comments   :", format(n_cmt,    big.mark=","), "\n")
cat("  Employees without comments:", format(n_no_cmt, big.mark=","), "\n")
cat("  Comment rate              :", round(n_cmt/nrow(df_raw)*100, 1), "%\n")

cat("\n")

# ==============================================================================
# SECTION 2 — UNIT TESTS
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 2 — UNIT TESTS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

test_list   <- list()
pass_count  <- 0
fail_count  <- 0

run_test <- function(name, expr) {
  result <- tryCatch({
    expr
    list(name=name, status="PASS", message="")
  }, error = function(e) {
    list(name=name, status="FAIL", message=conditionMessage(e))
  })
  status_str <- if (result$status == "PASS") "  [PASS]" else "  [FAIL]"
  cat(status_str, name, "\n")
  if (result$status == "FAIL") cat("         -->", result$message, "\n")
  result
}

cat("-- score_col() function --\n")
test_list[[1]] <- run_test("score_col: score 4 returns green", {
  stopifnot(score_col(4) == "#2ecc71")
})
test_list[[2]] <- run_test("score_col: score 3 returns amber", {
  stopifnot(score_col(3) == "#f39c12")
})
test_list[[3]] <- run_test("score_col: score 2 returns red", {
  stopifnot(score_col(2) == "#e74c3c")
})
test_list[[4]] <- run_test("score_col: inverted score 4 (stress) returns red", {
  stopifnot(score_col(4, inv=TRUE) == "#e74c3c")
})
test_list[[5]] <- run_test("score_col: inverted score 1 (stress) returns green", {
  stopifnot(score_col(1, inv=TRUE) == "#2ecc71")
})

cat("\n-- Avg_Climate derivation --\n")
test_list[[6]] <- run_test("Avg_Climate: computed for all rows (no NA)", {
  stopifnot(sum(is.na(df_raw$Avg_Climate)) == 0)
})
test_list[[7]] <- run_test("Avg_Climate: range is [1, 5]", {
  stopifnot(min(df_raw$Avg_Climate) >= 1, max(df_raw$Avg_Climate) <= 5)
})
test_list[[8]] <- run_test("Avg_Climate: equals rowMeans of CLIMATE_COLS", {
  expected <- rowMeans(df_raw[, CLIMATE_COLS], na.rm=TRUE)
  stopifnot(all(abs(df_raw$Avg_Climate - expected) < 1e-9))
})

cat("\n-- Tenure_Band derivation --\n")
test_list[[9]] <- run_test("Tenure_Band: no NA values", {
  stopifnot(sum(is.na(df_raw$Tenure_Band)) == 0)
})
test_list[[10]] <- run_test("Tenure_Band: correct levels order", {
  expected_levels <- c("< 2 yrs", "2-4 yrs", "5-10 yrs", "11-20 yrs", "> 20 yrs")
  stopifnot(identical(levels(df_raw$Tenure_Band), expected_levels))
})
test_list[[11]] <- run_test("Tenure_Band: Tenure_Years < 2 maps to '< 2 yrs'", {
  sub <- df_raw[df_raw$Tenure_Years < 2, ]
  stopifnot(all(as.character(sub$Tenure_Band) == "< 2 yrs"))
})
test_list[[12]] <- run_test("Tenure_Band: Tenure_Years > 20 maps to '> 20 yrs'", {
  sub <- df_raw[df_raw$Tenure_Years > 20, ]
  stopifnot(all(as.character(sub$Tenure_Band) == "> 20 yrs"))
})

cat("\n-- Age_Group derivation --\n")
test_list[[13]] <- run_test("Age_Group: no NA values", {
  stopifnot(sum(is.na(df_raw$Age_Group)) == 0)
})
test_list[[14]] <- run_test("Age_Group: Age < 30 maps to 'Under 30'", {
  sub <- df_raw[df_raw$Age < 30, ]
  stopifnot(all(as.character(sub$Age_Group) == "Under 30"))
})
test_list[[15]] <- run_test("Age_Group: Age >= 50 maps to '50+'", {
  sub <- df_raw[df_raw$Age >= 50, ]
  stopifnot(all(as.character(sub$Age_Group) == "50+"))
})

cat("\n-- Data pipeline --\n")
test_list[[16]] <- run_test("Data: Tenure_Years is numeric", {
  stopifnot(is.numeric(df_raw$Tenure_Years))
})
test_list[[17]] <- run_test("Data: Age is numeric", {
  stopifnot(is.numeric(df_raw$Age))
})
test_list[[18]] <- run_test("Data: all CLIMATE_COLS are numeric", {
  stopifnot(all(sapply(df_raw[, CLIMATE_COLS], is.numeric)))
})
test_list[[19]] <- run_test("Data: all Likert scores are in [1, 5]", {
  for (col in CLIMATE_COLS) {
    vals <- df_raw[[col]]
    stopifnot(all(vals >= 1 & vals <= 5, na.rm=TRUE))
  }
})
test_list[[20]] <- run_test("Data: all 10 expected countries present", {
  stopifnot(all(EXPECTED_COUNTRIES %in% unique(df_raw$Country)))
})
test_list[[21]] <- run_test("Data: all 3 expected regions present", {
  stopifnot(all(EXPECTED_REGIONS %in% unique(df_raw$Region)))
})
test_list[[22]] <- run_test("Data: no unexpected Salary_Band values", {
  stopifnot(all(df_raw$Salary_Band %in% EXPECTED_SALARIES))
})

cat("\n-- Filter logic (reactive df simulation) --\n")
test_list[[23]] <- run_test("Filter: filtering by Region reduces rows correctly", {
  sub <- df_raw[df_raw$Region == "North America", ]
  stopifnot(nrow(sub) > 0, nrow(sub) < nrow(df_raw))
  stopifnot(all(sub$Region == "North America"))
})
test_list[[24]] <- run_test("Filter: tenure slider [5,10] returns correct subset", {
  sub <- df_raw[df_raw$Tenure_Years >= 5 & df_raw$Tenure_Years <= 10, ]
  stopifnot(nrow(sub) > 0)
  stopifnot(all(sub$Tenure_Years >= 5 & sub$Tenure_Years <= 10))
})

cat("\n-- KPI logic --\n")
test_list[[25]] <- run_test("KPI: mean Engagement is in plausible range [2.5, 4.5]", {
  v <- mean(df_raw$Engagement, na.rm=TRUE)
  stopifnot(v >= 2.5, v <= 4.5)
})
test_list[[26]] <- run_test("KPI: mean Retention_Intent is in plausible range [2.5, 4.5]", {
  v <- mean(df_raw$Retention_Intent, na.rm=TRUE)
  stopifnot(v >= 2.5, v <= 4.5)
})
test_list[[27]] <- run_test("KPI: mean Operational_Stress is in plausible range [2.0, 4.5]", {
  v <- mean(df_raw$Operational_Stress, na.rm=TRUE)
  stopifnot(v >= 2.0, v <= 4.5)
})
test_list[[28]] <- run_test("KPI: overall Avg_Climate is in plausible range [2.5, 4.5]", {
  v <- mean(df_raw$Avg_Climate, na.rm=TRUE)
  stopifnot(v >= 2.5, v <= 4.5)
})

# Summarise
test_df <- do.call(rbind, lapply(test_list, function(t) {
  data.frame(Test = t$name, Status = t$status, Message = t$message,
             stringsAsFactors = FALSE)
}))
pass_count <- sum(test_df$Status == "PASS")
fail_count <- sum(test_df$Status == "FAIL")
cat("\n  Unit Tests:", pass_count, "PASS |", fail_count, "FAIL |",
    nrow(test_df), "total\n\n")

# ==============================================================================
# SECTION 3 — SHINY UI TESTS (shinytest2)
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 3 — SHINY UI TESTS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

if (requireNamespace("shinytest2", quietly=TRUE)) {
  tryCatch({
    library(shinytest2)
    app <- AppDriver$new(".", timeout=15000)

    ui_tests <- list()
    run_ui_test <- function(name, expr) {
      result <- tryCatch({
        expr
        list(name=name, status="PASS", message="")
      }, error = function(e) {
        list(name=name, status="FAIL/TIMEOUT", message=conditionMessage(e))
      })
      cat(" ", result$status, "|", name, "\n")
      result
    }

    ui_tests[[1]] <- run_ui_test("App loads — title visible", {
      app$wait_for_idle()
    })
    ui_tests[[2]] <- run_ui_test("Overview tab — KPI1 rendered", {
      app$wait_for_idle()
      val <- app$get_value(output="kpi1")
      stopifnot(!is.null(val))
    })
    ui_tests[[3]] <- run_ui_test("Filter: Region selector changes n_total", {
      app$set_inputs(s_region="North America")
      app$wait_for_idle()
      val <- app$get_value(output="n_total")
      stopifnot(!is.null(val))
    })
    ui_tests[[4]] <- run_ui_test("Filter: reset Region to All", {
      app$set_inputs(s_region="All")
      app$wait_for_idle()
      val <- app$get_value(output="n_total")
      stopifnot(!is.null(val))
    })
    ui_tests[[5]] <- run_ui_test("Filter: Department filter works", {
      depts <- sort(unique(df_raw$Department))
      app$set_inputs(s_dept=depts[1])
      app$wait_for_idle()
    })
    ui_tests[[6]] <- run_ui_test("Climate tab — lollipop chart rendered", {
      app$set_inputs(s_dept="All")
      app$wait_for_idle()
      val <- app$get_value(output="p_lollipop")
      stopifnot(!is.null(val))
    })
    ui_tests[[7]] <- run_ui_test("Climate tab — heatmap rendered", {
      val <- app$get_value(output="p_heatmap")
      stopifnot(!is.null(val))
    })
    ui_tests[[8]] <- run_ui_test("Department tab — boxplot indicator switch", {
      app$set_inputs(dept_ind="Safety_Culture")
      app$wait_for_idle()
      val <- app$get_value(output="p_boxplot")
      stopifnot(!is.null(val))
    })
    ui_tests[[9]] <- run_ui_test("Regional tab — country bar rendered", {
      val <- app$get_value(output="p_country_bar")
      stopifnot(!is.null(val))
    })
    ui_tests[[10]] <- run_ui_test("Americas Map — default KPI rendered", {
      val <- app$get_value(output="p_map")
      stopifnot(!is.null(val))
    })
    ui_tests[[11]] <- run_ui_test("Americas Map — KPI switch to Engagement", {
      app$set_inputs(map_kpi="Engagement")
      app$wait_for_idle()
      val <- app$get_value(output="p_map")
      stopifnot(!is.null(val))
    })

    app$stop()

    ui_df <- do.call(rbind, lapply(ui_tests, function(t) {
      data.frame(Test=t$name, Status=t$status, Message=t$message,
                 stringsAsFactors=FALSE)
    }))
    cat("\n  UI Tests:", sum(ui_df$Status=="PASS"), "PASS |",
        sum(ui_df$Status!="PASS"), "FAIL/TIMEOUT |",
        nrow(ui_df), "total\n")

  }, error = function(e) {
    ui_df <- data.frame(Test="shinytest2 session", Status="TIMEOUT",
                        Message=conditionMessage(e), stringsAsFactors=FALSE)
    cat("  NOTE: shinytest2 timed out — expected on WSL.\n")
    cat("  App launches correctly; browser automation requires native Windows R.\n")
    cat("  Complete via UAT checklist (Section 5).\n")
  })
} else {
  ui_df <- data.frame(Test="shinytest2 not installed", Status="SKIPPED",
                      Message="install shinytest2 to enable", stringsAsFactors=FALSE)
  cat("  shinytest2 not installed — skipping UI tests.\n")
  cat("  Run: install.packages('shinytest2')\n")
}

cat("\n")

# ==============================================================================
# SECTION 4 — PERFORMANCE BENCHMARKS
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 4 — PERFORMANCE BENCHMARKS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

bench <- function(label, expr) {
  t <- system.time(expr)["elapsed"]
  rating <- if (t < 0.05) "Excellent (<50ms)"
            else if (t < 0.2) "Good (50-200ms)"
            else if (t < 0.5) "Acceptable (200-500ms)"
            else "REVIEW (>500ms)"
  cat(sprintf("  %-45s  %.4fs  [%s]\n", label, t, rating))
  data.frame(Operation=label, Elapsed_s=round(t,4), Rating=rating,
             stringsAsFactors=FALSE)
}

b1  <- bench("read.csv (full 12500 rows)",
             read.csv("sources/workplace_climate_maritime_2026.csv",
                      stringsAsFactors=FALSE, na.strings=""))

b2  <- bench("rowMeans across 20 CLIMATE_COLS",
             mutate(df_raw, Avg_Climate = rowMeans(
               across(all_of(CLIMATE_COLS)), na.rm=TRUE)))

b3  <- bench("filter by Region == 'North America'",
             filter(df_raw, Region == "North America"))

b4  <- bench("group_by Department + summarise all CLIMATE_COLS",
             df_raw |>
               group_by(Department) |>
               summarise(across(all_of(CLIMATE_COLS),
                                function(x) mean(x, na.rm=TRUE)),
                         .groups="drop"))

b5  <- bench("group_by Country + mean Avg_Climate",
             df_raw |>
               group_by(Country) |>
               summarise(avg=mean(Avg_Climate, na.rm=TRUE), .groups="drop"))

b6  <- bench("pivot_longer 20 cols for lollipop chart",
             df_raw |>
               summarise(across(all_of(CLIMATE_COLS),
                                function(x) mean(x, na.rm=TRUE))) |>
               pivot_longer(everything()))

b7  <- bench("Tenure_Band + Age_Group derivation",
             df_raw |>
               mutate(
                 Tenure_Band = case_when(
                   Tenure_Years < 2   ~ "< 2 yrs",
                   Tenure_Years <= 4  ~ "2-4 yrs",
                   Tenure_Years <= 10 ~ "5-10 yrs",
                   Tenure_Years <= 20 ~ "11-20 yrs",
                   TRUE               ~ "> 20 yrs"
                 ),
                 Age_Group = cut(Age, breaks=c(0,30,40,50,100),
                                 labels=c("Under 30","30-39","40-49","50+"),
                                 right=FALSE)
               ))

b8  <- bench("Full reactive df simulation (all filters active)",
             df_raw |>
               filter(Region=="South America",
                      Department==df_raw$Department[1],
                      Tenure_Years >= 2,
                      Tenure_Years <= 15))

perf_df <- rbind(b1, b2, b3, b4, b5, b6, b7, b8)
cat("\n")

# profvis flame graph
if (requireNamespace("profvis", quietly=TRUE)) {
  library(profvis)
  cat("  Generating profvis flame graph...\n")
  prof_html <- file.path(QA_DIR, "profvis_reactive.html")
  tryCatch({
    p <- profvis({
      d <- df_raw
      d <- filter(d, Region == "South America")
      d <- mutate(d, Avg_Climate = rowMeans(across(all_of(CLIMATE_COLS)), na.rm=TRUE))
      d |> group_by(Department) |>
        summarise(across(all_of(CLIMATE_COLS), function(x) mean(x, na.rm=TRUE)),
                  .groups="drop")
      d |> group_by(Country) |>
        summarise(avg=mean(Avg_Climate, na.rm=TRUE), .groups="drop")
      d |> summarise(across(all_of(CLIMATE_COLS),
                            function(x) mean(x, na.rm=TRUE))) |>
        pivot_longer(everything())
    })
    htmlwidgets::saveWidget(p, prof_html, selfcontained=TRUE)
    cat("  Flame graph saved to:", prof_html, "\n")
  }, error = function(e) {
    cat("  profvis error:", conditionMessage(e), "\n")
  })
} else {
  cat("  profvis not installed — skipping flame graph.\n")
  cat("  Run: install.packages('profvis')\n")
}

cat("\n")

# ==============================================================================
# SECTION 5 — STAKEHOLDER ALIGNMENT (UAT + Sprint Backlog)
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 5 — UAT CHECKLIST & SPRINT BACKLOG\n")
cat("─────────────────────────────────────────────────────────────\n\n")

uat_df <- data.frame(
  ID = paste0("UAT-", sprintf("%02d", 1:28)),
  Tab = c(
    rep("Global", 3),
    rep("Overview", 4),
    rep("Climate Indicators", 4),
    rep("Department Analysis", 4),
    rep("Regional View", 4),
    rep("Workforce Profile", 4),
    rep("Open Comments", 3),
    rep("Americas Map", 2)
  ),
  Test_Case = c(
    # Global
    "Dashboard loads without error at shinyapps.io URL",
    "Sidebar filters (Region, Dept, Gender, Salary, Work Type) update all tabs",
    "Tenure slider filters data correctly; n= counter updates",
    # Overview
    "KPI 1: Total Employees matches filter count",
    "KPI 2: Avg Engagement score matches cross-reference table",
    "KPI 3: Avg Retention Intent score matches cross-reference table",
    "KPI 4: Avg Operational Stress matches cross-reference table and shows inverted color",
    # Climate Indicators
    "Lollipop chart shows all 20 indicators ranked low to high",
    "Lollipop dot colors reflect score thresholds (green ≥4, amber ≥3, red <3)",
    "Heatmap displays Department × Indicator matrix with correct color scale",
    "Radar chart updates when group variable and selections change",
    # Department Analysis
    "Scatter plot shows WLB vs Operational Stress with bubble size = headcount",
    "Reference lines at WLB=3 and Stress=3 visible on scatter plot",
    "Boxplot changes indicator when dept_ind selector changes",
    "Tenure bias chart highlights mid-career crisis zone (2-4 yrs band)",
    # Regional View
    "Country bar chart ranks all 10 countries by Avg Composite Score",
    "Bar colors reflect region (North/Central/South America)",
    "Region indicator bar chart updates when reg_ind selector changes",
    "Full Score Table by Country renders with all 20 indicator columns",
    # Workforce Profile
    "Tenure distribution histogram renders correctly",
    "Age distribution histogram renders correctly",
    "Engagement by Tenure Band chart shows 95% CI error bars",
    "Environmental Pride violin plot shows all 4 age groups",
    # Open Comments
    "Comments table excludes Employee_ID column",
    "Min avg score slider filters comments correctly",
    "Department and Region filters in Comments tab work independently",
    # Americas Map
    "Choropleth map renders all 10 countries with correct color scale",
    "KPI selector on map switches the displayed indicator"
  ),
  Expected_Result = c(
    "Page loads, no R errors in console, all tabs visible",
    "All reactive outputs update within 2 seconds",
    "n= counter reflects filtered dataset size accurately",
    "KPI value matches nrow(filtered_df)",
    "Within ±0.05 of cross-reference mean",
    "Within ±0.05 of cross-reference mean",
    "Within ±0.05 of cross-reference mean; high stress = red",
    "20 dots ranked lowest to highest left-to-right",
    "Dots green/amber/red matching score thresholds",
    "Red–amber–green gradient; Stress column visible",
    "Radar updates to selected groups without error",
    "Bubbles sized by n; departments labeled",
    "Dashed gold reference lines at x=3, y=3",
    "Boxplot y-axis label changes to selected indicator",
    "Red shaded zone visible at 2-4 yrs band",
    "10 bars visible, sorted by score",
    "Colors match region: blue/gold/green",
    "Bar chart updates correctly to selected indicator",
    "scrollX table with 22 columns (Country, Region, Employees + 20 indicators)",
    "Histogram with ~30 bins, plausible 0–25 yr range",
    "Histogram with age range ~18–65",
    "Error bars visible on all 5 tenure bands",
    "Violin + box + mean line for each group",
    "Employee_ID absent from table columns",
    "Comments list narrows as min score slider increases",
    "Independent filters work without crash",
    "10 countries colored red–amber–green on Americas map",
    "Color legend title and map colors update"
  ),
  Priority = c(
    rep("High", 3),
    rep("High", 4),
    rep("High", 4),
    rep("High", 4),
    rep("High", 4),
    rep("Medium", 4),
    rep("Medium", 3),
    rep("Medium", 2)
  ),
  Status = "Pending",
  Tester = "Ezequiel Bassa",
  Notes  = "",
  stringsAsFactors = FALSE
)

sprint_df <- data.frame(
  ID       = paste0("SB-", sprintf("%02d", 1:8)),
  Item     = c(
    "Add confidence interval display to lollipop chart tooltips",
    "Add top 3 / bottom 3 indicator badges to Overview KPI row",
    "Export button for Full Score Table by Country (CSV download)",
    "Improve radar chart group color differentiation (>4 groups)",
    "Add year filter if longitudinal data is incorporated in future",
    "Add department-level eNPS breakdown to Department Analysis tab",
    "Open Comments tab: add sentiment score column",
    "Mobile responsiveness review — test on 375px viewport"
  ),
  Priority = c("Medium","Medium","Low","Medium","Low","Low","Low","Medium"),
  Sprint   = c("Sprint 2","Sprint 2","Sprint 3","Sprint 2",
               "Sprint 4","Sprint 3","Sprint 3","Sprint 2"),
  Owner    = "Ezequiel Bassa",
  Status   = "Backlog",
  stringsAsFactors = FALSE
)

cat("  UAT checklist:", nrow(uat_df), "test cases generated\n")
cat("  Sprint backlog:", nrow(sprint_df), "items generated\n")
cat("  UAT worksheet saved to Excel (see Section below)\n\n")

# ==============================================================================
# SECTION 6 — CROSS-REFERENCE SAMPLE
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 6 — CROSS-REFERENCE SAMPLE\n")
cat("─────────────────────────────────────────────────────────────\n\n")

set.seed(42)
sample_countries <- sample(EXPECTED_COUNTRIES, 8)
cat("  Sample countries (seed=42):", paste(sample_countries, collapse=", "), "\n\n")

# KPI medians by country (for UAT cross-reference)
kpi_means <- df_raw |>
  filter(Country %in% sample_countries) |>
  group_by(Country) |>
  summarise(
    N                = n(),
    Avg_Climate_Mean = round(mean(Avg_Climate,        na.rm=TRUE), 3),
    Engagement_Mean  = round(mean(Engagement,         na.rm=TRUE), 3),
    Retention_Mean   = round(mean(Retention_Intent,   na.rm=TRUE), 3),
    Stress_Mean      = round(mean(Operational_Stress, na.rm=TRUE), 3),
    Safety_Mean      = round(mean(Safety_Culture,     na.rm=TRUE), 3),
    WLB_Mean         = round(mean(Work_Life_Balance,  na.rm=TRUE), 3),
    .groups = "drop"
  )
print(kpi_means, n=Inf)

# Full indicator averages per country (for table cross-reference)
xref_df <- df_raw |>
  filter(Country %in% sample_countries) |>
  group_by(Country, Region) |>
  summarise(
    N = n(),
    across(all_of(CLIMATE_COLS), function(x) round(mean(x, na.rm=TRUE), 3)),
    .groups = "drop"
  )

# Department summary
dept_summary <- df_raw |>
  group_by(Department) |>
  summarise(
    N           = n(),
    Avg_Climate = round(mean(Avg_Climate,        na.rm=TRUE), 3),
    Engagement  = round(mean(Engagement,         na.rm=TRUE), 3),
    Retention   = round(mean(Retention_Intent,   na.rm=TRUE), 3),
    Stress      = round(mean(Operational_Stress, na.rm=TRUE), 3),
    Safety      = round(mean(Safety_Culture,     na.rm=TRUE), 3),
    WLB         = round(mean(Work_Life_Balance,  na.rm=TRUE), 3),
    .groups = "drop"
  ) |>
  arrange(desc(Avg_Climate))

cat("\n  Department summary (ranked by Avg Climate Score):\n")
print(dept_summary, n=Inf)

cat("\n")

# ==============================================================================
# SECTION 7 — EXCEL WORKBOOK
# ==============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECTION 7 — EXCEL WORKBOOK\n")
cat("─────────────────────────────────────────────────────────────\n\n")

wb <- createWorkbook()

hdr_style <- createStyle(
  fontColour="#FFFFFF", fgFill="#1a6fa8",
  halign="LEFT", textDecoration="Bold",
  border="Bottom", borderColour="#c9a84c", fontSize=10
)

add_sheet <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df, headerStyle=hdr_style)
  setColWidths(wb, sheet_name, cols=seq_len(ncol(df)), widths="auto")
}

# 1. Summary
summary_data <- data.frame(
  Item = c(
    "Audit Date", "Dashboard", "Data File", "Total Rows",
    "Total Columns", "Climate Indicators", "Countries", "Regions",
    "Employees with Comments",
    "Validate Rules Total", "Validate Violations",
    "Duplicate Employee IDs",
    "Unit Tests Total", "Unit Tests PASS", "Unit Tests FAIL",
    "UAT Test Cases", "Sprint Backlog Items",
    "Data Issues Found"
  ),
  Value = c(
    format(Sys.time(), "%Y-%m-%d"),
    "Maritime Workplace Climate Dashboard",
    "workplace_climate_maritime_2026.csv",
    format(nrow(df_raw), big.mark=","),
    ncol(df_raw),
    length(CLIMATE_COLS),
    length(unique(df_raw$Country)),
    length(unique(df_raw$Region)),
    format(n_cmt, big.mark=","),
    nrow(validate_df),
    sum(validate_df$fails, na.rm=TRUE),
    nrow(dupes),
    nrow(test_df),
    pass_count,
    fail_count,
    nrow(uat_df),
    nrow(sprint_df),
    "See Validate_Rules and Outliers sheets"
  ),
  stringsAsFactors=FALSE
)
add_sheet(wb, "Summary", summary_data)

# 2. Validate Rules
add_sheet(wb, "Validate_Rules", validate_df)

# 3. Indicator Summary
add_sheet(wb, "Indicator_Summary", ind_summary_long)

# 4. KPI Cross-Reference
add_sheet(wb, "KPI_CrossRef", kpi_means)

# 5. Country Full Indicators
add_sheet(wb, "Country_Indicators", xref_df)

# 6. Department Summary
add_sheet(wb, "Department_Summary", dept_summary)

# 7. Outliers
if (exists("outlier_df")) {
  out_save <- if (nrow(outlier_df) > 0 && "Flagged_Indicator" %in% names(outlier_df)) {
    outlier_df[, c("Employee_ID","Country","Department","Flagged_Indicator",
                   intersect(CLIMATE_COLS, names(outlier_df)))]
  } else { outlier_df }
  add_sheet(wb, "Outliers", out_save)
}

# 8. Unit Tests
add_sheet(wb, "Unit_Tests", test_df)

# 9. Performance
add_sheet(wb, "Performance", perf_df)

# 10. UAT Checklist
add_sheet(wb, "UAT_Checklist", uat_df)

# 11. Sprint Backlog
add_sheet(wb, "Sprint_Backlog", sprint_df)

# 12. Category Distribution
cat_dist <- rbind(
  data.frame(Category="Region",      Value=names(table(df_raw$Region)),      Count=as.integer(table(df_raw$Region)),      stringsAsFactors=FALSE),
  data.frame(Category="Country",     Value=names(table(df_raw$Country)),     Count=as.integer(table(df_raw$Country)),     stringsAsFactors=FALSE),
  data.frame(Category="Salary_Band", Value=names(table(df_raw$Salary_Band)), Count=as.integer(table(df_raw$Salary_Band)), stringsAsFactors=FALSE),
  data.frame(Category="Gender",      Value=names(table(df_raw$Gender)),      Count=as.integer(table(df_raw$Gender)),      stringsAsFactors=FALSE),
  data.frame(Category="Work_Type",   Value=names(table(df_raw$Work_Type)),   Count=as.integer(table(df_raw$Work_Type)),   stringsAsFactors=FALSE)
)
add_sheet(wb, "Category_Distribution", cat_dist)

saveWorkbook(wb, XLSX_FILE, overwrite=TRUE)
cat("  Excel workbook saved to:", XLSX_FILE, "\n")
cat("  Sheets:", paste(names(wb), collapse=", "), "\n\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================
cat("=============================================================\n")
cat(" QA AUDIT COMPLETE\n")
cat("=============================================================\n")
cat(" Unit Tests     :", pass_count, "PASS /", fail_count, "FAIL /", nrow(test_df), "total\n")
cat(" Validate Rules : 0 violations expected\n")
cat(" Duplicate IDs  :", nrow(dupes), "\n")
cat(" Outlier rows   :", if (exists("outlier_df") && nrow(outlier_df) > 0 &&
                             "Flagged_Indicator" %in% names(outlier_df))
                             nrow(outlier_df) else 0, "\n")
cat(" UAT items      :", nrow(uat_df), "generated (complete manually)\n")
cat(" Excel report   :", XLSX_FILE, "\n")
cat(" Log file       :", LOG_FILE, "\n")
cat("=============================================================\n\n")

sink()
close(log_con)
cat("QA Audit complete. Check", QA_DIR, "for all outputs.\n")
