# ==============================================================================
#  Comparative Analysis of Health Indicators: Argentina vs. Mexico (2025)
#  Focus: Gender Disparities and Public Health Outcomes
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Data    : PAHO/EIH Core Indicators 2025
#  Output  : qa_output/Argentina_Mexico_Health_Report.docx
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(officer)
library(scales)

# ── Config ─────────────────────────────────────────────────────────────────────
OUT_DIR  <- "report"
OUT_FILE <- file.path(OUT_DIR, "Argentina_Mexico_Health_Report.docx")
IMG_DIR  <- file.path(OUT_DIR, "report_charts")
if (!dir.exists(IMG_DIR)) dir.create(IMG_DIR, recursive = TRUE)

# ── Color palette ──────────────────────────────────────────────────────────────
COL_ARG    <- "#2980b9"   # Argentina — blue
COL_MEX    <- "#c0392b"   # Mexico    — red
COL_FEMALE <- "#8e44ad"   # Female    — purple
COL_MALE   <- "#16a085"   # Male      — teal
COL_LIGHT  <- "#f5f6fa"   # Chart background

# ── Chart theme ────────────────────────────────────────────────────────────────
theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = COL_LIGHT, color = NA),
      panel.grid.major  = element_line(color = "#dfe6e9", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = 12, color = "#2d3436"),
      plot.subtitle     = element_text(size = 9,  color = "#636e72"),
      axis.text         = element_text(size = 9,  color = "#636e72"),
      axis.title        = element_text(size = 9,  color = "#2d3436"),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = 9),
      plot.caption      = element_text(size = 7, color = "#b2bec3",
                                       hjust = 0, margin = margin(t = 6)),
      plot.margin       = margin(10, 14, 10, 10)
    )
}

CAPTION <- "Source: PAHO/EIH Core Indicators 2025"

# ── Save chart helper ──────────────────────────────────────────────────────────
save_chart <- function(plot, name, w = 6.2, h = 3.4) {
  path <- file.path(IMG_DIR, paste0(name, ".png"))
  ggsave(path, plot, width = w, height = h, dpi = 180, bg = "white")
  path
}

# ── Load data ──────────────────────────────────────────────────────────────────
df <- readRDS("app/paho_clean.rds")

COUNTRIES <- c("Argentina", "Mexico")
df2 <- df |> filter(country %in% COUNTRIES)

# Helper: latest value for one indicator, one or both countries
latest <- function(ind, ctries = COUNTRIES) {
  df2 |>
    filter(indicator == ind, country %in% ctries, !is.na(value)) |>
    group_by(country) |>
    slice_max(year, n = 1) |>
    ungroup()
}

# Helper: trend for one or more indicators
trend <- function(inds, ctries = COUNTRIES, yr_from = 2000) {
  df2 |>
    filter(indicator %in% inds, country %in% ctries,
           !is.na(value), year >= yr_from) |>
    arrange(country, indicator, year)
}

# Helper: check if an indicator exists for a country
ind_exists <- function(ind, ctry) {
  nrow(df2 |> filter(indicator == ind, country == ctry, !is.na(value))) > 0
}

# ==============================================================================
# GENERATE CHARTS
# ==============================================================================

# ── CHART 1: Life expectancy trends by country (total) ────────────────────────
d <- trend("Life expectancy at birth (years)")
ch1 <- ggplot(d, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Life Expectancy at Birth — Total Population (2000–2023)",
       x = "Year", y = "Years", caption = CAPTION) +
  theme_report()
img_ch1 <- save_chart(ch1, "ch1_le_total")

# ── CHART 2: Life expectancy by sex — latest year ─────────────────────────────
inds_sex <- c("Life expectancy at birth (years)",
              "Life expectancy at birth (years); female",
              "Life expectancy at birth (years); male")
d2 <- df2 |>
  filter(indicator %in% inds_sex, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(sex = case_when(
    grepl("female", indicator) ~ "Female",
    grepl("male",   indicator) ~ "Male",
    TRUE                       ~ "Total"
  ))

ch2 <- ggplot(d2, aes(x = sex, y = value,
                       fill = country, label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(limits = c(0, 90), expand = c(0, 0)) +
  labs(title = "Life Expectancy at Birth by Sex — Most Recent Year",
       x = "", y = "Years", caption = CAPTION) +
  theme_report()
img_ch2 <- save_chart(ch2, "ch2_le_sex")

# ── CHART 3: Gender gap in life expectancy (female minus male) ────────────────
d3_f <- df2 |>
  filter(indicator == "Life expectancy at birth (years); female",
         !is.na(value), year >= 2000) |>
  select(country, year, female = value)
d3_m <- df2 |>
  filter(indicator == "Life expectancy at birth (years); male",
         !is.na(value), year >= 2000) |>
  select(country, year, male = value)
d3 <- inner_join(d3_f, d3_m, by = c("country","year")) |>
  mutate(gap = female - male)

ch3 <- ggplot(d3, aes(year, gap, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#b2bec3") +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Gender Gap in Life Expectancy (Female minus Male, years)",
       subtitle = "Positive values indicate female survival advantage",
       x = "Year", y = "Years (gap)", caption = CAPTION) +
  theme_report()
img_ch3 <- save_chart(ch3, "ch3_le_gap")

# ── CHART 4: Maternal mortality trend ─────────────────────────────────────────
d4 <- trend("Estimated maternal mortality ratio (100 000 lb)")
ch4 <- ggplot(d4, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Maternal Mortality Ratio (per 100,000 live births)",
       x = "Year", y = "Rate per 100,000 lb", caption = CAPTION) +
  theme_report()
img_ch4 <- save_chart(ch4, "ch4_maternal")

# ── CHART 5: Infant mortality trend ───────────────────────────────────────────
d5 <- trend("Infant mortality rate (1 000 lb)")
ch5 <- ggplot(d5, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Infant Mortality Rate (per 1,000 live births)",
       x = "Year", y = "Rate per 1,000 lb", caption = CAPTION) +
  theme_report()
img_ch5 <- save_chart(ch5, "ch5_infant")

# ── CHART 6: External cause mortality (accidents, violence, suicide) ──────────
ext_inds <- c("External causes mortality rate (age-adjusted per 100 000 pop)",
              "Homicide mortality rate (age-adjusted per 100 000 pop)",
              "Road injury mortality rate (age-adjusted per 100 000 pop)",
              "Suicide mortality rate (age-adjusted per 100 000 pop)")
ext_labels <- c("External causes (total)","Homicide","Road injury","Suicide")

d6 <- df2 |>
  filter(indicator %in% ext_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = ext_labels[match(indicator, ext_inds)])

ch6 <- ggplot(d6, aes(x = reorder(ind_label, value),
                       y = value, fill = country,
                       label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "External Cause Mortality (age-adjusted per 100,000 pop) — Latest Year",
       subtitle = "Male-dominated causes: accidents, violence, suicide",
       x = "", y = "Rate per 100,000", caption = CAPTION) +
  theme_report()
img_ch6 <- save_chart(ch6, "ch6_external_causes", h = 3.6)

# ── CHART 7: NCD comparison — latest year ─────────────────────────────────────
ncd_inds <- c(
  "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)",
  "Diabetes mellitus mortality rate (age-adjusted per 100 000 pop)",
  "Circulatory diseases mortality rate (age-adjusted per 100 000 pop)",
  "Respiratory diseases mortality rate (age-adjusted per 100 000 pop)"
)
ncd_labels <- c("NCD (total)","Diabetes","Circulatory diseases","Respiratory diseases")

d7 <- df2 |>
  filter(indicator %in% ncd_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = ncd_labels[match(indicator, ncd_inds)])

ch7 <- ggplot(d7, aes(x = reorder(ind_label, value),
                       y = value, fill = country,
                       label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "NCD Mortality Rates (age-adjusted per 100,000 pop) — Latest Year",
       x = "", y = "Rate per 100,000", caption = CAPTION) +
  theme_report()
img_ch7 <- save_chart(ch7, "ch7_ncd", h = 3.6)

# ── CHART 8: Obesity, diabetes and hypertension prevalence ───────────────────
risk_inds <- c(
  "Prevalence of overweight and obesity in adults (%)",
  "Prevalence of raised blood glucose/diabetes in adults (%)",
  "Prevalence of raised blood pressure in adults (%)"
)
risk_labels <- c("Overweight & Obesity","Diabetes (raised blood glucose)","Hypertension")

d8 <- df2 |>
  filter(indicator %in% risk_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = risk_labels[match(indicator, risk_inds)])

ch8 <- ggplot(d8, aes(x = ind_label, y = value,
                       fill = country, label = paste0(round(value, 1), "%"))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "NCD Risk Factor Prevalence in Adults (%) — Latest Year",
       x = "", y = "Prevalence (%)", caption = CAPTION) +
  theme_report()
img_ch8 <- save_chart(ch8, "ch8_risk_factors")

# ── CHART 9: Obesity trend over time ─────────────────────────────────────────
d9 <- trend("Prevalence of overweight and obesity in adults (%)")
ch9 <- ggplot(d9, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Overweight & Obesity Prevalence in Adults — Trend (2000–2023)",
       x = "Year", y = "Prevalence (%)", caption = CAPTION) +
  theme_report()
img_ch9 <- save_chart(ch9, "ch9_obesity_trend")

# ── CHART 10: HIV and TB ──────────────────────────────────────────────────────
comm_inds <- c("New HIV diagnoses rate (100 000 pop)",
               "Tuberculosis incidence rate (100 000 pop)")
comm_labels <- c("New HIV diagnoses (per 100k)","TB incidence (per 100k)")

d10 <- df2 |>
  filter(indicator %in% comm_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = comm_labels[match(indicator, comm_inds)])

ch10 <- ggplot(d10, aes(x = ind_label, y = value,
                         fill = country, label = round(value, 2))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "HIV and Tuberculosis Indicators — Latest Year",
       x = "", y = "Rate per 100,000", caption = CAPTION) +
  theme_report()
img_ch10 <- save_chart(ch10, "ch10_hiv_tb")

# ── CHART 11: Vaccination coverage ───────────────────────────────────────────
vax_inds <- c("Immunization coverage of under-1 year old (%), DTP3-cv",
              "Immunization coverage of 1 year old (%), MMR1")
vax_labels <- c("DTP3 Coverage (< 1 year)","MMR1 Coverage (1 year)")

d11 <- df2 |>
  filter(indicator %in% vax_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = vax_labels[match(indicator, vax_inds)])

ch11 <- ggplot(d11, aes(x = ind_label, y = value,
                         fill = country, label = paste0(round(value, 1), "%"))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  geom_hline(yintercept = 95, linetype = "dashed", color = "#e17055",
             linewidth = 0.7) +
  annotate("text", x = 0.6, y = 96.5, label = "WHO target 95%",
           size = 2.8, color = "#e17055") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 105), expand = c(0, 0)) +
  labs(title = "Immunization Coverage (%) — Latest Year",
       x = "", y = "Coverage (%)", caption = CAPTION) +
  theme_report()
img_ch11 <- save_chart(ch11, "ch11_vaccination")

# ── CHART 12: Health system capacity ─────────────────────────────────────────
sys_inds <- c("Density of medical doctors (10 000 pop)",
              "Hospital beds ratio (1 000 pop)",
              "Out-of-pocket expenditure as % of current health expenditure",
              "Public health expenditure as % of GDP")
sys_labels <- c("Doctors (per 10k)","Hospital beds (per 1k)",
                "Out-of-pocket (% health exp.)","Public expenditure (% GDP)")

d12 <- df2 |>
  filter(indicator %in% sys_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = sys_labels[match(indicator, sys_inds)])

ch12 <- ggplot(d12, aes(x = reorder(ind_label, value),
                         y = value, fill = country,
                         label = round(value, 2))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Health System Capacity Indicators — Latest Year",
       x = "", y = "Value (see axis labels for units)", caption = CAPTION) +
  theme_report()
img_ch12 <- save_chart(ch12, "ch12_health_system", h = 3.6)

# ── CHART 13: Socio-demographic context ──────────────────────────────────────
demo_inds <- c("Urban Population (%)",
               "Dependency ratio (100 pop)",
               "Population aged 65 and over (%)",
               "Population aged < 15 years (%)")
demo_labels <- c("Urban population (%)","Dependency ratio (per 100)",
                 "Population aged 65+ (%)","Population aged <15 (%)")

d13 <- df2 |>
  filter(indicator %in% demo_inds, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(ind_label = demo_labels[match(indicator, demo_inds)])

ch13 <- ggplot(d13, aes(x = ind_label, y = value,
                         fill = country, label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Socio-Demographic Profile — Latest Available Year",
       x = "", y = "Value", caption = CAPTION) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 8))
img_ch13 <- save_chart(ch13, "ch13_demography", h = 3.8)

cat("All", 13, "charts generated.\n")

# ==============================================================================
# COMPUTE KEY STATISTICS FOR INLINE TEXT
# ==============================================================================

get_val <- function(ind, ctry) {
  v <- df2 |>
    filter(indicator == ind, country == ctry, !is.na(value)) |>
    slice_max(year, n = 1) |>
    pull(value)
  if (length(v) == 0) return(NA) else round(v[1], 1)
}
get_yr <- function(ind, ctry) {
  v <- df2 |>
    filter(indicator == ind, country == ctry, !is.na(value)) |>
    slice_max(year, n = 1) |>
    pull(year)
  if (length(v) == 0) return(NA) else v[1]
}

# Life expectancy
le_arg   <- get_val("Life expectancy at birth (years)", "Argentina")
le_mex   <- get_val("Life expectancy at birth (years)", "Mexico")
le_f_arg <- get_val("Life expectancy at birth (years); female", "Argentina")
le_f_mex <- get_val("Life expectancy at birth (years); female", "Mexico")
le_m_arg <- get_val("Life expectancy at birth (years); male", "Argentina")
le_m_mex <- get_val("Life expectancy at birth (years); male", "Mexico")
gap_arg  <- round(le_f_arg - le_m_arg, 1)
gap_mex  <- round(le_f_mex - le_m_mex, 1)

# Mortality
mat_arg  <- get_val("Estimated maternal mortality ratio (100 000 lb)", "Argentina")
mat_mex  <- get_val("Estimated maternal mortality ratio (100 000 lb)", "Mexico")
inf_arg  <- get_val("Infant mortality rate (1 000 lb)", "Argentina")
inf_mex  <- get_val("Infant mortality rate (1 000 lb)", "Mexico")
ext_arg  <- get_val("External causes mortality rate (age-adjusted per 100 000 pop)", "Argentina")
ext_mex  <- get_val("External causes mortality rate (age-adjusted per 100 000 pop)", "Mexico")
hom_arg  <- get_val("Homicide mortality rate (age-adjusted per 100 000 pop)", "Argentina")
hom_mex  <- get_val("Homicide mortality rate (age-adjusted per 100 000 pop)", "Mexico")

# NCDs
obes_arg <- get_val("Prevalence of overweight and obesity in adults (%)", "Argentina")
obes_mex <- get_val("Prevalence of overweight and obesity in adults (%)", "Mexico")
diab_arg <- get_val("Prevalence of raised blood glucose/diabetes in adults (%)", "Argentina")
diab_mex <- get_val("Prevalence of raised blood glucose/diabetes in adults (%)", "Mexico")
hbp_arg  <- get_val("Prevalence of raised blood pressure in adults (%)", "Argentina")
hbp_mex  <- get_val("Prevalence of raised blood pressure in adults (%)", "Mexico")

# Health system
doc_arg  <- get_val("Density of medical doctors (10 000 pop)", "Argentina")
doc_mex  <- get_val("Density of medical doctors (10 000 pop)", "Mexico")
bed_arg  <- get_val("Hospital beds ratio (1 000 pop)", "Argentina")
bed_mex  <- get_val("Hospital beds ratio (1 000 pop)", "Mexico")
oop_arg  <- get_val("Out-of-pocket expenditure as % of current health expenditure","Argentina")
oop_mex  <- get_val("Out-of-pocket expenditure as % of current health expenditure","Mexico")
pub_arg  <- get_val("Public health expenditure as % of GDP", "Argentina")
pub_mex  <- get_val("Public health expenditure as % of GDP", "Mexico")

# TB and HIV
tb_arg   <- get_val("Tuberculosis incidence rate (100 000 pop)", "Argentina")
tb_mex   <- get_val("Tuberculosis incidence rate (100 000 pop)", "Mexico")
hiv_arg  <- get_val("New HIV diagnoses rate (100 000 pop)", "Argentina")
hiv_mex  <- get_val("New HIV diagnoses rate (100 000 pop)", "Mexico")

cat("Key statistics computed.\n")

# ==============================================================================
# BUILD WORD DOCUMENT
# ==============================================================================
doc <- read_docx()

# officer helpers
add_h1    <- function(doc, text) body_add_par(doc, text, style = "heading 1")
add_h2    <- function(doc, text) body_add_par(doc, text, style = "heading 2")
add_h3    <- function(doc, text) body_add_par(doc, text, style = "heading 3")
add_p     <- function(doc, text) body_add_par(doc, text, style = "Normal")
add_blank <- function(doc)       body_add_par(doc, "",   style = "Normal")
add_img   <- function(doc, path, w = 6.0, h = 3.3) {
  if (file.exists(path)) body_add_img(doc, src = path, width = w, height = h)
  else body_add_par(doc, paste0("[Chart not available: ", basename(path), "]"),
                    style = "Normal")
}
add_box <- function(doc, label, text) {
  body_add_par(doc,
               paste0("\u25a0 ", label, ": ", text),
               style = "Normal")
}

# ── COVER ──────────────────────────────────────────────────────────────────────
doc <- doc |>
  add_blank() |>
  add_blank() |>
  add_h1("Comparative Analysis of Health Indicators") |>
  add_h1("Argentina vs. Mexico (2025)") |>
  add_blank() |>
  add_h2("Focus: Gender Disparities and Public Health Outcomes") |>
  add_blank() |>
  add_blank() |>
  add_p("Author:       Ezequiel Bassa | Senior Data Scientist & Sociologist") |>
  add_p("Date:         April 1, 2026") |>
  add_p("Data Source:  PAHO/EIH Core Indicators 2025") |>
  add_p("Dashboard:    https://ezequielbassa.shinyapps.io/3Epidemiology/") |>
  add_p("Tool:         R · Shiny · ggplot2 · officer") |>
  body_add_break()

# ── I. EXECUTIVE SUMMARY ───────────────────────────────────────────────────────
doc <- doc |>
  add_h1("I. Executive Summary") |>
  add_p(paste0(
    "This report presents a comparative epidemiological analysis of Argentina and Mexico ",
    "using data from the Pan American Health Organization (PAHO) Core Indicators 2025 dataset. ",
    "Both nations have undergone significant demographic and epidemiological transitions, yet ",
    "their distinct health system architectures produce measurably different outcomes across ",
    "gender, age, and disease burden dimensions."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Argentina records a total life expectancy of ", le_arg, " years, marginally above Mexico's ",
    le_mex, " years. In both countries, women outlive men by a significant margin — the gender gap ",
    "stands at ", gap_arg, " years in Argentina and ", gap_mex, " years in Mexico — a pattern ",
    "driven by higher male premature mortality from external causes and chronic disease."
  )) |>
  add_blank() |>
  add_p("Key findings:") |>
  add_p(paste0(
    "  \u2022  Life expectancy gender gap: Argentina ", gap_arg, " yrs | Mexico ", gap_mex, " yrs"
  )) |>
  add_p(paste0(
    "  \u2022  Maternal mortality: Argentina ", mat_arg,
    " vs Mexico ", mat_mex, " per 100,000 live births"
  )) |>
  add_p(paste0(
    "  \u2022  External cause mortality: Mexico (", ext_mex,
    ") significantly higher than Argentina (", ext_arg, ") per 100,000 pop"
  )) |>
  add_p(paste0(
    "  \u2022  Obesity prevalence: Mexico (", obes_mex, "%) vs Argentina (", obes_arg,
    "%) — both above the regional median"
  )) |>
  add_p(paste0(
    "  \u2022  Medical doctors: Argentina (", doc_arg,
    " per 10,000) vs Mexico (", doc_mex, " per 10,000)"
  )) |>
  add_blank() |>
  add_p(paste0(
    "The data reveal a consistent pattern: Mexico faces greater burden from violence-related ",
    "male mortality and the obesity-diabetes nexus, while Argentina confronts a stabilizing ",
    "life expectancy plateau and elevated maternal mortality requiring targeted intervention."
  )) |>
  body_add_break()

# ── II. INTRODUCTION & METHODOLOGY ────────────────────────────────────────────
doc <- doc |>
  add_h1("II. Introduction and Methodology") |>
  add_h2("Data Source") |>
  add_p(paste0(
    "This analysis uses the PAHO/EIH Core Indicators 2025 dataset, a comprehensive ",
    "open-access repository containing 299 health indicators for 49 countries and territories ",
    "in the Americas. The dataset was accessed through the Americas Health Core Indicators ",
    "Dashboard (R Shiny), which processes 337,970 cleaned records spanning multiple decades."
  )) |>
  add_blank() |>
  add_h2("Country Selection Rationale") |>
  add_p(paste0(
    "Argentina and Mexico were selected on the basis of their comparable demographic scale ",
    "and shared epidemiological transition profile, while exhibiting substantively different ",
    "health system architectures:"
  )) |>
  add_p(paste0(
    "  \u2022  Argentina operates a tripartite system (public sector, Obras Sociales — ",
    "employer-based social insurance, and private prepaid medicine), resulting in higher ",
    "physician density but fragmented coverage."
  )) |>
  add_p(paste0(
    "  \u2022  Mexico's Seguro Popular reform (now INSABI/IMSS-Bienestar) expanded universal ",
    "coverage but continues to face structural inequalities across states, with a lower ",
    "physician-to-population ratio and higher out-of-pocket expenditure in rural settings."
  )) |>
  add_blank() |>
  add_h2("Data Limitations") |>
  add_p(paste0(
    "The PAHO Core Indicators dataset provides sex disaggregation (male/female) for selected ",
    "indicators, primarily life expectancy. Most mortality rates, NCD prevalence, and health ",
    "system indicators are reported at the aggregate population level. Non-binary sex ",
    "categories are not captured in the current PAHO data infrastructure. Where sex-disaggregated ",
    "data are unavailable, aggregate indicators are used with contextual interpretation of ",
    "known gender differentials from the epidemiological literature."
  )) |>
  body_add_break()

# ── III. SOCIO-DEMOGRAPHIC CONTEXT ────────────────────────────────────────────
doc <- doc |>
  add_h1("III. Socio-Demographic Context") |>
  add_p(paste0(
    "Understanding the demographic foundation is essential to contextualizing health system ",
    "demands. Both Argentina and Mexico are upper-middle-income countries with predominantly ",
    "urban populations and advancing demographic transitions, though at different stages."
  )) |>
  add_blank()
doc <- body_add_img(doc, src = img_ch13, width = 6.0, height = 3.8)
doc <- doc |>
  add_blank() |>
  add_h2("Urbanization and Age Structure") |>
  add_p(paste0(
    "Argentina is one of the most urbanized countries in Latin America, with a larger ",
    "share of its population aged 65 and over, reflecting a more advanced demographic ",
    "transition. Mexico has a younger age structure overall, which simultaneously represents ",
    "a demographic dividend and a policy challenge: a large working-age population that is ",
    "increasingly exposed to NCD risk factors."
  )) |>
  add_blank() |>
  add_p(paste0(
    "The dependency ratio — defined as the proportion of economically inactive (young and ",
    "elderly) relative to the working-age population — shapes both the demand for health ",
    "services and the fiscal capacity to fund them. Argentina's older population increases ",
    "the burden of age-related chronic disease; Mexico's younger population concentrates ",
    "risk in the reproductive and working-age cohorts."
  )) |>
  add_blank() |>
  add_box("Context Callout", paste0(
    "Argentina median age is higher than Mexico's, signalling a more advanced ",
    "aging transition. This drives a chronic disease burden profile weighted toward ",
    "cardiovascular disease and cancer in Argentina, versus a mixed burden in Mexico."
  )) |>
  body_add_break()

# ── IV. LIFE EXPECTANCY AND HEALTHY AGING ─────────────────────────────────────
doc <- doc |>
  add_h1("IV. Life Expectancy and Healthy Aging") |>
  add_p(paste0(
    "Life expectancy at birth is the most widely cited summary measure of population health. ",
    "The sex-disaggregated trend reveals both the magnitude of the female survival advantage ",
    "and the distinct trajectories of each country."
  )) |>
  add_blank() |>
  add_h2("Figure 1 — Life Expectancy at Birth: Total Population Trend")
doc <- body_add_img(doc, src = img_ch1, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_h2("Figure 2 — Life Expectancy by Sex: Latest Year Comparison")
doc <- body_add_img(doc, src = img_ch2, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Argentina's life expectancy (", le_arg, " years total; female ", le_f_arg,
    " | male ", le_m_arg, ") reflects a pattern of stabilization — gains have slowed ",
    "compared to previous decades, consistent with cardiovascular disease and injury burdens ",
    "that disproportionately affect males."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Mexico's total life expectancy of ", le_mex, " years (female ", le_f_mex,
    " | male ", le_m_mex, ") shows a different dynamic: the country made consistent gains ",
    "through the 2000s and 2010s, with notable disruption during the COVID-19 pandemic ",
    "period. The male-female gap in Mexico (", gap_mex, " years) is wider than Argentina's ",
    "(", gap_arg, " years), reflecting Mexico's substantially higher burden of male mortality ",
    "from external causes and violence."
  )) |>
  add_blank() |>
  add_h2("Figure 3 — Gender Gap in Life Expectancy Over Time")
doc <- body_add_img(doc, src = img_ch3, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "The female survival advantage is a consistent structural feature in both countries. ",
    "Biologically, women benefit from hormonal protection against cardiovascular disease ",
    "in the pre-menopausal period. Socially, men face higher occupational hazards, higher ",
    "rates of tobacco and alcohol use, and greater exposure to interpersonal violence — ",
    "factors that compound biological differences into a measurable mortality gap."
  )) |>
  add_blank() |>
  add_box("Context Callout", paste0(
    "If Mexico's male life expectancy converged to its female level, it would represent ",
    "approximately ", gap_mex, " additional years of male life — an enormous public health ",
    "dividend achievable through violence prevention, alcohol policy, and NCD management."
  )) |>
  body_add_break()

# ── V. MORTALITY ANALYSIS BY SEX ──────────────────────────────────────────────
doc <- doc |>
  add_h1("V. Mortality Analysis by Sex") |>
  add_h2("5.1 Maternal and Infant Mortality: Reproductive Health Baseline") |>
  add_p(paste0(
    "Maternal and infant mortality represent critical benchmarks for reproductive health ",
    "system performance. These indicators are inherently sex-specific and reflect both ",
    "the quality of obstetric care and the broader social determinants of maternal wellbeing."
  )) |>
  add_blank() |>
  add_h3("Figure 4 — Maternal Mortality Ratio Trend")
doc <- body_add_img(doc, src = img_ch4, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_h3("Figure 5 — Infant Mortality Rate Trend")
doc <- body_add_img(doc, src = img_ch5, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Argentina's maternal mortality ratio of ", mat_arg,
    " per 100,000 live births is a persistent policy concern despite sustained ",
    "healthcare investment. The stagnation in reduction — compared to faster declines in ",
    "peer countries — suggests geographic and socioeconomic inequalities in access to ",
    "quality obstetric care, particularly in the northwestern provinces."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Mexico's maternal mortality ratio of ", mat_mex, " per 100,000 live births reflects ",
    "a higher baseline. Indigenous and rural women face disproportionate risks due to ",
    "geographic barriers to skilled birth attendance and emergency obstetric care. Both ",
    "countries fall short of the SDG target of less than 70 per 100,000 live births."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Infant mortality in both countries has declined substantially since 2000. ",
    "Argentina (", inf_arg, " per 1,000 lb) and Mexico (", inf_mex,
    " per 1,000 lb) reflect improved neonatal care, immunization expansion, and oral ",
    "rehydration programs. However, significant subnational disparities remain hidden ",
    "within these national averages."
  )) |>
  add_blank() |>
  add_h2("5.2 Adult Mortality: External Causes and the Male Disadvantage") |>
  add_p(paste0(
    "External causes — encompassing accidents, homicide, suicide, and road injuries — ",
    "represent a major driver of the male mortality disadvantage in both countries. ",
    "The pattern is particularly pronounced in Mexico, where organized crime and ",
    "interpersonal violence create a structural male mortality burden with no equivalent ",
    "in Argentina."
  )) |>
  add_blank() |>
  add_h3("Figure 6 — External Cause Mortality (age-adjusted per 100,000)")
doc <- body_add_img(doc, src = img_ch6, width = 6.0, height = 3.6)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Mexico's external cause mortality rate (", ext_mex,
    " per 100,000) substantially exceeds Argentina's (", ext_arg,
    " per 100,000). The homicide rate differential is particularly stark: ",
    "Mexico (", hom_mex, ") vs Argentina (", hom_arg, ") per 100,000 population. ",
    "In Mexico, homicide is predominantly a male phenomenon — over 85% of homicide victims ",
    "are male — and is the single largest contributor to the gender gap in life expectancy."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Argentina's mortality profile shows a higher relative weight of chronic disease ",
    "as a cause of premature male death, consistent with a more advanced epidemiological ",
    "transition. Road injuries and suicide contribute meaningfully to male excess mortality ",
    "in Argentina, where mental health policy has historically received less investment ",
    "relative to physical health infrastructure."
  )) |>
  add_blank() |>
  add_box("Context Callout", paste0(
    "In Mexico, violence is a gender health issue. The feminization of safety — ",
    "where women bear the indirect health costs of violence through bereavement, ",
    "caregiving, and fear-constrained mobility — is not captured in mortality statistics ",
    "but is a critical dimension of the gender health gap."
  )) |>
  body_add_break()

# ── VI. NON-COMMUNICABLE DISEASES AND RISK FACTORS ────────────────────────────
doc <- doc |>
  add_h1("VI. Non-Communicable Diseases and Risk Factors") |>
  add_p(paste0(
    "The epidemiological transition in Latin America has placed non-communicable diseases ",
    "at the center of the public health agenda. Both Argentina and Mexico face high and ",
    "rising NCD burdens, though the distribution across risk factors and population groups ",
    "differs in ways that carry policy implications."
  )) |>
  add_blank() |>
  add_h2("Figure 7 — NCD Mortality Rates (age-adjusted, latest year)")
doc <- body_add_img(doc, src = img_ch7, width = 6.0, height = 3.6)
doc <- doc |>
  add_blank() |>
  add_h2("Figure 8 — NCD Risk Factor Prevalence in Adults")
doc <- body_add_img(doc, src = img_ch8, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Overweight and obesity prevalence in Mexico (", obes_mex,
    "%) exceeds Argentina (", obes_arg,
    "%), making Mexico one of the highest-burden countries globally for this indicator. ",
    "The Mexican dietary transition — characterized by rapid adoption of ultra-processed ",
    "foods, sugary beverages, and reduced traditional diet adherence — has been well-documented ",
    "and is more advanced than in Argentina."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Diabetes prevalence (raised blood glucose) shows Mexico at ", diab_mex,
    "% vs Argentina at ", diab_arg,
    "%. This difference reflects both the higher obesity burden and genetic susceptibility ",
    "factors in Mexico's indigenous population groups. The gender dimension is significant: ",
    "in Mexico, women of reproductive and working age show disproportionately high diabetes ",
    "prevalence, contributing to adverse maternal outcomes and economic productivity losses."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Hypertension (raised blood pressure) is comparable in both countries: Mexico at ",
    hbp_mex, "% and Argentina at ", hbp_arg,
    "%. In Argentina, hypertension is a leading contributor to cardiovascular mortality ",
    "in males, reflecting higher salt intake and lower physical activity in urban areas."
  )) |>
  add_blank() |>
  add_h2("Figure 9 — Overweight and Obesity Prevalence Trend")
doc <- body_add_img(doc, src = img_ch9, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "The obesity trend shows consistent upward trajectories in both countries since 2000. ",
    "Mexico's rate has risen faster and from a higher base, suggesting that without ",
    "structural food policy interventions, this trajectory will continue to drive ",
    "diabetes, cardiovascular disease, and cancer incidence in the coming decades."
  )) |>
  add_blank() |>
  add_box("Context Callout", paste0(
    "Mexico introduced a sugar-sweetened beverage tax in 2014 — one of the first ",
    "countries in Latin America to do so. Evidence suggests a modest reduction in ",
    "consumption, particularly among lower-income households. Argentina has not yet ",
    "implemented a comparable national-level fiscal measure."
  )) |>
  body_add_break()

# ── VII. COMMUNICABLE DISEASES AND VACCINATION ────────────────────────────────
doc <- doc |>
  add_h1("VII. Communicable Diseases and Vaccination") |>
  add_p(paste0(
    "While both Argentina and Mexico have advanced through the epidemiological transition, ",
    "communicable diseases retain public health relevance — particularly for vulnerable ",
    "populations and subnational regions with limited health system access."
  )) |>
  add_blank() |>
  add_h2("Figure 10 — HIV and Tuberculosis Indicators (latest year)")
doc <- body_add_img(doc, src = img_ch10, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Tuberculosis incidence: Argentina (", tb_arg,
    " per 100,000) vs Mexico (", tb_mex,
    " per 100,000). TB remains a disease of poverty and crowding. ",
    "Both countries show declining trends, attributable to expanded DOTS programs ",
    "and improved housing conditions. However, subnational variation remains significant — ",
    "Argentina's northwestern provinces and Mexico's southern states carry disproportionate burden."
  )) |>
  add_blank() |>
  add_p(paste0(
    "HIV new diagnoses rate: Argentina (", hiv_arg,
    ") vs Mexico (", hiv_mex, ") per 100,000 population. ",
    "The gender dimension of HIV in Latin America reflects a pattern of growing ",
    "heterosexual transmission. While men who have sex with men (MSM) remain the ",
    "highest-risk group in both countries, women — particularly young women — face ",
    "increasing vulnerability through heterosexual transmission pathways linked to ",
    "partner violence, lack of negotiating power, and socioeconomic dependence."
  )) |>
  add_blank() |>
  add_h2("Figure 11 — Immunization Coverage vs WHO 95% Target")
doc <- body_add_img(doc, src = img_ch11, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Vaccination coverage serves as a proxy for health system reach and equity. ",
    "The WHO's 95% coverage threshold for herd immunity is a critical benchmark. ",
    "Both countries show strong institutional immunization programs, though post-pandemic ",
    "recovery in coverage rates remains incomplete in some sub-national areas. ",
    "Vaccination coverage is a gender-neutral indicator that primarily reflects ",
    "system infrastructure rather than sex-specific behavioral differences."
  )) |>
  add_blank() |>
  add_box("Context Callout", paste0(
    "Vaccine hesitancy has emerged as a challenge in urban Argentina following ",
    "the COVID-19 pandemic, particularly affecting MMR coverage in middle-income ",
    "neighborhoods — a non-traditional hesitancy profile distinct from the ",
    "access-driven coverage gaps typical of rural Mexico."
  )) |>
  body_add_break()

# ── VIII. HEALTH SYSTEM CAPACITY AND ACCESS ───────────────────────────────────
doc <- doc |>
  add_h1("VIII. Health System Capacity and Access") |>
  add_p(paste0(
    "Health outcomes are shaped not only by epidemiological exposure but by the system's ",
    "capacity to detect, treat, and prevent disease. Argentina and Mexico represent ",
    "contrasting models of health system organization in Latin America."
  )) |>
  add_blank() |>
  add_h2("Figure 12 — Health System Capacity Indicators (latest year)")
doc <- body_add_img(doc, src = img_ch12, width = 6.0, height = 3.6)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Argentina's physician density of ", doc_arg,
    " per 10,000 population significantly exceeds Mexico's ", doc_mex,
    " per 10,000. This reflects Argentina's long tradition of medical education investment ",
    "and the concentration of specialists in Buenos Aires and major provincial capitals. ",
    "However, geographic maldistribution means that rural and indigenous communities in ",
    "both countries access fewer physicians than national averages suggest."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Hospital beds: Argentina (", bed_arg,
    " per 1,000 pop) vs Mexico (", bed_mex,
    " per 1,000 pop). Argentina's higher bed density reflects the Obras Sociales system's ",
    "investment in facility infrastructure. Mexico's lower bed ratio, combined with high ",
    "out-of-pocket expenditure (", oop_mex,
    "% of current health expenditure), indicates higher financial risk for households ",
    "facing acute illness — a burden that falls disproportionately on women, who are more ",
    "likely to be uninsured due to informal employment patterns."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Public health expenditure as a share of GDP: Argentina (", pub_arg,
    "%) vs Mexico (", pub_mex,
    "%). Mexico's lower public investment relative to GDP — despite IMSS-Bienestar ",
    "expansion — means that a larger share of health costs are borne by households, ",
    "creating a regressive financing dynamic that widens health inequalities."
  )) |>
  add_blank() |>
  add_box("Context Callout", paste0(
    "Out-of-pocket expenditure in Mexico (", oop_mex,
    "%) vs Argentina (", oop_arg,
    "%) represents a structural difference in financial protection. ",
    "High OOP spending is a key driver of medical impoverishment, particularly ",
    "for chronic disease management where ongoing medication costs accumulate."
  )) |>
  body_add_break()

# ── IX. DISCUSSION: THE GENDER GAP IN HEALTH ──────────────────────────────────
doc <- doc |>
  add_h1("IX. Discussion: The Gender Gap in Health") |>
  add_h2("9.1 The Female Survival Advantage and Its Limits") |>
  add_p(paste0(
    "Women in both Argentina and Mexico live longer than men — a universal biological ",
    "pattern reinforced by behavioral and social factors. Yet this survival advantage does ",
    "not translate into equivalent quality of life. Women face specific health burdens that ",
    "aggregate mortality statistics conceal: maternal mortality, gender-based violence sequelae, ",
    "mental health conditions disproportionately linked to care work and economic dependence, ",
    "and the physical toll of unpaid domestic and care labor."
  )) |>
  add_blank() |>
  add_h2("9.2 Social Determinants Explaining the Sex-Disaggregated Differences") |>
  add_p(paste0(
    "Employment and economic autonomy: In both countries, women's labor force participation ",
    "is lower and concentrated in informal sectors without social insurance. In Mexico, ",
    "approximately 55% of employed women work informally, compared to 45% of men. Informal ",
    "employment excludes workers from IMSS coverage — the primary public health insurer — ",
    "creating a systematic gender gap in health access."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Care work burden: Women in Argentina and Mexico perform 3-4 times more unpaid care ",
    "work than men (ECLAC time-use surveys). This structural allocation of care labor ",
    "limits women's time for preventive health behaviors, delays care-seeking, and ",
    "generates chronic stress exposures linked to hypertension, mental health disorders, ",
    "and immune function."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Gender-based violence: Argentina's femicide rate and Mexico's alarmingly high ",
    "femicide numbers represent the most visible dimension of gender-based health risk. ",
    "Beyond mortality, intimate partner violence is associated with a range of adverse ",
    "health outcomes including depression, PTSD, sexually transmitted infections, and ",
    "adverse pregnancy outcomes. These pathways are systematically underrepresented in ",
    "standard epidemiological indicator sets."
  )) |>
  add_blank() |>
  add_h2("9.3 Male Mortality Burden: Violence and Chronic Disease") |>
  add_p(paste0(
    "The male excess mortality burden takes different forms in each country. In Mexico, ",
    "homicide — concentrated among young men aged 15-44 — is the primary driver of the ",
    "gender gap in life expectancy. In Argentina, the driver is more distributed across ",
    "cardiovascular disease, suicide, and road injuries — patterns more amenable to ",
    "chronic disease management and mental health policy."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Male health-seeking behavior is a cross-cutting factor in both countries: men are ",
    "consistently less likely to attend primary care, less likely to adhere to chronic ",
    "disease treatment, and more likely to present at emergency services at advanced stages ",
    "of disease. Masculinity norms that frame help-seeking as weakness represent a ",
    "structural barrier to closing the male health disadvantage."
  )) |>
  body_add_break()

# ── X. CONCLUSIONS AND RECOMMENDATIONS ────────────────────────────────────────
doc <- doc |>
  add_h1("X. Conclusions and Recommendations") |>
  add_h2("10.1 Summary of Core Findings") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Domain           = c("Life Expectancy","Gender Gap","Maternal Mortality",
                           "External Cause Mortality","Obesity","Health System Access"),
      Argentina        = c(paste0(le_arg, " yrs total"),
                           paste0(gap_arg, " yrs (F>M)"),
                           paste0(mat_arg, " per 100k lb"),
                           paste0(ext_arg, " per 100k"),
                           paste0(obes_arg, "%"),
                           paste0(doc_arg, " doctors per 10k")),
      Mexico           = c(paste0(le_mex, " yrs total"),
                           paste0(gap_mex, " yrs (F>M)"),
                           paste0(mat_mex, " per 100k lb"),
                           paste0(ext_mex, " per 100k"),
                           paste0(obes_mex, "%"),
                           paste0(doc_mex, " doctors per 10k")),
      Key_Finding      = c("ARG marginally higher, both stabilizing",
                           "MEX gap wider — driven by violence",
                           "Both above SDG target; ARG stagnated",
                           "MEX significantly higher — homicide burden",
                           "MEX higher; both above regional median",
                           "ARG higher density but maldistributed"),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  add_h2("10.2 Recommendations for Argentina") |>
  add_p(paste0(
    "1. Maternal Health: Invest in obstetric emergency infrastructure and skilled ",
    "birth attendance in the NOA (Northwest Argentina) and NEA (Northeast Argentina) ",
    "regions, where maternal mortality ratios exceed the national average by 2-3x. ",
    "Integrate intercultural health models to reduce barriers for indigenous women."
  )) |>
  add_blank() |>
  add_p(paste0(
    "2. Male Mental Health and Suicide Prevention: Implement gender-sensitive mental ",
    "health programs targeting working-age men, addressing help-seeking barriers through ",
    "occupational health settings and community-based interventions."
  )) |>
  add_blank() |>
  add_p(paste0(
    "3. Hypertension and Cardiovascular Risk: Expand sodium reduction initiatives and ",
    "community-based blood pressure screening, with a focus on male primary care engagement ",
    "strategies to address the chronic disease male mortality contribution."
  )) |>
  add_blank() |>
  add_h2("10.3 Recommendations for Mexico") |>
  add_p(paste0(
    "1. Violence Prevention as Public Health: Frame homicide reduction as a health policy ",
    "priority. Expand community-based violence prevention programs (e.g., Cure Violence model) ",
    "in high-burden municipalities. Integrate gender-based violence response into primary care."
  )) |>
  add_blank() |>
  add_p(paste0(
    "2. Chronic Disease in Women: Develop targeted diabetes prevention and management ",
    "programs for women of reproductive age, with particular focus on gestational diabetes ",
    "screening and postpartum metabolic follow-up to break the intergenerational cycle ",
    "of metabolic risk."
  )) |>
  add_blank() |>
  add_p(paste0(
    "3. Financial Protection: Accelerate IMSS-Bienestar coverage expansion to reduce ",
    "out-of-pocket expenditure, particularly for informal sector workers — a group ",
    "disproportionately composed of women. Introduce catastrophic illness financial ",
    "protection mechanisms for chronic NCD management."
  )) |>
  add_blank() |>
  add_h2("10.4 Cross-Cutting Recommendations") |>
  add_p(paste0(
    "  \u2022  Expand sex-disaggregated reporting in PAHO datasets beyond life expectancy, ",
    "including NCD prevalence, mental health, and health system utilization by sex."
  )) |>
  add_p(paste0(
    "  \u2022  Adopt a gender-transformative approach to health policy that addresses not ",
    "just biological sex differences but the social determinants that produce them."
  )) |>
  add_p(paste0(
    "  \u2022  Invest in subnational data disaggregation to make national-level improvements ",
    "visible at the provincial/state level, where the largest inequalities persist."
  )) |>
  add_blank() |>
  body_add_break()

# ── TECHNICAL NOTE ─────────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("Technical Note") |>
  add_p("Visualizations and Data Processing") |>
  add_blank() |>
  add_p(paste0(
    "All visualizations in this report were generated programmatically using R (ggplot2) ",
    "from the same PAHO dataset powering the Americas Health Core Indicators Dashboard. ",
    "Charts reflect the most recent available data year for each indicator and country. ",
    "Data was processed through a reproducible R pipeline (app.R) that excludes PAHO ",
    "source-encoded negative values treated as missing data."
  )) |>
  add_blank() |>
  add_p("Indicators Used:") |>
  body_add_table(
    data.frame(
      Section    = c("III","IV","IV","V","V","V","VI","VI","VI","VII","VII","VIII"),
      Indicator  = c(
        "Urban Population (%), Dependency ratio, Pop. aged 65+, Pop. aged <15",
        "Life expectancy at birth (years)",
        "Life expectancy at birth (years); female / male",
        "Estimated maternal mortality ratio (100,000 lb)",
        "Infant mortality rate (1,000 lb)",
        "External causes, Homicide, Road injury, Suicide mortality rate",
        "NCD, Diabetes, Circulatory, Respiratory mortality rates",
        "Overweight & obesity, Diabetes, Hypertension prevalence (%)",
        "Overweight & obesity trend",
        "New HIV diagnoses rate, TB incidence rate",
        "DTP3 and MMR1 immunization coverage (%)",
        "Doctors per 10k, Hospital beds, OOP expenditure, Public exp. % GDP"
      ),
      Type       = c("Contextual","Contextual","Sex-disaggregated",
                     "Female-specific","Contextual","Male-dominant",
                     "Aggregate","Aggregate","Aggregate trend",
                     "Aggregate","Contextual","Contextual"),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  add_p(paste0(
    "Report generated: April 1, 2026 | ",
    "Dashboard: https://ezequielbassa.shinyapps.io/3Epidemiology/ | ",
    "Data: PAHO/EIH Open Data Core Indicators 2025"
  ))

# ── SAVE ───────────────────────────────────────────────────────────────────────
print(doc, target = OUT_FILE)
cat("\nReport saved to:", OUT_FILE, "\n")
cat("Charts saved to:", IMG_DIR, "\n")
