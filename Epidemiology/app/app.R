# ==============================================================================
#  PAHO Core Indicators — Americas Health Dashboard 2025
#  Interactive Epidemiological Dashboard
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Stack   : Shiny · bslib · plotly · DT
#  Data    : PAHO/EIH Open Data — Core Indicators 2025
# ==============================================================================

# ── 0. PACKAGES ────────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(glue)

# ── 1. DATA ────────────────────────────────────────────────────────────────────
if (!file.exists("paho_clean.rds")) {
  df_raw <- read.csv(
    "../sources/PAHO-Core-Indicators-2025-20251001.csv",
    stringsAsFactors = FALSE,
    na.strings        = c("", "NULL", "NA"),
    fileEncoding      = "UTF-8-BOM"
  )
  df_raw$numeric_value <- suppressWarnings(as.numeric(df_raw$numeric_value))
  df_raw$time_dim      <- suppressWarnings(as.integer(df_raw$time_dim))
  # Indicators where negative values are valid (economic / demographic)
  allowed_negative <- c(
    "Annual GDP growth (%)",
    "Annual population growth rate (%)",
    "Inflation (%)"
  )
  df_clean <- df_raw |>
    filter(!is.na(numeric_value), !is.na(time_dim)) |>
    select(
      indicator_id   = paho_indicator_id,
      indicator      = indicator_name,
      iso3           = spatial_dim,
      country        = spatial_dim_en,
      year           = time_dim,
      value          = numeric_value
    ) |>
    mutate(
      country = trimws(country),
      # Negative coverage/rate values are PAHO encoding for suppressed data → treat as NA
      value   = ifelse(value < 0 & !indicator %in% allowed_negative, NA_real_, value)
    ) |>
    filter(!is.na(value))
  saveRDS(df_clean, "paho_clean.rds")
}

df <- readRDS("paho_clean.rds")

ALL_COUNTRIES  <- sort(unique(df$country))
ALL_INDICATORS <- sort(unique(df$indicator))
YEAR_MIN       <- min(df$year, na.rm = TRUE)
YEAR_MAX       <- max(df$year, na.rm = TRUE)

# ── COUNTRY REGIONS ────────────────────────────────────────────────────────────
COUNTRY_GROUPS <- list(
  "North America" = c(
    "Bermuda",
    "Canada",
    "Mexico",
    "United States of America"
  ),
  "Central America" = c(
    "Belize",
    "Costa Rica",
    "El Salvador",
    "Guatemala",
    "Honduras",
    "Nicaragua",
    "Panama"
  ),
  "Caribbean" = c(
    "Anguilla",
    "Antigua and Barbuda",
    "Aruba",
    "Bahamas",
    "Barbados",
    "Cayman Islands",
    "Cuba",
    "Curaçao",
    "Dominica",
    "Dominican Republic",
    "Grenada",
    "Guadeloupe",
    "Haiti",
    "Jamaica",
    "Martinique",
    "Montserrat",
    "Puerto Rico",
    "Saint Kitts and Nevis",
    "Saint Lucia",
    "Saint Vincent and the Grenadines",
    "Sint Maarten (Dutch part)",
    "Trinidad and Tobago",
    "Turks and Caicos Islands",
    "Virgin Islands (British)",
    "Virgin Islands (U.S.)"
  ),
  "South America" = c(
    "Argentina",
    "Bolivia (the Plurinational State of)",
    "Brazil",
    "Chile",
    "Colombia",
    "Ecuador",
    "French Guiana",
    "Guyana",
    "Paraguay",
    "Peru",
    "Suriname",
    "Uruguay",
    "Venezuela (Bolivarian Republic of)"
  )
)

# Keep only countries present in the data
country_grouped_choices <- lapply(COUNTRY_GROUPS, function(ctries) {
  ctries[ctries %in% ALL_COUNTRIES]
})

# ── INDICATOR CATEGORIES ───────────────────────────────────────────────────────
CATEGORIES <- list(
  "Demography" = c(
    "Life expectancy at birth (years)",
    "Life expectancy at birth (years); female",
    "Life expectancy at birth (years); male",
    "Total fertility rate (live births per woman)",
    "Crude birth rate (1 000 pop)",
    "Crude death rate (1 000 pop)",
    "Annual population growth rate (%)",
    "Median age (years)",
    "Urban Population (%)",
    "Dependency ratio (100 pop)",
    "Population aged 65 and over (%)",
    "Population aged < 15 years (%)",
    "Adolescent fertility rate (births per 1 000 women aged 15-19 years)"
  ),
  "Mortality" = c(
    "General mortality rate (age-adjusted per 1 000 pop)",
    "Communicable diseases mortality rate (age-adjusted per 100 000 pop)",
    "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)",
    "External causes mortality rate (age-adjusted per 100 000 pop)",
    "Circulatory diseases mortality rate (age-adjusted per 100 000 pop)",
    "Ischemic heart diseases mortality rate (age-adjusted per 100 000 pop)",
    "Stroke diseases mortality rate (age-adjusted per 100 000 pop)",
    "Diabetes mellitus mortality rate (age-adjusted per 100 000 pop)",
    "Respiratory diseases mortality rate (age-adjusted per 100 000 pop)",
    "Homicide mortality rate (age-adjusted per 100 000 pop)",
    "Road injury mortality rate (age-adjusted per 100 000 pop)",
    "Suicide mortality rate (age-adjusted per 100 000 pop)"
  ),
  "Maternal & Child Health" = c(
    "Estimated maternal mortality ratio (100 000 lb)",
    "Infant mortality rate (1 000 lb)",
    "Neonatal mortality rate (1 000 lb)",
    "Under-five mortality (1 000 lb)",
    "Fetal mortality rate (1 000 births)",
    "Antenatal care coverage -at least 4 visits (%)",
    "Births attended by skilled health personnel (%)",
    "Low birthweight (<2 500 g) (%)",
    "Immunization coverage of 1 year old (%), MMR1",
    "Immunization coverage of under-1 year old (%), DTP3-cv",
    "Immunization coverage of under-1 year old (%), BCG",
    "Immunization coverage of under-1 year old (%), Polio 3",
    "Exclusive breastfeeding under 6 months (%)"
  ),
  "Communicable Diseases" = c(
    "Tuberculosis incidence rate (100 000 pop)",
    "Tuberculosis mortality rate (age-adjusted per 100 000 pop)",
    "HIV/AIDS mortality rate (age-adjusted per 100 000 pop)",
    "New HIV diagnoses rate (100 000 pop)",
    "Dengue cases",
    "Malaria cases",
    "Cholera cases",
    "Measles cases",
    "Annual parasite incidence (1 000 pop)",
    "Incidence of congenital syphilis (1 000 live births)"
  ),
  "NCD & Risk Factors" = c(
    "Prevalence of overweight and obesity in adults (%)",
    "Prevalence of obesity in children/adolescents aged 10-19 years (%)",
    "Prevalence of overweight in children under age 5 (%)",
    "Prevalence of raised blood glucose/diabetes in adults (%)",
    "Prevalence of raised blood pressure in adults (%)",
    "Prevalence of current tobacco use in adults (%)",
    "Prevalence of current tobacco use in adolescents (%)",
    "Total alcohol per capita (age 15+ years) consumption",
    "Prevalence of insufficient physical activity in adults (%)",
    "Malignant neoplasms mortality rate (age-adjusted per 100 000 pop)",
    "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)"
  ),
  "Health System" = c(
    "Density of medical doctors (10 000 pop)",
    "Density of nursing personnel (10 000 pop)",
    "Density of dentists (10 000 pop)",
    "Hospital beds ratio (1 000 pop)",
    "Hospital ICU beds ratio (100 000 pop)",
    "Public health expenditure as % of GDP",
    "Private health expenditure as % of GDP",
    "Out-of-pocket expenditure as % of current health expenditure",
    "Gross domestic product (US$ per capita), current international (PPP-adjusted)",
    "GINI Index",
    "Poverty headcount ratio at $ 2.15 day (2017 PPP) (% of population)"
  )
)

# Build grouped choices for selectInput
grouped_choices <- lapply(names(CATEGORIES), function(cat) {
  inds <- CATEGORIES[[cat]]
  inds[inds %in% ALL_INDICATORS]
})
names(grouped_choices) <- names(CATEGORIES)

# ── FEATURED KPIs (Overview) ───────────────────────────────────────────────────
FEATURED <- list(
  list(id = "life_exp",   ind = "Life expectancy at birth (years)",
       label = "Life Expectancy",  unit = "years",   fmt = "%.1f",  icon = "heartbeat"),
  list(id = "inf_mort",   ind = "Infant mortality rate (1 000 lb)",
       label = "Infant Mortality", unit = "per 1,000 lb", fmt = "%.1f", icon = "child"),
  list(id = "mat_mort",   ind = "Estimated maternal mortality ratio (100 000 lb)",
       label = "Maternal Mortality", unit = "per 100,000 lb", fmt = "%.0f", icon = "female"),
  list(id = "ncd_mort",   ind = "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)",
       label = "NCD Mortality",    unit = "per 100,000 pop", fmt = "%.0f", icon = "stethoscope"),
  list(id = "tb_inc",     ind = "Tuberculosis incidence rate (100 000 pop)",
       label = "TB Incidence",     unit = "per 100,000",     fmt = "%.1f", icon = "lungs"),
  list(id = "doc_dens",   ind = "Density of medical doctors (10 000 pop)",
       label = "Medical Doctors",  unit = "per 10,000 pop",  fmt = "%.1f", icon = "user-md")
)

# Palette
PALETTE <- c("#00b4d8","#f4a261","#06d6a0","#ef476f",
             "#ffd166","#9b5de5","#f15bb5","#00bbf9")

COUNTRY_COLORS <- setNames(
  rep(PALETTE, length.out = length(ALL_COUNTRIES)),
  ALL_COUNTRIES
)

# ── 2. THEME ───────────────────────────────────────────────────────────────────
theme_epi <- bs_theme(
  version      = 5,
  bg           = "#060d1a",
  fg           = "#e0eaf4",
  primary      = "#00b4d8",
  secondary    = "#f4a261",
  success      = "#06d6a0",
  warning      = "#ffd166",
  danger       = "#ef476f",
  info         = "#9b5de5",
  base_font    = font_google("Inter"),
  heading_font = font_google("Barlow Condensed", wght = 600),
  font_scale   = 0.92
) |>
  bs_add_rules("
    .sidebar { background-color: #030812 !important; }
    .card    { background-color: #0c1a2e !important;
               border: 1px solid #162640 !important; border-radius: 8px; }
    .card-header { background-color: #060d1a !important;
                   border-bottom: 1px solid #162640 !important;
                   color: #f4a261; font-size:.82rem;
                   text-transform: uppercase; letter-spacing:.07em; }
    .nav-tabs .nav-link.active { background-color: #00b4d8 !important;
                                  color: #060d1a !important; border-radius:4px;
                                  font-weight:600; }
    .nav-tabs .nav-link        { color: #5a8fa8 !important; }
    .form-label                { color: #5a8fa8; font-size:.78rem; }
    .form-select, .form-control{ background-color:#060d1a; border-color:#162640;
                                  color:#e0eaf4; }
    .irs--shiny .irs-bar       { background: #00b4d8; }
    .irs--shiny .irs-handle    { border-color: #f4a261; }
    h1,h2,h3,h4,h5,h6         { font-family:'Barlow Condensed',sans-serif; }
    .kpi-box { background: rgba(0,180,216,.08);
               border-left: 4px solid #00b4d8;
               border-radius: 6px; padding: 14px 18px; height:100%; }
    .kpi-label  { font-size:.68rem; text-transform:uppercase;
                  letter-spacing:.12em; color:#5a8fa8; }
    .kpi-value  { font-size:1.9rem; font-weight:700; line-height:1.1; }
    .kpi-sub    { font-size:.72rem; color:#3d5468; margin-top:2px; }
    .badge-cat  { font-size:.65rem; background:#162640; color:#00b4d8;
                  padding:2px 7px; border-radius:10px; }
    .dataTables_wrapper { color: #e0eaf4 !important; }
    table.dataTable td, table.dataTable th { color: #e0eaf4 !important;
                                             border-color: #162640 !important; }
    table.dataTable.stripe tbody tr.odd { background-color: #060d1a !important; }
    select[multiple] option { background-color: #0c1a2e; color: #e0eaf4; }
    .bootstrap-select .dropdown-menu { background-color: #0c1a2e !important;
                                       border: 1px solid #162640 !important; }
    .bootstrap-select .dropdown-menu li a { color: #e0eaf4 !important; }
    .bootstrap-select .dropdown-menu li a:hover,
    .bootstrap-select .dropdown-menu li.active a { background-color: #162640 !important;
                                                    color: #00b4d8 !important; }
    .bootstrap-select .dropdown-menu .divider { background-color: #162640 !important; }
    .bootstrap-select .dropdown-toggle { background-color: #060d1a !important;
                                         border-color: #162640 !important;
                                         color: #e0eaf4 !important; }
    .bootstrap-select .bs-searchbox input { background-color: #060d1a !important;
                                            border-color: #162640 !important;
                                            color: #e0eaf4 !important; }
    .bootstrap-select .bs-actionsbox .btn { background-color: #162640 !important;
                                            color: #00b4d8 !important;
                                            border-color: #162640 !important; }
    .bootstrap-select .dropdown-menu .dropdown-header { color: #f4a261 !important;
                                                         font-size: .68rem;
                                                         text-transform: uppercase;
                                                         letter-spacing: .1em; }
  ")

# ── 3. HELPERS ─────────────────────────────────────────────────────────────────
plt_layout <- function(p, title = "", xlab = "", ylab = "") {
  p |> layout(
    title         = list(text = title, font = list(color = "#e0eaf4", size = 13)),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color = "#8ab0c4", family = "Inter"),
    xaxis         = list(title = xlab, gridcolor = "#0f2035",
                         zerolinecolor = "#0f2035", titlefont = list(size = 11)),
    yaxis         = list(title = ylab, gridcolor = "#0f2035",
                         zerolinecolor = "#0f2035", titlefont = list(size = 11)),
    legend        = list(bgcolor = "rgba(0,0,0,0)",
                         font    = list(color = "#8ab0c4")),
    margin        = list(l = 55, r = 20, t = 40, b = 55),
    hoverlabel    = list(bgcolor = "#060d1a", bordercolor = "#f4a261",
                         font = list(color = "#e0eaf4"))
  )
}

kpi_box <- function(label, value, sub, color = "#00b4d8", icon_name = "chart-bar") {
  div(class = "kpi-box", style = glue("border-left-color:{color};"),
    div(class = "kpi-label",
        tags$i(class = glue("fa fa-{icon_name}"),
               style = glue("color:{color}; margin-right:5px;")),
        label),
    div(class = "kpi-value", style = glue("color:{color};"), value),
    div(class = "kpi-sub", sub)
  )
}

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

# ── 4. UI ──────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  theme = theme_epi,
  title = tags$span(
    tags$span(icon("globe-americas"),
              style = "color:#f4a261; margin-right:8px; vertical-align:middle;"),
    "Americas Health Core Indicators 2025"
  ),

  sidebar = sidebar(
    width = 255,
    open  = "open",

    tags$p(style = "color:#f4a261; font-size:.68rem; letter-spacing:.15em;
                    text-transform:uppercase; margin-bottom:10px;",
           "Global Filters"),

    pickerInput("s_countries", "Countries",
                choices  = country_grouped_choices,
                selected = ALL_COUNTRIES,
                multiple = TRUE,
                options  = pickerOptions(
                  actionsBox            = TRUE,
                  liveSearch            = TRUE,
                  liveSearchPlaceholder = "Search country…",
                  selectedTextFormat    = "count > 3",
                  countSelectedText     = "{0} countries selected",
                  selectAllText         = "Select All",
                  deselectAllText       = "Deselect All",
                  container             = "body"
                )),

    sliderInput("s_years", "Year range",
                min   = YEAR_MIN, max = YEAR_MAX,
                value = c(2000, YEAR_MAX),
                step  = 1, sep = ""),

    selectInput("s_map_year", "Map year (Overview)",
                choices  = rev(seq(2000, YEAR_MAX, 1)),
                selected = 2023),

    hr(style = "border-color:#162640; margin:12px 0;"),

    tags$div(style = "color:#3d5468; font-size:.68rem; line-height:1.8;",
      tags$strong(style = "color:#5a8fa8;", "Data source"), tags$br(),
      "PAHO/EIH Open Data", tags$br(),
      "Core Indicators 2025", tags$br(),
      tags$a(href = "https://www.paho.org/en/data-and-statistics",
             style = "color:#00b4d8;", "paho.org"),
      tags$br(), tags$br(),
      tags$strong(style = "color:#5a8fa8;", "Ezequiel Bassa"), tags$br(),
      "Senior Data Scientist", tags$br(),
      "& Sociologist", tags$br(),
      tags$a(href = "https://ezequielbassa.com",
             style = "color:#00b4d8;", "ezequielbassa.com")
    )
  ),

  navset_tab(

    # ── TAB 1: OVERVIEW ────────────────────────────────────────────────────────
    nav_panel(
      title = "Overview",
      icon  = icon("tachometer-alt"),

      layout_columns(
        fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
        uiOutput("kpi_life"),   uiOutput("kpi_infant"),
        uiOutput("kpi_maternal"), uiOutput("kpi_ncd"),
        uiOutput("kpi_tb"),    uiOutput("kpi_doctors")
      ),

      tags$br(),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header(
            tags$span("Americas Health Map"),
            tags$span(style = "float:right;",
              selectInput("map_indicator", NULL,
                          choices  = grouped_choices,
                          selected = "Life expectancy at birth (years)",
                          width    = "340px")
            )
          ),
          plotlyOutput("p_map", height = "460px")
        ),
        card(
          card_header("Top & Bottom Countries — selected indicator"),
          plotlyOutput("p_top_bottom", height = "460px")
        )
      )
    ),

    # ── TAB 2: TREND EXPLORER ─────────────────────────────────────────────────
    nav_panel(
      title = "Trend Explorer",
      icon  = icon("chart-line"),

      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Settings"),
          selectInput("trend_ind", "Indicator",
                      choices  = grouped_choices,
                      selected = "Life expectancy at birth (years)"),
          tags$div(style = "font-size:.75rem; color:#5a8fa8; margin-top:-6px; margin-bottom:8px;",
                   "Countries controlled by sidebar filter"),
          checkboxInput("trend_sex", "Split by sex (if available)", value = FALSE),
          hr(style = "border-color:#162640;"),
          DTOutput("tbl_trend", height = "280px")
        ),
        card(
          card_header("Trend over time — selected countries"),
          plotlyOutput("p_trend", height = "430px")
        )
      ),

      tags$br(),

      card(
        card_header("Country Comparison — most recent available year"),
        plotlyOutput("p_bar_compare", height = "300px")
      )
    ),

    # ── TAB 3: COUNTRY PROFILE ────────────────────────────────────────────────
    nav_panel(
      title = "Country Profile",
      icon  = icon("flag"),

      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header("Select Country"),
          selectInput("profile_country", NULL,
                      choices  = ALL_COUNTRIES,
                      selected = "Brazil"),
          hr(style = "border-color:#162640;"),
          uiOutput("profile_kpis")
        ),
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Life Expectancy & Mortality"),
                 plotlyOutput("p_profile1", height = "250px")),
            card(card_header("Maternal & Infant Mortality"),
                 plotlyOutput("p_profile2", height = "250px"))
          ),
          tags$br(),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Disease Burden: Communicable vs NCD"),
                 plotlyOutput("p_profile3", height = "250px")),
            card(card_header("Health System Indicators"),
                 plotlyOutput("p_profile4", height = "250px"))
          )
        )
      )
    ),

    # ── TAB 4: DISEASE BURDEN ─────────────────────────────────────────────────
    nav_panel(
      title = "Disease Burden",
      icon  = icon("virus"),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Communicable vs NCD Mortality Rate — latest year"),
          plotlyOutput("p_comm_ncd", height = "360px")
        ),
        card(
          card_header("Communicable Disease Cases — select disease"),
          selectInput("disease_sel", NULL,
                      choices = c("Dengue cases", "Malaria cases",
                                  "Cholera cases", "Measles cases",
                                  "Tuberculosis incidence rate (100 000 pop)"),
                      selected = "Dengue cases"),
          plotlyOutput("p_disease_trend", height = "300px")
        )
      ),

      tags$br(),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("HIV/AIDS — New diagnoses rate (per 100,000 pop)"),
          plotlyOutput("p_hiv", height = "300px")
        ),
        card(
          card_header("Tuberculosis Incidence Rate (per 100,000 pop)"),
          plotlyOutput("p_tb", height = "300px")
        )
      )
    ),

    # ── TAB 5: MATERNAL & CHILD HEALTH ────────────────────────────────────────
    nav_panel(
      title = "Maternal & Child",
      icon  = icon("baby"),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Under-5, Infant & Neonatal Mortality (per 1,000 live births)"),
          plotlyOutput("p_mch1", height = "340px")
        ),
        card(
          card_header("Maternal Mortality Ratio (per 100,000 live births)"),
          plotlyOutput("p_mch2", height = "340px")
        )
      ),

      tags$br(),

      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header("DTP3 Immunization Coverage (%)"),
          plotlyOutput("p_imm1", height = "270px")
        ),
        card(
          card_header("Births Attended by Skilled Personnel (%)"),
          plotlyOutput("p_mch3", height = "270px")
        ),
        card(
          card_header("Antenatal Care ≥4 Visits (%)"),
          plotlyOutput("p_mch4", height = "270px")
        )
      )
    ),

    # ── TAB 6: HEALTH SYSTEM ──────────────────────────────────────────────────
    nav_panel(
      title = "Health System",
      icon  = icon("hospital"),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Medical Doctors Density (per 10,000 pop) — latest year"),
          plotlyOutput("p_doctors", height = "360px")
        ),
        card(
          card_header("Health Expenditure — Public vs Out-of-pocket (% GDP)"),
          plotlyOutput("p_health_exp", height = "360px")
        )
      ),

      tags$br(),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Hospital Beds (per 1,000 pop) — latest year"),
          plotlyOutput("p_beds", height = "300px")
        ),
        card(
          card_header("GDP per Capita (PPP) vs Life Expectancy"),
          plotlyOutput("p_gdp_le", height = "300px")
        )
      )
    )
  )
)

# ── 5. SERVER ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive: filtered by sidebar countries & year range
  df_f <- reactive({
    df |>
      filter(
        country %in% input$s_countries,
        year >= input$s_years[1],
        year <= input$s_years[2]
      )
  })

  # Helper: latest value per country for an indicator (within filtered set)
  latest_country <- function(data, ind) {
    data |>
      filter(indicator == ind, !is.na(value)) |>
      group_by(country, iso3) |>
      slice_max(year, n = 1) |>
      ungroup()
  }

  # ── KPI CARDS ──────────────────────────────────────────────────────────────
  make_kpi <- function(feat) {
    renderUI({
      d <- df |>
        filter(indicator == feat$ind, country %in% input$s_countries,
               !is.na(value)) |>
        group_by(country) |>
        slice_max(year, n = 1) |>
        ungroup()
      if (nrow(d) == 0) return(kpi_box(feat$label, "N/A", "no data", "#5a8fa8", feat$icon))
      med_val   <- median(d$value, na.rm = TRUE)
      yr        <- max(d$year, na.rm = TRUE)
      kpi_box(
        feat$label,
        sprintf(feat$fmt, med_val),
        glue("{feat$unit} | median {yr}"),
        "#00b4d8",
        feat$icon
      )
    })
  }

  output$kpi_life     <- make_kpi(FEATURED[[1]])
  output$kpi_infant   <- make_kpi(FEATURED[[2]])
  output$kpi_maternal <- make_kpi(FEATURED[[3]])
  output$kpi_ncd      <- make_kpi(FEATURED[[4]])
  output$kpi_tb       <- make_kpi(FEATURED[[5]])
  output$kpi_doctors  <- make_kpi(FEATURED[[6]])

  # ── OVERVIEW MAP ───────────────────────────────────────────────────────────
  output$p_map <- renderPlotly({
    ind <- input$map_indicator
    yr  <- as.integer(input$s_map_year)
    d   <- df |>
      filter(indicator == ind, year == yr, !is.na(value))
    if (nrow(d) == 0) {
      d <- df |> filter(indicator == ind, !is.na(value)) |>
        group_by(country, iso3) |> slice_max(year, n = 1) |> ungroup()
    }
    plot_ly(
      d,
      type          = "choropleth",
      locations     = ~iso3,
      z             = ~value,
      text          = ~paste0("<b>", country, "</b><br>", ind, ": <b>",
                              round(value, 2), "</b><br>Year: ", year),
      hovertemplate = "%{text}<extra></extra>",
      colorscale    = list(c(0,"#ef476f"), c(.5,"#ffd166"), c(1,"#06d6a0")),
      colorbar      = list(
        title       = list(text = "Value", font = list(color="#e0eaf4", size=10)),
        tickfont    = list(color="#8ab0c4"),
        bgcolor     = "rgba(0,0,0,0)",
        bordercolor = "#162640"
      ),
      marker = list(line = list(color = "#162640", width = 0.8))
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        geo = list(
          scope          = "world",
          showframe      = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#162640",
          showland       = TRUE,
          landcolor      = "#0c1a2e",
          showocean      = TRUE,
          oceancolor     = "#030812",
          showcountries  = TRUE,
          countrycolor   = "#162640",
          bgcolor        = "rgba(0,0,0,0)",
          projection     = list(type = "mercator"),
          lataxis        = list(range = c(-60, 75)),
          lonaxis        = list(range = c(-175, -30))
        ),
        margin = list(l = 0, r = 0, t = 5, b = 0)
      )
  })

  # ── TOP / BOTTOM COUNTRIES ─────────────────────────────────────────────────
  output$p_top_bottom <- renderPlotly({
    ind <- input$map_indicator
    yr  <- as.integer(input$s_map_year)
    d   <- df |> filter(indicator == ind, year == yr, !is.na(value))
    if (nrow(d) == 0)
      d <- df |> filter(indicator == ind, !is.na(value)) |>
        group_by(country, iso3) |> slice_max(year, n = 1) |> ungroup()
    d <- d |> arrange(value)
    n <- nrow(d)
    if (n == 0) return(plot_ly() |> plt_layout())
    top5 <- tail(d, 5)
    bot5 <- head(d, 5)
    combined <- bind_rows(bot5, top5)
    colors    <- c(rep("#ef476f", nrow(bot5)), rep("#06d6a0", nrow(top5)))
    plot_ly(combined,
            x = ~value, y = ~reorder(country, value),
            type        = "bar", orientation = "h",
            marker      = list(color = colors),
            text        = ~round(value, 2), textposition = "outside",
            hovertemplate = "<b>%{y}</b>: %{x:.2f}<extra></extra>") |>
      plt_layout(xlab = ind)
  })

  # ── TREND EXPLORER ─────────────────────────────────────────────────────────
  output$p_trend <- renderPlotly({
    ind  <- input$trend_ind
    base <- df_f() |> filter(indicator == ind, !is.na(value))
    if (nrow(base) == 0) return(plot_ly() |> plt_layout("No data"))

    if (input$trend_sex) {
      fem_ind  <- paste0(ind, "; female")
      male_ind <- paste0(ind, "; male")
      extra <- df_f() |>
        filter(indicator %in% c(fem_ind, male_ind), !is.na(value)) |>
        mutate(country = paste0(country, " (", sub(".*; ", "", indicator), ")"))
      if (nrow(extra) > 0) base <- extra
    }

    p <- plot_ly()
    countries_in <- unique(base$country)
    for (i in seq_along(countries_in)) {
      d_c <- base |> filter(country == countries_in[i]) |> arrange(year)
      p <- add_trace(p, data = d_c, x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers",
                     name = countries_in[i],
                     line   = list(color = PALETTE[(i - 1) %% length(PALETTE) + 1],
                                   width = 2),
                     marker = list(color = PALETTE[(i - 1) %% length(PALETTE) + 1],
                                   size = 5),
                     hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                                            "Year: %{x}<br>Value: %{y:.2f}<extra></extra>"))
    }
    p |> plt_layout(xlab = "Year", ylab = ind)
  })

  output$p_bar_compare <- renderPlotly({
    ind <- input$trend_ind
    d   <- df_f() |>
      filter(indicator == ind, !is.na(value)) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      arrange(desc(value))
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    plot_ly(d,
            x = ~reorder(country, value), y = ~value,
            type   = "bar",
            marker = list(color  = ~value,
                          colorscale = list(c(0,"#ef476f"), c(.5,"#ffd166"), c(1,"#06d6a0")),
                          showscale = FALSE),
            text   = ~round(value, 2), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{y:.2f}<extra></extra>") |>
      plt_layout(xlab = "Country", ylab = ind) |>
      layout(xaxis = list(tickangle = -35))
  })

  output$tbl_trend <- renderDT({
    ind <- input$trend_ind
    d   <- df_f() |>
      filter(indicator == ind, !is.na(value)) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      select(Country = country, Year = year, Value = value) |>
      mutate(Value = round(Value, 3)) |>
      arrange(desc(Value))
    datatable(d, rownames = FALSE, class = "compact stripe",
              options = list(pageLength = 8, dom = "tip", scrollX = TRUE,
                initComplete = JS("function(s,j){$(this.api().table().container())
                  .css({'font-size':'10px','color':'#e0eaf4'});}")))
  })

  # ── COUNTRY PROFILE ────────────────────────────────────────────────────────
  output$profile_kpis <- renderUI({
    ctry <- input$profile_country
    kpis <- list(
      list(ind = "Life expectancy at birth (years)", lbl = "Life Expectancy",
           unit = "years", fmt = "%.1f", icon = "heartbeat", col = "#00b4d8"),
      list(ind = "Infant mortality rate (1 000 lb)", lbl = "Infant Mortality",
           unit = "per 1,000", fmt = "%.1f", icon = "child", col = "#ef476f"),
      list(ind = "Estimated maternal mortality ratio (100 000 lb)",
           lbl = "Maternal Mortality",
           unit = "per 100k", fmt = "%.0f", icon = "female", col = "#f4a261"),
      list(ind = "Density of medical doctors (10 000 pop)", lbl = "Doctors",
           unit = "per 10,000", fmt = "%.1f", icon = "user-md", col = "#06d6a0"),
      list(ind = "Public health expenditure as % of GDP", lbl = "Health Exp.",
           unit = "% of GDP", fmt = "%.1f", icon = "hospital", col = "#9b5de5"),
      list(ind = "Tuberculosis incidence rate (100 000 pop)", lbl = "TB Incidence",
           unit = "per 100k", fmt = "%.1f", icon = "lungs", col = "#ffd166")
    )
    tags$div(
      lapply(kpis, function(k) {
        v <- df |> filter(indicator == k$ind, country == ctry, !is.na(value)) |>
          slice_max(year, n = 1) |> pull(value)
        val <- if (length(v) == 0) "N/A" else sprintf(k$fmt, v[1])
        tags$div(style = "margin-bottom:8px;",
          kpi_box(k$lbl, val, k$unit, k$col, k$icon)
        )
      })
    )
  })

  profile_trend <- function(ind_list, label_list, ctry_input) {
    p <- plot_ly()
    for (i in seq_along(ind_list)) {
      d <- df |> filter(indicator == ind_list[i], country == ctry_input,
                        !is.na(value), year >= input$s_years[1],
                        year <= input$s_years[2]) |> arrange(year)
      if (nrow(d) == 0) next
      p <- add_trace(p, data = d, x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers",
                     name = label_list[i],
                     line = list(color = PALETTE[i], width = 2.2),
                     marker = list(color = PALETTE[i], size = 5),
                     hovertemplate = paste0("<b>", label_list[i], "</b><br>",
                                            "Year: %{x}<br>%{y:.2f}<extra></extra>"))
    }
    p |> plt_layout(xlab = "Year")
  }

  output$p_profile1 <- renderPlotly({
    profile_trend(
      c("Life expectancy at birth (years)",
        "Life expectancy at birth (years); female",
        "Life expectancy at birth (years); male"),
      c("Total", "Female", "Male"),
      input$profile_country
    )
  })

  output$p_profile2 <- renderPlotly({
    profile_trend(
      c("Estimated maternal mortality ratio (100 000 lb)",
        "Infant mortality rate (1 000 lb)",
        "Neonatal mortality rate (1 000 lb)"),
      c("Maternal Mortality", "Infant Mortality", "Neonatal Mortality"),
      input$profile_country
    )
  })

  output$p_profile3 <- renderPlotly({
    profile_trend(
      c("Communicable diseases mortality rate (age-adjusted per 100 000 pop)",
        "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)",
        "External causes mortality rate (age-adjusted per 100 000 pop)"),
      c("Communicable", "NCD", "External Causes"),
      input$profile_country
    )
  })

  output$p_profile4 <- renderPlotly({
    profile_trend(
      c("Density of medical doctors (10 000 pop)",
        "Hospital beds ratio (1 000 pop)",
        "Public health expenditure as % of GDP"),
      c("Doctors (per 10k)", "Hosp. Beds (per 1k)", "Health Exp. (% GDP)"),
      input$profile_country
    )
  })

  # ── DISEASE BURDEN ─────────────────────────────────────────────────────────
  output$p_comm_ncd <- renderPlotly({
    inds <- c("Communicable diseases mortality rate (age-adjusted per 100 000 pop)",
              "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)")
    d <- df_f() |>
      filter(indicator %in% inds, !is.na(value)) |>
      group_by(country, indicator) |> slice_max(year, n = 1) |> ungroup() |>
      mutate(ind_short = ifelse(grepl("^Communicable", indicator),
                                "Communicable", "NCD"))
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    plot_ly(d, x = ~country, y = ~value, color = ~ind_short,
            colors = c("Communicable" = "#00b4d8", "NCD" = "#ef476f"),
            type = "bar",
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}<extra></extra>") |>
      plt_layout(ylab = "Age-adjusted rate per 100,000") |>
      layout(barmode = "group",
             xaxis   = list(tickangle = -35, tickfont = list(size = 9)))
  })

  output$p_disease_trend <- renderPlotly({
    ind <- input$disease_sel
    d   <- df_f() |> filter(indicator == ind, !is.na(value)) |> arrange(year)
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    p <- plot_ly()
    for (i in seq_along(unique(d$country))) {
      ctry <- unique(d$country)[i]
      d_c  <- d |> filter(country == ctry)
      p    <- add_trace(p, data = d_c, x = ~year, y = ~value,
                        type = "scatter", mode = "lines",
                        name = ctry,
                        line = list(color = PALETTE[(i-1) %% length(PALETTE) + 1],
                                    width = 1.8),
                        hovertemplate = paste0("<b>", ctry, "</b><br>Year: %{x}<br>%{y:.1f}<extra></extra>"))
    }
    p |> plt_layout(xlab = "Year", ylab = ind)
  })

  output$p_hiv <- renderPlotly({
    ind <- "New HIV diagnoses rate (100 000 pop)"
    d   <- df_f() |> filter(indicator == ind, !is.na(value)) |> arrange(year)
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    p <- plot_ly()
    for (i in seq_along(unique(d$country))) {
      ctry <- unique(d$country)[i]
      d_c  <- d |> filter(country == ctry)
      p    <- add_trace(p, data = d_c, x = ~year, y = ~value,
                        type = "scatter", mode = "lines",
                        name = ctry,
                        line = list(color = PALETTE[(i-1) %% length(PALETTE) + 1],
                                    width = 1.8),
                        hovertemplate = paste0("<b>", ctry, "</b><br>Year: %{x}<br>%{y:.2f}<extra></extra>"))
    }
    p |> plt_layout(xlab = "Year", ylab = "New HIV diagnoses (per 100,000)")
  })

  output$p_tb <- renderPlotly({
    ind <- "Tuberculosis incidence rate (100 000 pop)"
    d   <- df_f() |> filter(indicator == ind, !is.na(value)) |> arrange(year)
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    p <- plot_ly()
    for (i in seq_along(unique(d$country))) {
      ctry <- unique(d$country)[i]
      d_c  <- d |> filter(country == ctry)
      p    <- add_trace(p, data = d_c, x = ~year, y = ~value,
                        type = "scatter", mode = "lines",
                        name = ctry,
                        line = list(color = PALETTE[(i-1) %% length(PALETTE) + 1],
                                    width = 1.8),
                        hovertemplate = paste0("<b>", ctry, "</b><br>Year: %{x}<br>%{y:.1f}<extra></extra>"))
    }
    p |> plt_layout(xlab = "Year", ylab = "TB incidence (per 100,000)")
  })

  # ── MATERNAL & CHILD HEALTH ────────────────────────────────────────────────
  mch_trend <- function(ind, ylab_txt) {
    d <- df_f() |> filter(indicator == ind, !is.na(value)) |> arrange(year)
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    p <- plot_ly()
    for (i in seq_along(unique(d$country))) {
      ctry <- unique(d$country)[i]
      d_c  <- d |> filter(country == ctry)
      p    <- add_trace(p, data = d_c, x = ~year, y = ~value,
                        type = "scatter", mode = "lines+markers",
                        name = ctry,
                        line = list(color = PALETTE[(i-1) %% length(PALETTE) + 1],
                                    width = 2),
                        marker = list(color = PALETTE[(i-1) %% length(PALETTE) + 1],
                                      size = 4),
                        hovertemplate = paste0("<b>", ctry, "</b><br>Year: %{x}<br>%{y:.1f}<extra></extra>"))
    }
    p |> plt_layout(xlab = "Year", ylab = ylab_txt)
  }

  output$p_mch1 <- renderPlotly({
    inds   <- c("Under-five mortality (1 000 lb)",
                "Infant mortality rate (1 000 lb)",
                "Neonatal mortality rate (1 000 lb)")
    labels <- c("Under-5 Mortality", "Infant Mortality", "Neonatal Mortality")
    d <- df_f() |> filter(indicator %in% inds, !is.na(value)) |>
      mutate(label = labels[match(indicator, inds)]) |> arrange(year)
    p <- plot_ly()
    for (i in seq_along(unique(d$label))) {
      lbl <- unique(d$label)[i]
      d_l <- d |> filter(label == lbl)
      for (j in seq_along(unique(d_l$country))) {
        ctry <- unique(d_l$country)[j]
        d_c  <- d_l |> filter(country == ctry) |> arrange(year)
        p <- add_trace(p, data = d_c, x = ~year, y = ~value,
                       type = "scatter", mode = "lines",
                       name = paste0(ctry, " — ", lbl),
                       legendgroup = lbl,
                       line = list(color = PALETTE[i], width = 1.8,
                                   dash = c("solid","dot","dash")[i]),
                       hovertemplate = paste0("<b>", ctry, "</b><br>", lbl,
                                              "<br>Year: %{x}<br>%{y:.1f}<extra></extra>"))
      }
    }
    p |> plt_layout(xlab = "Year", ylab = "Rate per 1,000 live births")
  })

  output$p_mch2 <- renderPlotly({
    mch_trend("Estimated maternal mortality ratio (100 000 lb)",
              "Maternal mortality (per 100,000 lb)")
  })

  output$p_imm1 <- renderPlotly({
    mch_trend("Immunization coverage of under-1 year old (%), DTP3-cv", "Coverage (%)")
  })

  output$p_mch3 <- renderPlotly({
    mch_trend("Births attended by skilled health personnel (%)", "Coverage (%)")
  })

  output$p_mch4 <- renderPlotly({
    mch_trend("Antenatal care coverage -at least 4 visits (%)", "Coverage (%)")
  })

  # ── HEALTH SYSTEM ──────────────────────────────────────────────────────────
  latest_bar <- function(ind, ylab_txt, color = "#00b4d8") {
    d <- df_f() |>
      filter(indicator == ind, !is.na(value)) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      arrange(desc(value))
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    plot_ly(d,
            x = ~reorder(country, value), y = ~value,
            type = "bar",
            marker = list(color = color, line = list(color="#162640", width=0.5)),
            text  = ~round(value, 2), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{y:.2f}<extra></extra>") |>
      plt_layout(xlab = "Country", ylab = ylab_txt) |>
      layout(xaxis = list(tickangle = -38, tickfont = list(size = 8)))
  }

  output$p_doctors <- renderPlotly({
    latest_bar("Density of medical doctors (10 000 pop)",
               "Doctors per 10,000 pop", "#00b4d8")
  })

  output$p_beds <- renderPlotly({
    latest_bar("Hospital beds ratio (1 000 pop)",
               "Beds per 1,000 pop", "#9b5de5")
  })

  output$p_health_exp <- renderPlotly({
    inds   <- c("Public health expenditure as % of GDP",
                "Out-of-pocket expenditure as % of current health expenditure")
    labels <- c("Public (% GDP)", "Out-of-pocket (% curr. health exp.)")
    d <- df_f() |>
      filter(indicator %in% inds, !is.na(value)) |>
      group_by(country, indicator) |> slice_max(year, n = 1) |> ungroup() |>
      mutate(label = labels[match(indicator, inds)])
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    plot_ly(d, x = ~country, y = ~value, color = ~label,
            colors = c("Public (% GDP)" = "#00b4d8",
                       "Out-of-pocket (% curr. health exp.)" = "#ef476f"),
            type = "bar",
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}<extra></extra>") |>
      plt_layout(ylab = "Value (%)") |>
      layout(barmode   = "group",
             xaxis     = list(tickangle = -35, tickfont = list(size = 8)))
  })

  output$p_gdp_le <- renderPlotly({
    gdp_ind <- "Gross domestic product (US$ per capita), current international (PPP-adjusted)"
    le_ind  <- "Life expectancy at birth (years)"
    gdp <- df_f() |> filter(indicator == gdp_ind, !is.na(value)) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      select(country, gdp = value)
    le  <- df_f() |> filter(indicator == le_ind, !is.na(value)) |>
      group_by(country) |> slice_max(year, n = 1) |> ungroup() |>
      select(country, le = value)
    d   <- inner_join(gdp, le, by = "country")
    if (nrow(d) == 0) return(plot_ly() |> plt_layout("No data"))
    plot_ly(d,
            x    = ~gdp, y = ~le,
            type = "scatter", mode = "markers+text",
            text = ~country, textposition = "top center",
            marker = list(size = 10, color = "#00b4d8",
                          line = list(color = "#f4a261", width = 1)),
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "GDP/capita: $%{x:,.0f}<br>",
              "Life expectancy: %{y:.1f} yrs<extra></extra>"
            )) |>
      plt_layout(xlab = "GDP per capita (PPP, current US$)",
                 ylab = "Life expectancy at birth (years)")
  })
}

# ── 6. LAUNCH ─────────────────────────────────────────────────────────────────
shinyApp(ui, server)
