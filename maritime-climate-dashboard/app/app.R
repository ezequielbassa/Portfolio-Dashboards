# ==============================================================================
#  Workplace Climate Survey — Maritime Logistics Americas 2026
#  Interactive Dashboard
#  Author  : Ezequiel Bassa | Senior Data Scientist & Sociologist
#  Stack   : Shiny · bslib · plotly · DT
# ==============================================================================

# ── 0. PACKAGES ────────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(glue)

# ── 1. DATA ────────────────────────────────────────────────────────────────────
csv_path <- ifelse(
  file.exists("../sources/workplace_climate_maritime_2026.csv"),
  "../sources/workplace_climate_maritime_2026.csv",
  "workplace_climate_maritime_2026.csv"
)
df_raw <- read.csv(
  csv_path,
  stringsAsFactors = FALSE,
  na.strings       = ""
)

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

# Derived tenure bands
df_raw <- df_raw |>
  mutate(
    Tenure_Band = case_when(
      Tenure_Years < 2            ~ "< 2 yrs",
      Tenure_Years <= 4           ~ "2-4 yrs",
      Tenure_Years <= 10          ~ "5-10 yrs",
      Tenure_Years <= 20          ~ "11-20 yrs",
      TRUE                        ~ "> 20 yrs"
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

# ── 2. THEME ───────────────────────────────────────────────────────────────────
theme_maritime <- bs_theme(
  version      = 5,
  bg           = "#0d1b2a",
  fg           = "#dde6ee",
  primary      = "#1a6fa8",
  secondary    = "#c9a84c",
  success      = "#2ecc71",
  warning      = "#f39c12",
  danger       = "#e74c3c",
  info         = "#3498db",
  base_font    = font_google("Inter"),
  heading_font = font_google("Barlow Condensed", wght = 600),
  font_scale   = 0.92
) |>
  bs_add_rules("
    .sidebar { background-color: #070f1a !important; }
    .card    { background-color: #101f30 !important;
               border: 1px solid #1a2e44 !important; border-radius: 8px; }
    .card-header { background-color: #0d1b2a !important;
                   border-bottom: 1px solid #1a2e44 !important;
                   color: #c9a84c; font-size:.82rem;
                   text-transform: uppercase; letter-spacing:.07em; }
    .nav-tabs .nav-link.active { background-color: #1a6fa8 !important;
                                  color: #fff !important; border-radius:4px; }
    .nav-tabs .nav-link        { color: #7fa8c5 !important; }
    .sidebar-title             { color: #c9a84c !important; font-weight:600; }
    .form-label                { color: #7fa8c5; font-size:.78rem; }
    .form-select, .form-control{ background-color:#0d1b2a; border-color:#1a2e44;
                                  color:#dde6ee; }
    .irs--shiny .irs-bar       { background: #1a6fa8; }
    .irs--shiny .irs-handle    { border-color: #c9a84c; }
    h1,h2,h3,h4,h5,h6         { font-family:'Barlow Condensed',sans-serif; }
    .kpi-box { background: rgba(26,111,168,.1);
               border-left: 4px solid #1a6fa8;
               border-radius: 6px; padding: 14px 18px; height:100%; }
    .kpi-label  { font-size:.68rem; text-transform:uppercase;
                  letter-spacing:.12em; color:#7fa8c5; }
    .kpi-value  { font-size:2rem; font-weight:700; line-height:1.1; }
    .kpi-sub    { font-size:.72rem; color:#556a7a; margin-top:2px; }
    .dataTables_wrapper { color: #dde6ee !important; }
    table.dataTable td, table.dataTable th { color: #dde6ee !important;
                                             border-color: #1a2e44 !important; }
    table.dataTable.stripe tbody tr.odd { background-color: #0d1b2a !important; }
  ")

# ── 3. HELPERS ─────────────────────────────────────────────────────────────────
plt_layout <- function(p, title = "", xlab = "", ylab = "") {
  p |> layout(
    title         = list(text = title, font = list(color = "#dde6ee", size = 14)),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color = "#9ab0c4", family = "Inter"),
    xaxis         = list(title = xlab, gridcolor = "#162336",
                         zerolinecolor = "#162336", titlefont = list(size = 11)),
    yaxis         = list(title = ylab, gridcolor = "#162336",
                         zerolinecolor = "#162336", titlefont = list(size = 11)),
    legend        = list(bgcolor = "rgba(0,0,0,0)",
                         font    = list(color = "#9ab0c4")),
    margin        = list(l = 55, r = 20, t = 45, b = 55),
    hoverlabel    = list(bgcolor = "#0d1b2a", bordercolor = "#c9a84c",
                         font = list(color = "#dde6ee"))
  )
}

score_col <- function(x, inv = FALSE) {
  if (inv) x <- 6 - x   # invert for stress (high = bad)
  dplyr::case_when(x >= 4  ~ "#2ecc71",
                   x >= 3  ~ "#f39c12",
                   TRUE     ~ "#e74c3c")
}

kpi_box <- function(label, value, sub, color = "#1a6fa8") {
  div(class = "kpi-box", style = glue("border-left-color:{color};"),
    div(class = "kpi-label", label),
    div(class = "kpi-value", style = glue("color:{color};"), value),
    div(class = "kpi-sub",   sub)
  )
}

PALETTE <- c("#1a6fa8", "#c9a84c", "#2ecc71", "#e74c3c",
             "#9b59b6", "#1abc9c", "#e67e22", "#3498db")

# ── 4. UI ──────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  theme = theme_maritime,
  title = tags$span(
    tags$span(icon("ship"),
              style = "color:#c9a84c; margin-right:8px; vertical-align:middle;"),
    "Maritime Workplace Climate Survey 2026"
  ),

  # ── SIDEBAR ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 240,
    open  = "open",

    tags$p(style = "color:#c9a84c; font-size:.68rem; letter-spacing:.15em;
                    text-transform:uppercase; margin-bottom:10px;",
           "Global Filters"),

    selectInput("s_region", "Region",
                c("All", sort(unique(df_raw$Region))), "All"),
    selectInput("s_dept",   "Department",
                c("All", sort(unique(df_raw$Department))), "All"),
    selectInput("s_gender", "Gender",
                c("All", sort(unique(df_raw$Gender))), "All"),
    selectInput("s_salary", "Salary Band",
                c("All", "Junior", "Mid", "Senior", "Executive"), "All"),
    selectInput("s_wtype",  "Work Type",
                c("All", sort(unique(df_raw$Work_Type))), "All"),

    sliderInput("s_tenure", "Tenure (years)",
                min = 0, max = 25, value = c(0, 25), step = 0.5),

    hr(style = "border-color:#1a2e44; margin:14px 0;"),

    tags$div(style = "color:#556a7a; font-size:.72rem; line-height:1.6;",
      tags$strong(style = "color:#7fa8c5;", "n = "),
      textOutput("n_total", inline = TRUE),
      tags$br(),
      "employees in selection"
    ),

    hr(style = "border-color:#1a2e44; margin:14px 0;"),

    tags$div(style = "color:#3d5468; font-size:.68rem; line-height:1.8;",
      tags$strong(style = "color:#556a7a;", "Ezequiel Bassa"), tags$br(),
      "Senior Data Scientist", tags$br(),
      "& Sociologist", tags$br(),
      tags$a(href = "https://ezequielbassa.com",
             style = "color:#1a6fa8;", "ezequielbassa.com")
    )
  ),

  # ── MAIN TABS ──────────────────────────────────────────────────────────────
  navset_tab(

    # ── TAB 1: OVERVIEW ──────────────────────────────────────────────────────
    nav_panel(
      title = "Overview",
      icon  = icon("tachometer-alt"),

      layout_columns(
        fill       = FALSE,
        col_widths = c(3, 3, 3, 3),
        uiOutput("kpi1"), uiOutput("kpi2"),
        uiOutput("kpi3"), uiOutput("kpi4")
      ),

      tags$br(),

      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Employees by Department"),
             plotlyOutput("p_dept_bar", height = "290px")),
        card(card_header("Work Type Mix"),
             plotlyOutput("p_wtype_donut", height = "290px"))
      ),

      tags$br(),

      layout_columns(
        col_widths = c(4, 4, 4),
        card(card_header("Regional Split"),
             plotlyOutput("p_region_pie", height = "250px")),
        card(card_header("Gender"),
             plotlyOutput("p_gender_bar", height = "250px")),
        card(card_header("Salary Band"),
             plotlyOutput("p_salary_bar", height = "250px"))
      )
    ),

    # ── TAB 2: CLIMATE INDICATORS ─────────────────────────────────────────────
    nav_panel(
      title = "Climate Indicators",
      icon  = icon("thermometer-half"),

      card(
        card_header("Average Score per Indicator — ranked low to high (Likert 1-5)"),
        plotlyOutput("p_lollipop", height = "490px")
      ),

      tags$br(),

      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Heatmap: Department x Indicator"),
          plotlyOutput("p_heatmap", height = "390px")
        ),
        card(
          card_header("Radar Chart — group comparison"),
          layout_columns(
            col_widths = c(6, 6),
            selectInput("radar_col", NULL,
                        c("Department", "Region", "Salary_Band", "Work_Type"),
                        "Department"),
            checkboxGroupInput("radar_grp", NULL, choices = NULL)
          ),
          plotlyOutput("p_radar", height = "320px")
        )
      )
    ),

    # ── TAB 3: DEPARTMENT ANALYSIS ────────────────────────────────────────────
    nav_panel(
      title = "Department Analysis",
      icon  = icon("building"),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Work-Life Balance vs Operational Stress (bubble = headcount)"),
          plotlyOutput("p_scatter", height = "350px")
        ),
        card(
          card_header("Distribution by Department — select indicator"),
          selectInput("dept_ind", NULL,
                      setNames(CLIMATE_COLS, FRIENDLY), "Engagement"),
          plotlyOutput("p_boxplot", height = "290px")
        )
      ),

      tags$br(),

      card(
        card_header(
          "Engagement & Retention Intent across Tenure Bands",
          tags$span(style = "float:right; color:#e74c3c; font-size:.75rem;",
                    icon("exclamation-triangle"), " Mid-career crisis zone highlighted")
        ),
        plotlyOutput("p_tenure_bias", height = "320px")
      )
    ),

    # ── TAB 4: REGIONAL VIEW ─────────────────────────────────────────────────
    nav_panel(
      title = "Regional View",
      icon  = icon("globe-americas"),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Average Composite Climate Score by Country"),
          plotlyOutput("p_country_bar", height = "380px")
        ),
        card(
          card_header("Indicator by Region — select"),
          selectInput("reg_ind", NULL,
                      setNames(CLIMATE_COLS, FRIENDLY), "Safety_Culture"),
          plotlyOutput("p_reg_ind", height = "310px")
        )
      ),

      tags$br(),

      card(
        card_header("Full Score Table by Country"),
        DTOutput("tbl_country")
      )
    ),

    # ── TAB 5: WORKFORCE PROFILE ─────────────────────────────────────────────
    nav_panel(
      title = "Workforce Profile",
      icon  = icon("users"),

      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Tenure Distribution"),
             plotlyOutput("p_tenure_hist", height = "290px")),
        card(card_header("Age Distribution"),
             plotlyOutput("p_age_hist",    height = "290px"))
      ),

      tags$br(),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Avg Engagement by Tenure Band (95% CI)"),
          plotlyOutput("p_eng_tenure", height = "290px")
        ),
        card(
          card_header("Environmental Pride by Age Group (violin)"),
          plotlyOutput("p_env_age",    height = "290px")
        )
      )
    ),

    # ── TAB 6: OPEN COMMENTS ─────────────────────────────────────────────────
    nav_panel(
      title = "Open Comments",
      icon  = icon("comments"),

      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header("Filters"),
          sliderInput("cmt_score", "Min avg score:",
                      min = 1, max = 5, value = 1, step = 0.1),
          selectInput("cmt_dept", "Department:",
                      c("All", sort(unique(df_raw$Department))), "All"),
          selectInput("cmt_region", "Region:",
                      c("All", sort(unique(df_raw$Region))), "All"),
          hr(style = "border-color:#1a2e44;"),
          tags$div(style = "font-size:.8rem; color:#7fa8c5;",
                   textOutput("n_comments"))
        ),
        card(
          card_header("Employee Open Responses — hover for full text"),
          DTOutput("tbl_comments")
        )
      )
    ),

    # ── TAB 7: AMERICAS MAP ───────────────────────────────────────────────────
    nav_panel(
      title = "Americas Map",
      icon  = icon("map"),

      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header("Map Settings"),
          selectInput("map_kpi", "KPI to display:",
                      c("Overall Composite Score" = "Avg_Climate",
                        setNames(CLIMATE_COLS, FRIENDLY)),
                      "Avg_Climate"),
          hr(style = "border-color:#1a2e44;"),
          tags$div(style = "font-size:.78rem; color:#7fa8c5; line-height:1.7;",
            tags$p("Average score per country."),
            tags$p(icon("circle", style="color:#27ae60"), " ≥ 4.0  Good"),
            tags$p(icon("circle", style="color:#f39c12"), " 3.0 – 3.9  Moderate"),
            tags$p(icon("circle", style="color:#c0392b"), " < 3.0  Critical")
          )
        ),
        card(
          card_header("Workplace Climate KPI by Country — Americas 2026"),
          plotlyOutput("p_map", height = "540px")
        )
      )
    )
  )
)

# ── 5. SERVER ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # -- Reactive filtered dataset ----------------------------------------------
  df <- reactive({
    d <- df_raw
    if (input$s_region != "All") d <- filter(d, Region      == input$s_region)
    if (input$s_dept   != "All") d <- filter(d, Department  == input$s_dept)
    if (input$s_gender != "All") d <- filter(d, Gender      == input$s_gender)
    if (input$s_salary != "All") d <- filter(d, Salary_Band == input$s_salary)
    if (input$s_wtype  != "All") d <- filter(d, Work_Type   == input$s_wtype)
    filter(d,
           Tenure_Years >= input$s_tenure[1],
           Tenure_Years <= input$s_tenure[2])
  })

  output$n_total <- renderText(format(nrow(df()), big.mark = ","))

  # -- KPI Cards --------------------------------------------------------------
  output$kpi1 <- renderUI({
    kpi_box("Total Employees",
            format(nrow(df()), big.mark = ","),
            "in filtered selection", "#1a6fa8")
  })
  output$kpi2 <- renderUI({
    v <- round(mean(df()$Engagement, na.rm = TRUE), 2)
    kpi_box("Avg Engagement", v, "out of 5.0", score_col(v))
  })
  output$kpi3 <- renderUI({
    v <- round(mean(df()$Retention_Intent, na.rm = TRUE), 2)
    kpi_box("Avg Retention Intent", v, "out of 5.0", score_col(v))
  })
  output$kpi4 <- renderUI({
    v <- round(mean(df()$Operational_Stress, na.rm = TRUE), 2)
    kpi_box("Avg Operational Stress", v, "higher = more stress",
            score_col(v, inv = TRUE))
  })

  # -- Overview ---------------------------------------------------------------
  output$p_dept_bar <- renderPlotly({
    d <- count(df(), Department) |> arrange(n)
    plot_ly(d, x = ~n, y = ~reorder(Department, n), type = "bar",
            orientation = "h",
            marker = list(color = PALETTE[1],
                          line = list(color = "#c9a84c", width = .6)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>%{x} employees<extra></extra>") |>
      plt_layout(xlab = "Employees")
  })

  output$p_wtype_donut <- renderPlotly({
    d <- count(df(), Work_Type)
    plot_ly(d, labels = ~Work_Type, values = ~n, type = "pie", hole = .52,
            marker = list(colors = PALETTE[1:3],
                          line   = list(color = "#0d1b2a", width = 2)),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>%{value}<extra></extra>") |>
      plt_layout()
  })

  output$p_region_pie <- renderPlotly({
    d <- count(df(), Region)
    plot_ly(d, labels = ~Region, values = ~n, type = "pie",
            marker = list(colors = PALETTE[1:3],
                          line   = list(color = "#0d1b2a", width = 2)),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>%{value}<extra></extra>") |>
      plt_layout()
  })

  output$p_gender_bar <- renderPlotly({
    d <- count(df(), Gender)
    plot_ly(d, x = ~Gender, y = ~n, type = "bar",
            marker = list(color = PALETTE[1:nrow(d)]),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{y}<extra></extra>") |>
      plt_layout(ylab = "Count")
  })

  output$p_salary_bar <- renderPlotly({
    lvls <- c("Junior", "Mid", "Senior", "Executive")
    d <- df() |>
      mutate(Salary_Band = factor(Salary_Band, lvls)) |>
      count(Salary_Band)
    plot_ly(d, x = ~Salary_Band, y = ~n, type = "bar",
            marker = list(color = PALETTE[1],
                          line  = list(color = "#c9a84c", width = .8)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{y}<extra></extra>") |>
      plt_layout(ylab = "Count")
  })

  # -- Climate Indicators -----------------------------------------------------
  output$p_lollipop <- renderPlotly({
    d <- df() |>
      summarise(across(all_of(CLIMATE_COLS), function(x) mean(x, na.rm = TRUE))) |>
      pivot_longer(everything(), names_to = "col", values_to = "score") |>
      mutate(label = FRIENDLY[col],
             color = score_col(score)) |>
      arrange(score)

    plot_ly(d) |>
      add_segments(
        x = 1, xend = ~score,
        y = ~reorder(label, score), yend = ~reorder(label, score),
        line = list(color = "#1a2e44", width = 2),
        showlegend = FALSE
      ) |>
      add_markers(
        x = ~score, y = ~reorder(label, score),
        marker = list(color = ~color, size = 14,
                      line = list(color = "#0d1b2a", width = 1.5)),
        text = ~glue("{label}: {round(score,2)}"),
        hovertemplate = "%{text}<extra></extra>"
      ) |>
      add_annotations(
        x = ~score + 0.04, y = ~reorder(label, score),
        text = ~round(score, 2), showarrow = FALSE,
        font = list(color = "#9ab0c4", size = 10), xanchor = "left"
      ) |>
      plt_layout(xlab = "Average Score (1-5)") |>
      layout(xaxis = list(range = c(0.8, 5.4)))
  })

  output$p_heatmap <- renderPlotly({
    d <- df() |>
      group_by(Department) |>
      summarise(across(all_of(CLIMATE_COLS), function(x) round(mean(x, na.rm=TRUE), 2)),
                .groups = "drop")

    z   <- as.matrix(d[, CLIMATE_COLS])
    rownames(z) <- d$Department
    colnames(z) <- FRIENDLY[CLIMATE_COLS]

    plot_ly(
      x = colnames(z), y = rownames(z), z = z,
      type      = "heatmap",
      colorscale = list(c(0,"#c0392b"), c(.5,"#f39c12"), c(1,"#27ae60")),
      zmin = 1, zmax = 5,
      text = z,
      hovertemplate = "<b>%{y}</b><br>%{x}: <b>%{z:.2f}</b><extra></extra>"
    ) |>
      plt_layout() |>
      layout(xaxis = list(tickangle = -42, tickfont = list(size = 9)))
  })

  observeEvent(input$radar_col, {
    ch <- sort(unique(df_raw[[input$radar_col]]))
    updateCheckboxGroupInput(session, "radar_grp",
                             choices  = ch,
                             selected = ch[seq_len(min(3L, length(ch)))])
  })

  output$p_radar <- renderPlotly({
    req(input$radar_grp)
    gc <- input$radar_col
    rc <- CLIMATE_COLS[CLIMATE_COLS != "Operational_Stress"]
    rl <- FRIENDLY[rc]

    d <- df() |>
      filter(.data[[gc]] %in% input$radar_grp) |>
      group_by(.data[[gc]]) |>
      summarise(across(all_of(rc), function(x) mean(x, na.rm=TRUE)), .groups="drop")

    p <- plot_ly(type = "scatterpolar", fill = "toself")
    for (i in seq_len(nrow(d))) {
      v <- as.numeric(d[i, rc])
      p <- add_trace(p,
                     r         = c(v, v[1]),
                     theta     = c(rl, rl[1]),
                     name      = d[[gc]][i],
                     line      = list(color = PALETTE[i], width = 2),
                     fillcolor = paste0(PALETTE[i], "25"))
    }
    p |> layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      polar = list(
        bgcolor    = "rgba(0,0,0,0)",
        radialaxis = list(visible = TRUE, range = c(0,5),
                          gridcolor = "#162336", color = "#7fa8c5",
                          tickfont  = list(size = 8)),
        angularaxis = list(gridcolor = "#162336", color = "#9ab0c4",
                           tickfont  = list(size = 8))
      ),
      legend = list(bgcolor = "rgba(0,0,0,0)",
                    font    = list(color = "#9ab0c4", size = 9)),
      margin = list(l = 55, r = 55, t = 20, b = 20)
    )
  })

  # -- Department Analysis ----------------------------------------------------
  output$p_scatter <- renderPlotly({
    d <- df() |>
      group_by(Department) |>
      summarise(WLB    = mean(Work_Life_Balance, na.rm=TRUE),
                Stress = mean(Operational_Stress, na.rm=TRUE),
                n      = n(), .groups = "drop")

    plot_ly(d, x = ~WLB, y = ~Stress, type = "scatter", mode = "markers+text",
            text          = ~Department,
            textposition  = "top center",
            marker        = list(
              size  = ~sqrt(n) * 2.5,
              color = PALETTE[seq_len(nrow(d))],
              line  = list(color = "#c9a84c", width = 1.2),
              opacity = .85
            ),
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Work-Life Balance: %{x:.2f}<br>",
              "Operational Stress: %{y:.2f}<extra></extra>"
            )) |>
      plt_layout(
        xlab = "Work-Life Balance (avg)",
        ylab = "Operational Stress (avg)"
      ) |>
      layout(shapes = list(
        list(type="line", x0=3, x1=3, y0=0, y1=6,
             line=list(color="#c9a84c55", dash="dot", width=1.2)),
        list(type="line", x0=0, x1=6, y0=3, y1=3,
             line=list(color="#c9a84c55", dash="dot", width=1.2))
      ))
  })

  output$p_boxplot <- renderPlotly({
    ind <- input$dept_ind
    d   <- df() |> select(Department, val = all_of(ind))
    plot_ly(d, x = ~Department, y = ~val, type = "box",
            color     = ~Department, colors = PALETTE,
            boxpoints = "outliers", jitter = .3, pointpos = 0,
            marker    = list(size = 3, opacity = .5),
            hovertemplate = "<b>%{x}</b><br>%{y}<extra></extra>") |>
      plt_layout(ylab = FRIENDLY[ind]) |>
      layout(showlegend = FALSE,
             xaxis = list(tickangle = -18))
  })

  output$p_tenure_bias <- renderPlotly({
    d <- df() |>
      group_by(Tenure_Band) |>
      summarise(Engagement       = mean(Engagement, na.rm=TRUE),
                Retention_Intent = mean(Retention_Intent, na.rm=TRUE),
                .groups = "drop")

    plot_ly(d, x = ~Tenure_Band) |>
      add_bars(y = ~Engagement, name = "Engagement",
               marker = list(color = PALETTE[1])) |>
      add_bars(y = ~Retention_Intent, name = "Retention Intent",
               marker = list(color = PALETTE[2])) |>
      plt_layout(ylab = "Average Score") |>
      layout(
        barmode = "group",
        yaxis   = list(range = c(0, 5)),
        shapes  = list(list(
          type = "rect",
          x0   = 0.5, x1 = 1.5, y0 = 0, y1 = 5,
          fillcolor = "rgba(231,76,60,.08)",
          line = list(color = "#e74c3c", dash = "dot", width = 1)
        )),
        annotations = list(list(
          x = 1, y = 4.85,
          text = "Mid-career crisis zone",
          showarrow = FALSE,
          font = list(color = "#e74c3c", size = 10)
        ))
      )
  })

  # -- Regional View ----------------------------------------------------------
  output$p_country_bar <- renderPlotly({
    reg_pal <- c(
      "North America"   = PALETTE[1],
      "Central America" = PALETTE[2],
      "South America"   = PALETTE[3]
    )
    d <- df() |>
      group_by(Country, Region) |>
      summarise(avg = mean(Avg_Climate, na.rm=TRUE),
                n   = n(), .groups = "drop") |>
      arrange(desc(avg))

    plot_ly(d, x = ~reorder(Country, avg), y = ~avg,
            type  = "bar",
            color = ~Region, colors = reg_pal,
            text  = ~round(avg, 2), textposition = "outside",
            hovertemplate = paste0(
              "<b>%{x}</b><br>",
              "Avg Score: %{y:.2f}<extra></extra>"
            )) |>
      plt_layout(xlab = "Country", ylab = "Avg Composite Score") |>
      layout(yaxis = list(range = c(0, 5.2)))
  })

  output$p_reg_ind <- renderPlotly({
    ind <- input$reg_ind
    d   <- df() |>
      group_by(Region) |>
      summarise(score = mean(.data[[ind]], na.rm=TRUE), .groups="drop")

    plot_ly(d, x = ~Region, y = ~score, type = "bar",
            marker = list(color = PALETTE[seq_len(nrow(d))]),
            text   = ~round(score, 2), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{y:.2f}<extra></extra>") |>
      plt_layout(ylab = FRIENDLY[ind]) |>
      layout(yaxis = list(range = c(0, 5.2)))
  })

  output$tbl_country <- renderDT({
    d <- df() |>
      group_by(Country, Region) |>
      summarise(Employees = n(),
                across(all_of(CLIMATE_COLS),
                       function(x) round(mean(x, na.rm=TRUE), 2)),
                .groups = "drop") |>
      arrange(Region, Country) |>
      rename_with(function(x) FRIENDLY[x], all_of(CLIMATE_COLS))

    datatable(
      d,
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(
        scrollX    = TRUE,
        pageLength = 10,
        dom        = "ftip",
        initComplete = JS(
          "function(s,j){$(this.api().table().container())
           .css({'font-size':'11px','color':'#dde6ee'});}"
        )
      )
    )
  })

  # -- Workforce Profile ------------------------------------------------------
  output$p_tenure_hist <- renderPlotly({
    plot_ly(df(), x = ~Tenure_Years, type = "histogram",
            nbinsx = 30,
            marker = list(color = PALETTE[1],
                          line  = list(color = "#0d1b2a", width = .4)),
            hovertemplate = "Tenure: %{x} yrs<br>Count: %{y}<extra></extra>") |>
      plt_layout(xlab = "Tenure (years)", ylab = "Count")
  })

  output$p_age_hist <- renderPlotly({
    plot_ly(df(), x = ~Age, type = "histogram",
            nbinsx = 30,
            marker = list(color = PALETTE[2],
                          line  = list(color = "#0d1b2a", width = .4)),
            hovertemplate = "Age: %{x}<br>Count: %{y}<extra></extra>") |>
      plt_layout(xlab = "Age", ylab = "Count")
  })

  output$p_eng_tenure <- renderPlotly({
    d <- df() |>
      group_by(Tenure_Band) |>
      summarise(m  = mean(Engagement, na.rm=TRUE),
                sd = sd(Engagement,   na.rm=TRUE),
                n  = n(), .groups="drop") |>
      mutate(se = sd / sqrt(n), ci = se * 1.96)

    plot_ly(d, x = ~Tenure_Band, y = ~m, type = "scatter",
            mode = "lines+markers",
            error_y = list(array = ~ci, color = PALETTE[2], thickness = 1.5),
            line   = list(color = PALETTE[1], width = 2.5),
            marker = list(color = PALETTE[2], size = 9,
                          line = list(color = PALETTE[1], width = 1.5)),
            hovertemplate = paste0(
              "<b>%{x}</b><br>",
              "Engagement: %{y:.2f} (±95% CI)<extra></extra>"
            )) |>
      plt_layout(xlab = "Tenure Band", ylab = "Avg Engagement") |>
      layout(yaxis = list(range = c(1, 5)))
  })

  output$p_env_age <- renderPlotly({
    plot_ly(df(), x = ~Age_Group, y = ~Environmental_Pride,
            type = "violin", color = ~Age_Group, colors = PALETTE[1:4],
            box      = list(visible = TRUE),
            meanline = list(visible = TRUE, color = "#fff"),
            hovertemplate = "<b>%{x}</b><br>Score: %{y}<extra></extra>") |>
      plt_layout(xlab = "Age Group", ylab = "Environmental Pride") |>
      layout(showlegend = FALSE)
  })

  # -- Open Comments ----------------------------------------------------------
  df_cmt <- reactive({
    d <- df_raw |>
      filter(!is.na(Open_Comments), nchar(trimws(Open_Comments)) > 0)
    if (input$cmt_dept   != "All") d <- filter(d, Department == input$cmt_dept)
    if (input$cmt_region != "All") d <- filter(d, Region     == input$cmt_region)
    filter(d, Avg_Climate >= input$cmt_score)
  })

  output$n_comments <- renderText({
    glue("{nrow(df_cmt())} comments match current filters")
  })

  output$tbl_comments <- renderDT({
    d <- df_cmt() |>
      select(Dept       = Department,
             Region,
             Tenure     = Tenure_Years,
             Avg_Score  = Avg_Climate,
             Comment    = Open_Comments) |>
      mutate(Avg_Score = round(Avg_Score, 2)) |>
      arrange(Avg_Score)

    datatable(
      d,
      rownames  = FALSE,
      class     = "compact stripe",
      escape    = FALSE,
      options   = list(
        pageLength = 8,
        dom        = "ftip",
        scrollX    = TRUE,
        columnDefs = list(
          list(width = "380px", targets = 4),
          list(className = "dt-center", targets = c(2, 3))
        ),
        initComplete = JS(
          "function(s,j){$(this.api().table().container())
           .css({'font-size':'11px','color':'#dde6ee'});}"
        )
      )
    ) |>
      formatStyle(
        "Avg_Score",
        background         = styleColorBar(c(1, 5), "#1a6fa8"),
        backgroundSize     = "90% 65%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle(
        "Avg_Score",
        color = styleInterval(c(2.5, 3.5), c("#e74c3c", "#f39c12", "#2ecc71"))
      )
  })
  # -- Americas Map -----------------------------------------------------------
  COUNTRY_ISO <- c(
    "USA"        = "USA",
    "Canada"     = "CAN",
    "Mexico"     = "MEX",
    "Panama"     = "PAN",
    "Costa Rica" = "CRI",
    "Brazil"     = "BRA",
    "Chile"      = "CHL",
    "Argentina"  = "ARG",
    "Peru"       = "PER",
    "Colombia"   = "COL"
  )

  output$p_map <- renderPlotly({
    kpi       <- input$map_kpi
    kpi_label <- if (kpi == "Avg_Climate") "Overall Composite Score" else FRIENDLY[kpi]

    d <- df() |>
      group_by(Country) |>
      summarise(score = round(mean(.data[[kpi]], na.rm = TRUE), 2),
                n     = n(), .groups = "drop") |>
      mutate(iso = COUNTRY_ISO[Country])

    plot_ly(
      d,
      type          = "choropleth",
      locations     = ~iso,
      z             = ~score,
      text          = ~paste0("<b>", Country, "</b><br>",
                              kpi_label, ": <b>", score, "</b><br>",
                              "n = ", format(n, big.mark = ",")),
      hovertemplate = "%{text}<extra></extra>",
      colorscale    = list(c(0, "#c0392b"), c(0.5, "#f39c12"), c(1, "#27ae60")),
      zmin = 1, zmax = 5,
      colorbar = list(
        title       = list(text = kpi_label,
                           font = list(color = "#dde6ee", size = 11)),
        tickfont    = list(color = "#9ab0c4"),
        bgcolor     = "rgba(0,0,0,0)",
        bordercolor = "#1a2e44"
      ),
      marker = list(line = list(color = "#1a2e44", width = 0.8))
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        geo = list(
          scope          = "world",
          showframe      = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#1a2e44",
          showland       = TRUE,
          landcolor      = "#101f30",
          showocean      = TRUE,
          oceancolor     = "#070f1a",
          showcountries  = TRUE,
          countrycolor   = "#1a2e44",
          bgcolor        = "rgba(0,0,0,0)",
          projection     = list(type = "mercator"),
          lataxis        = list(range = c(-60, 75)),
          lonaxis        = list(range = c(-170, -30))
        ),
        margin = list(l = 0, r = 0, t = 10, b = 0)
      )
  })
}

# ── 6. LAUNCH ─────────────────────────────────────────────────────────────────
shinyApp(ui, server)
