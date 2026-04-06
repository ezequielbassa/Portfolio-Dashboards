# Americas Health Core Indicators Dashboard 2025

An interactive epidemiological intelligence platform built with R Shiny, transforming PAHO's Core Indicators dataset into actionable public health insights across 49 countries and territories in the Americas.

---

## Project Overview

This project processes and visualizes the **Pan American Health Organization (PAHO) Core Indicators 2025** — one of the most comprehensive public health open data sources in the Western Hemisphere. The dashboard enables researchers, policymakers, and public health professionals to explore population health trends, compare countries, and analyze disease burden without requiring technical expertise.

**Live Dashboard:** [ezequielbassa.shinyapps.io/3Epidemiology](https://ezequielbassa.shinyapps.io/3Epidemiology/)

---

## Dataset

| Property | Detail |
|---|---|
| Source | PAHO/EIH Open Data — Core Indicators 2025 |
| Records | 337,970 |
| Health indicators | 299 |
| Countries / territories | 49 (all Americas regions) |
| Time span | Multi-decade (primary analysis 2000–2023) |
| Raw file | `PAHO-Core-Indicators-2025-20251001.csv` (155 MB — not included in repo) |
| Processed file | `app/paho_clean.rds` — cleaned and cached at app startup |

### Data Pipeline

The raw CSV is parsed, cleaned, and saved as an optimized RDS file on first run:

- Columns selected: `indicator_id`, `indicator`, `iso3`, `country`, `year`, `value`
- Negative values in non-economic indicators treated as `NA` (PAHO encoding for suppressed data)
- Economic indicators with legitimate negatives retained: GDP growth, population growth rate, inflation

### Thematic Domains

| Domain | Example Indicators |
|---|---|
| Demographics | Population, urbanization, dependency ratio |
| Mortality | Life expectancy, all-cause mortality, age-adjusted rates |
| Maternal & Child | Maternal mortality ratio, infant mortality, immunization coverage |
| Communicable Diseases | Dengue, malaria, tuberculosis, HIV/AIDS, cholera, measles |
| Non-Communicable Diseases | NCD mortality, diabetes, cardiovascular, obesity, risk factors |
| Health Systems | Doctors per 10,000, hospital beds, health expenditure, out-of-pocket spending |

---

## Dashboard

Built with **R Shiny** using a dark epidemiological theme (`#0d1b2a` background, `#1a6fa8` primary, `#c9a84c` accent).

### Tabs

| # | Tab | Description |
|---|---|---|
| 1 | **Overview** | 6 KPI cards, interactive Americas choropleth map, top 5 / bottom 5 country ranking |
| 2 | **Trend Explorer** | Multi-country time-series for any of 299 indicators, sex-disaggregated toggle, comparison bar chart |
| 3 | **Country Profile** | Individual country health profile — 6 KPIs + 4 mini trend charts |
| 4 | **Disease Burden** | Communicable vs NCD mortality, disease-specific trends (dengue, malaria, TB, HIV), cholera, measles |
| 5 | **Maternal & Child** | Child mortality trends, maternal mortality ratio, immunization coverage, antenatal care |
| 6 | **Health System** | Medical doctors density, health expenditure breakdown, hospital beds, GDP vs life expectancy scatter |

### Global Sidebar Filters

Countries (grouped by region: North America · Central America · Caribbean · South America) · Year range slider

---

## Tech Stack

### Dashboard
| Tool | Purpose |
|---|---|
| R | Core language |
| `shiny` | Web application framework |
| `bslib` | Bootstrap 5 theme and layout |
| `shinyWidgets` | Grouped country picker input |
| `plotly` | Interactive charts and choropleth map |
| `DT` | Interactive data tables |
| `dplyr` / `tidyr` | Data wrangling and pipeline |
| `scales` / `glue` | Formatting helpers |

### Deployment
| Service | Purpose |
|---|---|
| shinyapps.io | Dashboard hosting |
| Framer | Portfolio website with iframe embed |
| GitHub | Version control |

---

## Quality Assurance

A 5-pillar QA audit was completed on 2026-04-01:

| Pillar | Result |
|---|---|
| Data Integrity (21 validate rules) | PASS — 0 violations across 337,970 records |
| Unit Testing | PASS — 25 / 25 tests passed |
| Shiny UI Testing | Pending — WSL limitation (covered by UAT checklist) |
| Performance Benchmarking | PASS — all reactive computations < 500ms |
| UAT Checklist | 12 / 13 High priority PASS · 1 bug identified and fixed |

**Bug fixed:** 722 records with negative coverage values (PAHO encoding for suppressed data) were treated as `NA` and removed from all visualizations.

QA outputs are in `qa/qa_output/`:
- `paho_qa_report.xlsx` — 12-tab validation workbook
- `PAHO_Dashboard_QA_Report.docx` — stakeholder QA report

---

## Comparative Health Reports

The `report/` folder contains two standalone analytical reports comparing Argentina and Mexico:

| File | Description |
|---|---|
| `argentina_mexico_report.R` | English gender-gap health report (13 charts) |
| `argentina_mexico_reporte_esp.R` | Spanish version with PAHO/Latin American terminology |
| `Argentina_Mexico_Health_Report.docx` | English report output |
| `Argentina_Mexico_Brecha_Genero_ES.docx` | Spanish report output |
| `report_charts/` | 13 PNG charts (English) |
| `graficos_es/` | 13 PNG charts (Spanish) |

---

## Repository Structure

```
Epidemiology/
├── app/
│   ├── app.R                  # Shiny dashboard
│   └── paho_clean.rds         # Preprocessed data (generated from raw CSV)
├── content/
│   ├── portfolio_content.R    # Framer portfolio page content generator
│   └── PAHO_Portfolio_Content.docx
├── qa/
│   ├── qa_audit.R             # 5-pillar QA audit script
│   ├── qa_report.R            # Word QA report generator
│   └── qa_output/
│       ├── paho_qa_report.xlsx
│       └── PAHO_Dashboard_QA_Report.docx
└── report/
    ├── argentina_mexico_report.R
    ├── argentina_mexico_reporte_esp.R
    ├── Argentina_Mexico_Health_Report.docx
    ├── Argentina_Mexico_Brecha_Genero_ES.docx
    ├── report_charts/         # 13 EN charts (PNG)
    └── graficos_es/           # 13 ES charts (PNG)
```

---

## How to Run Locally

1. Clone this repository
2. Download the raw PAHO dataset from [paho.org/en/data-and-statistics](https://www.paho.org/en/data-and-statistics) and place it in `sources/` as `PAHO-Core-Indicators-2025-20251001.csv`
3. Open RStudio and set the working directory to `Epidemiology/app/`
4. Install dependencies (run once in the Console):
```r
install.packages(c("shiny", "bslib", "shinyWidgets", "dplyr", "tidyr", "plotly", "DT", "scales", "glue"))
```
5. Run the app (the RDS file will be generated automatically on first run):
```r
shiny::runApp()
```

---

## AI Assistance

This project was developed with the assistance of **Claude Code** (Anthropic).

---

## Author

**Ezequiel Bassa** — Senior Data Scientist & Sociologist
[ezequielbassa.com](https://ezequielbassa.com) · [github.com/ezequielbassa](https://github.com/ezequielbassa)
