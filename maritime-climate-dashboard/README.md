# Maritime Workplace Climate Dashboard 2026

An interactive data dashboard built with R Shiny, exploring workplace climate across a synthetic Tier-1 Global Maritime Logistics Company operating throughout the Americas (North, Central, and South).

---

## Project Overview

This project simulates a real-world **Workplace Climate Survey** for a maritime logistics company with operations across 10 countries in the Americas. The dashboard enables HR leaders, data scientists, and sociologists to explore climate indicators, workforce profiles, regional patterns, and open-ended employee feedback through a fully interactive interface.

**Live Dashboard:** [ezequielbassa.shinyapps.io/maritime-climate-dashboard](https://ezequielbassa.shinyapps.io/maritime-climate-dashboard)

---

## Synthetic Dataset

The dataset was purpose-built for this project using a Python script (`generate_maritime_survey.py`).

| Property | Detail |
|---|---|
| File | `workplace_climate_maritime_2026.csv` |
| Rows | 12,500 employees |
| Columns | 30 |
| Random seed | 42 (fully reproducible) |
| Generator | `generate_maritime_survey.py` |

### Columns

**Demographics & Organization**

| Column | Description |
|---|---|
| `Employee_ID` | Unique alphanumeric ID (e.g. MAR-0001) |
| `Region` | North America · Central America · South America |
| `Country` | USA, Canada, Mexico, Panama, Costa Rica, Brazil, Chile, Argentina, Peru, Colombia |
| `Department` | Operations-Ports · Logistics-Trucking · IT · Sales/Customer Service · HR · Maritime-Crew |
| `Work_Type` | On-site · Remote · Offshore/Vessel |
| `Gender` | Male · Female · Non-binary |
| `Age` | Derived from tenure and random seed |
| `Tenure_Years` | 0.5 – 25 years (float) |
| `Salary_Band` | Junior · Mid · Senior · Executive |

**Climate Indicators (Likert Scale 1–5)**

`eNPS` · `Safety_Culture` · `Cross_Border_Collab` · `Leadership_Trust` · `Work_Life_Balance` · `Career_Growth` · `Inclusion_Diversity` · `Operational_Stress` · `Fair_Compensation` · `Tech_Adequacy` · `Recognition` · `Environmental_Pride` · `Engagement` · `Job_Security` · `Managerial_Support` · `Ethical_Standards` · `Training_Quality` · `Communication_Clarity` · `Team_Cohesion` · `Retention_Intent`

**Text**

| Column | Description |
|---|---|
| `Open_Comments` | Synthetic free-text feedback (~20% of rows), tone calibrated to the employee's average climate score |

### Built-in Biases & Correlations

The dataset was designed to reflect realistic industry patterns:

- **Maritime-Crew bias** — lower `Work_Life_Balance`, higher `Operational_Stress` vs. other departments
- **Mid-career crisis** — employees with 2–4 years tenure show lower `Engagement` and `Retention_Intent`
- **Safety–Management correlation** — `Safety_Culture` scores correlate positively with `Managerial_Support`
- **Environmental variance** — employees under 32 show higher variance in `Environmental_Pride`
- **Outliers** — 3% disgruntled employees (score penalty), 2% highly promoted employees (score boost)

---

## Dashboard

Built with **R Shiny** using a dark maritime theme (`#0d1b2a` background, `#1a6fa8` primary, `#c9a84c` accent).

### Tabs

| # | Tab | Description |
|---|---|---|
| 1 | **Overview** | KPI cards, headcount by department, work type mix, regional split, gender and salary distribution |
| 2 | **Climate Indicators** | Lollipop chart ranked by score, department × indicator heatmap, interactive radar chart |
| 3 | **Department Analysis** | Work-Life Balance vs. Operational Stress scatter, boxplot by indicator, tenure bias bar chart |
| 4 | **Regional View** | Composite score by country, per-indicator bar by region, full country score table |
| 5 | **Workforce Profile** | Tenure and age histograms, engagement confidence intervals, environmental pride violin |
| 6 | **Open Comments** | Filterable table of free-text responses with color-coded average score bars |
| 7 | **Americas Map** | Choropleth map of all 10 countries — select any KPI or overall composite score |

### Global Sidebar Filters

Region · Department · Gender · Salary Band · Work Type · Tenure range (slider)

---

## Tech Stack

### Data Generation
| Tool | Purpose |
|---|---|
| Python 3 | Dataset generation script |
| `pandas` | Data structure and export |
| `numpy` | Statistical distributions and biases |
| `faker` | Synthetic free-text comment generation |

### Dashboard
| Tool | Purpose |
|---|---|
| R | Core language |
| `shiny` | Web application framework |
| `bslib` | Bootstrap 5 theme and layout |
| `plotly` | Interactive charts and choropleth map |
| `DT` | Interactive data tables |
| `dplyr` / `tidyr` | Data wrangling |
| `ggplot2` | Base plotting layer |
| `scales` / `glue` | Formatting helpers |

### Deployment
| Service | Purpose |
|---|---|
| shinyapps.io | Dashboard hosting |
| Framer | Portfolio website with iframe embed |
| GitHub | Version control |

---

## AI Assistance

This project was developed with the assistance of **Claude Code** (Anthropic).

---

## How to Run Locally

1. Clone this repository
2. Open RStudio and set the working directory to `maritime-climate-dashboard/`
3. Install dependencies (run once in the Console):
```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT", "scales", "glue"))
```
4. Run the app:
```r
shiny::runApp()
```

---

## Author

**Ezequiel Bassa** — Senior Data Scientist & Sociologist
[ezequielbassa.com](https://ezequielbassa.com) · [github.com/ezequielbassa](https://github.com/ezequielbassa)
