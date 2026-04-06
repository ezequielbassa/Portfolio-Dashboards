# ==============================================================================
#  Análisis Comparativo de Indicadores de Salud: Argentina vs. México (2025)
#  Enfoque: Disparidades de Género y Resultados en Salud Pública
#  Autor   : Ezequiel Bassa | Científico de Datos Senior y Sociólogo
#  Datos   : OPS/EIH Indicadores Básicos 2025
#  Salida  : report/Argentina_Mexico_Brecha_Genero_ES.docx
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(officer)
library(scales)

# ── Configuración ──────────────────────────────────────────────────────────────
OUT_DIR  <- "report"
OUT_FILE <- file.path(OUT_DIR, "Argentina_Mexico_Brecha_Genero_ES.docx")
IMG_DIR  <- file.path(OUT_DIR, "graficos_es")
if (!dir.exists(IMG_DIR)) dir.create(IMG_DIR, recursive = TRUE)

# ── Paleta de colores ──────────────────────────────────────────────────────────
COL_ARG   <- "#2980b9"   # Argentina — azul
COL_MEX   <- "#c0392b"   # México    — rojo
COL_LIGHT <- "#f5f6fa"   # Fondo gráfico
CAPTION   <- "Fuente: OPS/EIH Indicadores Básicos 2025"

# ── Tema gráfico ───────────────────────────────────────────────────────────────
tema_reporte <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = COL_LIGHT, color = NA),
      panel.grid.major = element_line(color = "#dfe6e9", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 12, color = "#2d3436"),
      plot.subtitle    = element_text(size = 9,  color = "#636e72"),
      axis.text        = element_text(size = 9,  color = "#636e72"),
      axis.title       = element_text(size = 9,  color = "#2d3436"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9),
      plot.caption     = element_text(size = 7, color = "#b2bec3",
                                      hjust = 0, margin = margin(t = 6)),
      plot.margin      = margin(10, 14, 10, 10)
    )
}

# ── Función para guardar gráficos ──────────────────────────────────────────────
guardar_grafico <- function(grafico, nombre, ancho = 6.2, alto = 3.4) {
  ruta <- file.path(IMG_DIR, paste0(nombre, ".png"))
  ggsave(ruta, grafico, width = ancho, height = alto, dpi = 180, bg = "white")
  ruta
}

# ── Carga de datos ─────────────────────────────────────────────────────────────
df <- readRDS("app/paho_clean.rds")

PAISES <- c("Argentina", "Mexico")
df2 <- df |> filter(country %in% PAISES)

# Auxiliares
ultimo_valor <- function(ind, paises = PAISES) {
  df2 |>
    filter(indicator == ind, country %in% paises, !is.na(value)) |>
    group_by(country) |>
    slice_max(year, n = 1) |>
    ungroup()
}

tendencia <- function(inds, paises = PAISES, desde = 2000) {
  df2 |>
    filter(indicator %in% inds, country %in% paises,
           !is.na(value), year >= desde) |>
    arrange(country, indicator, year)
}

get_val <- function(ind, pais) {
  v <- df2 |>
    filter(indicator == ind, country == pais, !is.na(value)) |>
    slice_max(year, n = 1) |> pull(value)
  if (length(v) == 0) return(NA) else round(v[1], 1)
}

# ==============================================================================
# GENERACIÓN DE GRÁFICOS
# ==============================================================================

# ── Gráfico 1: Tendencia EVN total ────────────────────────────────────────────
d <- tendencia("Life expectancy at birth (years)")
g1 <- ggplot(d, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                     labels = c("Argentina", "México")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Esperanza de Vida al Nacer — Población Total (2000–2023)",
       x = "Año", y = "Años", caption = CAPTION) +
  tema_reporte()
img_g1 <- guardar_grafico(g1, "g1_evn_total")

# ── Gráfico 2: EVN por sexo — último año ──────────────────────────────────────
inds_sexo <- c("Life expectancy at birth (years)",
               "Life expectancy at birth (years); female",
               "Life expectancy at birth (years); male")
d2 <- df2 |>
  filter(indicator %in% inds_sexo, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(sexo = case_when(
    grepl("female", indicator) ~ "Mujeres",
    grepl("male",   indicator) ~ "Hombres",
    TRUE                       ~ "Total"
  ))

g2 <- ggplot(d2, aes(x = sexo, y = value,
                      fill = country, label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(limits = c(0, 90), expand = c(0, 0)) +
  labs(title = "Esperanza de Vida al Nacer por Sexo — Año más Reciente",
       x = "", y = "Años", caption = CAPTION) +
  tema_reporte()
img_g2 <- guardar_grafico(g2, "g2_evn_sexo")

# ── Gráfico 3: Brecha de género en EVN ────────────────────────────────────────
d3_f <- df2 |>
  filter(indicator == "Life expectancy at birth (years); female",
         !is.na(value), year >= 2000) |>
  select(country, year, mujeres = value)
d3_m <- df2 |>
  filter(indicator == "Life expectancy at birth (years); male",
         !is.na(value), year >= 2000) |>
  select(country, year, hombres = value)
d3 <- inner_join(d3_f, d3_m, by = c("country","year")) |>
  mutate(brecha = mujeres - hombres)

g3 <- ggplot(d3, aes(year, brecha, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#b2bec3") +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                     labels = c("Argentina", "México")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Brecha de Género en la Esperanza de Vida (Mujeres menos Hombres, años)",
       subtitle = "Valores positivos indican ventaja de sobrevivencia femenina",
       x = "Año", y = "Años (brecha)", caption = CAPTION) +
  tema_reporte()
img_g3 <- guardar_grafico(g3, "g3_brecha_genero")

# ── Gráfico 4: Razón de mortalidad materna ────────────────────────────────────
d4 <- tendencia("Estimated maternal mortality ratio (100 000 lb)")
g4 <- ggplot(d4, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                     labels = c("Argentina", "México")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Razón de Mortalidad Materna (por 100.000 nacidos vivos)",
       x = "Año", y = "Tasa por 100.000 n.v.", caption = CAPTION) +
  tema_reporte()
img_g4 <- guardar_grafico(g4, "g4_mortalidad_materna")

# ── Gráfico 5: Tasa de mortalidad infantil ────────────────────────────────────
d5 <- tendencia("Infant mortality rate (1 000 lb)")
g5 <- ggplot(d5, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                     labels = c("Argentina", "México")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Tasa de Mortalidad Infantil (por 1.000 nacidos vivos)",
       x = "Año", y = "Tasa por 1.000 n.v.", caption = CAPTION) +
  tema_reporte()
img_g5 <- guardar_grafico(g5, "g5_mortalidad_infantil")

# ── Gráfico 6: Causas externas de mortalidad ──────────────────────────────────
inds_ext <- c("External causes mortality rate (age-adjusted per 100 000 pop)",
              "Homicide mortality rate (age-adjusted per 100 000 pop)",
              "Road injury mortality rate (age-adjusted per 100 000 pop)",
              "Suicide mortality rate (age-adjusted per 100 000 pop)")
etiq_ext <- c("Causas externas (total)","Homicidio",
              "Lesiones viales","Suicidio")

d6 <- df2 |>
  filter(indicator %in% inds_ext, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_ext[match(indicator, inds_ext)])

g6 <- ggplot(d6, aes(x = reorder(etiqueta, value),
                      y = value, fill = country,
                      label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Mortalidad por Causas Externas (ajustada por edad, por 100.000 hab.) — Último Año",
       subtitle = "Causas predominantemente masculinas: accidentes, violencia, suicidio",
       x = "", y = "Tasa por 100.000 hab.", caption = CAPTION) +
  tema_reporte()
img_g6 <- guardar_grafico(g6, "g6_causas_externas", alto = 3.6)

# ── Gráfico 7: Mortalidad por ENT ────────────────────────────────────────────
inds_ent <- c(
  "Noncommunicable diseases mortality rate (age-adjusted per 100 000 pop)",
  "Diabetes mellitus mortality rate (age-adjusted per 100 000 pop)",
  "Circulatory diseases mortality rate (age-adjusted per 100 000 pop)",
  "Respiratory diseases mortality rate (age-adjusted per 100 000 pop)"
)
etiq_ent <- c("ENT (total)","Diabetes mellitus",
              "Enfermedades circulatorias","Enfermedades respiratorias")

d7 <- df2 |>
  filter(indicator %in% inds_ent, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_ent[match(indicator, inds_ent)])

g7 <- ggplot(d7, aes(x = reorder(etiqueta, value),
                      y = value, fill = country,
                      label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Tasas de Mortalidad por ENT (ajustada por edad, por 100.000 hab.) — Último Año",
       x = "", y = "Tasa por 100.000 hab.", caption = CAPTION) +
  tema_reporte()
img_g7 <- guardar_grafico(g7, "g7_ent_mortalidad", alto = 3.6)

# ── Gráfico 8: Factores de riesgo para ENT ────────────────────────────────────
inds_riesgo <- c(
  "Prevalence of overweight and obesity in adults (%)",
  "Prevalence of raised blood glucose/diabetes in adults (%)",
  "Prevalence of raised blood pressure in adults (%)"
)
etiq_riesgo <- c("Sobrepeso y obesidad",
                 "Diabetes (glucemia elevada)",
                 "Hipertensión arterial")

d8 <- df2 |>
  filter(indicator %in% inds_riesgo, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_riesgo[match(indicator, inds_riesgo)])

g8 <- ggplot(d8, aes(x = etiqueta, y = value,
                      fill = country,
                      label = paste0(round(value, 1), "%"))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Prevalencia de Factores de Riesgo para ENT en Adultos (%) — Último Año",
       x = "", y = "Prevalencia (%)", caption = CAPTION) +
  tema_reporte() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
img_g8 <- guardar_grafico(g8, "g8_factores_riesgo")

# ── Gráfico 9: Tendencia de sobrepeso y obesidad ──────────────────────────────
d9 <- tendencia("Prevalence of overweight and obesity in adults (%)")
g9 <- ggplot(d9, aes(year, value, color = country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                     labels = c("Argentina", "México")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Prevalencia de Sobrepeso y Obesidad en Adultos — Tendencia (2000–2023)",
       x = "Año", y = "Prevalencia (%)", caption = CAPTION) +
  tema_reporte()
img_g9 <- guardar_grafico(g9, "g9_tendencia_obesidad")

# ── Gráfico 10: VIH/SIDA y Tuberculosis ──────────────────────────────────────
inds_transm <- c("New HIV diagnoses rate (100 000 pop)",
                 "Tuberculosis incidence rate (100 000 pop)")
etiq_transm <- c("Nuevos diagnósticos de VIH (por 100k)",
                 "Incidencia de tuberculosis (por 100k)")

d10 <- df2 |>
  filter(indicator %in% inds_transm, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_transm[match(indicator, inds_transm)])

g10 <- ggplot(d10, aes(x = etiqueta, y = value,
                        fill = country,
                        label = round(value, 2))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Indicadores de VIH/SIDA y Tuberculosis — Último Año Disponible",
       x = "", y = "Tasa por 100.000 hab.", caption = CAPTION) +
  tema_reporte()
img_g10 <- guardar_grafico(g10, "g10_vih_tb")

# ── Gráfico 11: Cobertura de vacunación ───────────────────────────────────────
inds_vac <- c("Immunization coverage of under-1 year old (%), DTP3-cv",
              "Immunization coverage of 1 year old (%), MMR1")
etiq_vac <- c("Cobertura DTP3 (< 1 año)",
              "Cobertura Triple Viral/MMR1 (1 año)")

d11 <- df2 |>
  filter(indicator %in% inds_vac, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_vac[match(indicator, inds_vac)])

g11 <- ggplot(d11, aes(x = etiqueta, y = value,
                        fill = country,
                        label = paste0(round(value, 1), "%"))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  geom_hline(yintercept = 95, linetype = "dashed", color = "#e17055",
             linewidth = 0.7) +
  annotate("text", x = 0.6, y = 96.5,
           label = "Meta OMS 95%", size = 2.8, color = "#e17055") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 105), expand = c(0, 0)) +
  labs(title = "Cobertura de Vacunación (%) — Último Año Disponible",
       x = "", y = "Cobertura (%)", caption = CAPTION) +
  tema_reporte()
img_g11 <- guardar_grafico(g11, "g11_vacunacion")

# ── Gráfico 12: Capacidad del sistema de salud ────────────────────────────────
inds_sis <- c("Density of medical doctors (10 000 pop)",
              "Hospital beds ratio (1 000 pop)",
              "Out-of-pocket expenditure as % of current health expenditure",
              "Public health expenditure as % of GDP")
etiq_sis <- c("Médicos (por 10.000 hab.)",
              "Camas hospitalarias (por 1.000 hab.)",
              "Gasto de bolsillo (% gasto corriente en salud)",
              "Gasto público en salud (% PIB)")

d12 <- df2 |>
  filter(indicator %in% inds_sis, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_sis[match(indicator, inds_sis)])

g12 <- ggplot(d12, aes(x = reorder(etiqueta, value),
                        y = value, fill = country,
                        label = round(value, 2))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), hjust = -0.15, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Indicadores de Capacidad del Sistema de Salud — Último Año",
       x = "", y = "Valor (ver etiquetas de ejes para unidades)",
       caption = CAPTION) +
  tema_reporte()
img_g12 <- guardar_grafico(g12, "g12_sistema_salud", alto = 3.6)

# ── Gráfico 13: Contexto sociodemográfico ────────────────────────────────────
inds_demo <- c("Urban Population (%)",
               "Dependency ratio (100 pop)",
               "Population aged 65 and over (%)",
               "Population aged < 15 years (%)")
etiq_demo <- c("Población urbana (%)",
               "Razón de dependencia (por 100 hab.)",
               "Población de 65 años y más (%)",
               "Población menor de 15 años (%)")

d13 <- df2 |>
  filter(indicator %in% inds_demo, !is.na(value)) |>
  group_by(country, indicator) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(etiqueta = etiq_demo[match(indicator, inds_demo)])

g13 <- ggplot(d13, aes(x = etiqueta, y = value,
                        fill = country,
                        label = round(value, 1))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(position = position_dodge(0.7), vjust = -0.4, size = 3.2,
            fontface = "bold") +
  scale_fill_manual(values = c("Argentina" = COL_ARG, "Mexico" = COL_MEX),
                    labels = c("Argentina", "México")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Perfil Sociodemográfico — Último Año Disponible",
       x = "", y = "Valor", caption = CAPTION) +
  tema_reporte() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 8))
img_g13 <- guardar_grafico(g13, "g13_demografia", alto = 3.8)

cat("Los", 13, "gráficos han sido generados.\n")

# ==============================================================================
# CÁLCULO DE ESTADÍSTICAS CLAVE
# ==============================================================================
le_arg   <- get_val("Life expectancy at birth (years)", "Argentina")
le_mex   <- get_val("Life expectancy at birth (years)", "Mexico")
le_f_arg <- get_val("Life expectancy at birth (years); female", "Argentina")
le_f_mex <- get_val("Life expectancy at birth (years); female", "Mexico")
le_m_arg <- get_val("Life expectancy at birth (years); male", "Argentina")
le_m_mex <- get_val("Life expectancy at birth (years); male", "Mexico")
brecha_arg <- round(le_f_arg - le_m_arg, 1)
brecha_mex <- round(le_f_mex - le_m_mex, 1)

mat_arg  <- get_val("Estimated maternal mortality ratio (100 000 lb)", "Argentina")
mat_mex  <- get_val("Estimated maternal mortality ratio (100 000 lb)", "Mexico")
inf_arg  <- get_val("Infant mortality rate (1 000 lb)", "Argentina")
inf_mex  <- get_val("Infant mortality rate (1 000 lb)", "Mexico")
ext_arg  <- get_val("External causes mortality rate (age-adjusted per 100 000 pop)", "Argentina")
ext_mex  <- get_val("External causes mortality rate (age-adjusted per 100 000 pop)", "Mexico")
hom_arg  <- get_val("Homicide mortality rate (age-adjusted per 100 000 pop)", "Argentina")
hom_mex  <- get_val("Homicide mortality rate (age-adjusted per 100 000 pop)", "Mexico")

obes_arg <- get_val("Prevalence of overweight and obesity in adults (%)", "Argentina")
obes_mex <- get_val("Prevalence of overweight and obesity in adults (%)", "Mexico")
diab_arg <- get_val("Prevalence of raised blood glucose/diabetes in adults (%)", "Argentina")
diab_mex <- get_val("Prevalence of raised blood glucose/diabetes in adults (%)", "Mexico")
hbp_arg  <- get_val("Prevalence of raised blood pressure in adults (%)", "Argentina")
hbp_mex  <- get_val("Prevalence of raised blood pressure in adults (%)", "Mexico")

doc_arg  <- get_val("Density of medical doctors (10 000 pop)", "Argentina")
doc_mex  <- get_val("Density of medical doctors (10 000 pop)", "Mexico")
camas_arg <- get_val("Hospital beds ratio (1 000 pop)", "Argentina")
camas_mex <- get_val("Hospital beds ratio (1 000 pop)", "Mexico")
oop_arg  <- get_val("Out-of-pocket expenditure as % of current health expenditure", "Argentina")
oop_mex  <- get_val("Out-of-pocket expenditure as % of current health expenditure", "Mexico")
pub_arg  <- get_val("Public health expenditure as % of GDP", "Argentina")
pub_mex  <- get_val("Public health expenditure as % of GDP", "Mexico")

tb_arg   <- get_val("Tuberculosis incidence rate (100 000 pop)", "Argentina")
tb_mex   <- get_val("Tuberculosis incidence rate (100 000 pop)", "Mexico")
vih_arg  <- get_val("New HIV diagnoses rate (100 000 pop)", "Argentina")
vih_mex  <- get_val("New HIV diagnoses rate (100 000 pop)", "Mexico")

cat("Estadísticas clave calculadas.\n")

# ==============================================================================
# CONSTRUCCIÓN DEL DOCUMENTO WORD
# ==============================================================================
doc <- read_docx()

add_h1    <- function(doc, texto) body_add_par(doc, texto, style = "heading 1")
add_h2    <- function(doc, texto) body_add_par(doc, texto, style = "heading 2")
add_h3    <- function(doc, texto) body_add_par(doc, texto, style = "heading 3")
add_p     <- function(doc, texto) body_add_par(doc, texto, style = "Normal")
add_blank <- function(doc)        body_add_par(doc, "",    style = "Normal")
add_nota  <- function(doc, etiqueta, texto) {
  body_add_par(doc, paste0("\u25a0 ", etiqueta, ": ", texto), style = "Normal")
}

# ── PORTADA ────────────────────────────────────────────────────────────────────
doc <- doc |>
  add_blank() |>
  add_blank() |>
  add_h1("Análisis Comparativo de Indicadores de Salud") |>
  add_h1("Argentina vs. México (2025)") |>
  add_blank() |>
  add_h2("Enfoque: Disparidades de Género y Resultados en Salud Pública") |>
  add_blank() |>
  add_blank() |>
  add_p("Autor:          Ezequiel Bassa | Científico de Datos Senior y Sociólogo") |>
  add_p("Fecha:          1 de abril de 2026") |>
  add_p("Fuente de datos: OPS/EIH Indicadores Básicos 2025") |>
  add_p("Panel de datos: https://ezequielbassa.shinyapps.io/3Epidemiology/") |>
  add_p("Herramientas:   R · Shiny · ggplot2 · officer") |>
  body_add_break()

# ── I. RESUMEN EJECUTIVO ───────────────────────────────────────────────────────
doc <- doc |>
  add_h1("I. Resumen Ejecutivo") |>
  add_p(paste0(
    "El presente informe ofrece un análisis epidemiológico comparativo entre Argentina y México ",
    "a partir de los datos del repositorio de Indicadores Básicos 2025 de la Organización ",
    "Panamericana de la Salud (OPS). Ambas naciones atraviesan transiciones demográficas y ",
    "epidemiológicas avanzadas, pero sus estructuras sanitarias producen resultados diferenciados ",
    "en función del género, la edad y la carga de enfermedad."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Argentina registra una esperanza de vida al nacer de ", le_arg,
    " años, levemente superior a la de México con ", le_mex,
    " años. En ambos países las mujeres superan a los hombres en años vividos: la brecha de ",
    "género asciende a ", brecha_arg, " años en Argentina y a ", brecha_mex,
    " años en México. Esta diferencia está determinada por una mayor mortalidad prematura ",
    "masculina asociada a causas externas y enfermedades crónicas."
  )) |>
  add_blank() |>
  add_p("Hallazgos clave:") |>
  add_p(paste0("  \u2022  Brecha de género en EVN: Argentina ", brecha_arg,
               " años | México ", brecha_mex, " años")) |>
  add_p(paste0("  \u2022  Razón de mortalidad materna: Argentina ", mat_arg,
               " vs. México ", mat_mex, " por 100.000 nacidos vivos")) |>
  add_p(paste0("  \u2022  Mortalidad por causas externas: México (",
               ext_mex, ") significativamente más alta que Argentina (",
               ext_arg, ") por 100.000 hab.")) |>
  add_p(paste0("  \u2022  Prevalencia de obesidad: México (", obes_mex,
               "%) vs. Argentina (", obes_arg, "%) — ambas por encima de la mediana regional")) |>
  add_p(paste0("  \u2022  Densidad médica: Argentina (", doc_arg,
               " por 10.000 hab.) vs. México (", doc_mex, " por 10.000 hab.)")) |>
  add_blank() |>
  add_p(paste0(
    "Los datos evidencian un patrón consistente: México enfrenta una mayor carga de mortalidad ",
    "masculina por violencia y el binomio obesidad-diabetes, mientras que Argentina afronta una ",
    "meseta en la esperanza de vida y una razón de mortalidad materna persistente que requiere ",
    "intervenciones focalizadas."
  )) |>
  body_add_break()

# ── II. INTRODUCCIÓN Y METODOLOGÍA ────────────────────────────────────────────
doc <- doc |>
  add_h1("II. Introducción y Metodología") |>
  add_h2("Fuente de datos") |>
  add_p(paste0(
    "Este análisis utiliza el repositorio de Indicadores Básicos OPS/EIH 2025, una fuente de ",
    "acceso abierto que contiene 299 indicadores de salud para 49 países y territorios de las ",
    "Américas. Los datos fueron procesados a través del Panel de Indicadores de Salud de las ",
    "Américas (R Shiny), que gestiona 337.970 registros depurados de múltiples décadas."
  )) |>
  add_blank() |>
  add_h2("Justificación de la selección de países") |>
  add_p(paste0(
    "Argentina y México fueron seleccionados en razón de su escala demográfica comparable y ",
    "su perfil de transición epidemiológica compartido, al tiempo que exhiben arquitecturas ",
    "de sistemas de salud sustancialmente distintas:"
  )) |>
  add_p(paste0(
    "  \u2022  Argentina opera un sistema tripartito: sector público, Obras Sociales ",
    "(seguro social de base laboral) y medicina prepaga privada. Esta estructura genera una ",
    "mayor densidad de recursos humanos en salud, aunque con cobertura fragmentada."
  )) |>
  add_p(paste0(
    "  \u2022  México ha avanzado hacia la universalización de la cobertura con el ",
    "esquema IMSS-Bienestar (sucesor del Seguro Popular), aunque persisten desigualdades ",
    "estructurales entre estados y entre el sector formal e informal de la economía."
  )) |>
  add_blank() |>
  add_h2("Limitaciones de los datos") |>
  add_p(paste0(
    "Los Indicadores Básicos de la OPS proveen desagregación por sexo (masculino/femenino) ",
    "para indicadores seleccionados, principalmente la esperanza de vida al nacer. La mayoría ",
    "de las tasas de mortalidad, la prevalencia de ENT y los indicadores del sistema de salud ",
    "se reportan de forma agregada. Las categorías de género no binarias no están incorporadas ",
    "en la infraestructura de datos actual de la OPS. En los casos en que no se dispone de ",
    "datos desagregados por sexo, se utilizan indicadores agregados con interpretación contextual ",
    "basada en la evidencia epidemiológica disponible."
  )) |>
  body_add_break()

# ── III. CONTEXTO SOCIODEMOGRÁFICO ────────────────────────────────────────────
doc <- doc |>
  add_h1("III. Contexto Sociodemográfico") |>
  add_p(paste0(
    "La comprensión de la estructura demográfica es indispensable para contextualizar la demanda ",
    "sobre los sistemas de salud. Tanto Argentina como México son países de ingreso medio-alto, ",
    "con poblaciones predominantemente urbanas y transiciones demográficas avanzadas, aunque en ",
    "estadios diferenciados."
  )) |>
  add_blank()
doc <- body_add_img(doc, src = img_g13, width = 6.0, height = 3.8)
doc <- doc |>
  add_blank() |>
  add_h2("Urbanización y estructura etaria") |>
  add_p(paste0(
    "Argentina es uno de los países más urbanizados de América Latina, con una proporción más ",
    "elevada de población mayor de 65 años, lo que refleja una transición demográfica más avanzada. ",
    "México presenta una estructura etaria más joven en términos generales, lo que representa ",
    "simultáneamente un bono demográfico y un desafío de política pública: una amplia población ",
    "en edad laboral crecientemente expuesta a factores de riesgo de enfermedades no transmisibles."
  )) |>
  add_blank() |>
  add_p(paste0(
    "La razón de dependencia demográfica —definida como la proporción de población inactiva ",
    "(menores y adultos mayores) respecto a la población en edad de trabajar— condiciona tanto ",
    "la demanda de servicios de salud como la capacidad fiscal para financiarlos. El mayor ",
    "envejecimiento de Argentina incrementa la carga de enfermedades crónicas; la población ",
    "más joven de México concentra el riesgo en las cohortes reproductivas y laborales."
  )) |>
  add_blank() |>
  add_nota("Recuadro de contexto", paste0(
    "La edad mediana de Argentina es superior a la de México, señalando una transición de ",
    "envejecimiento más avanzada. Esto configura una carga de enfermedad crónica orientada ",
    "hacia enfermedades cardiovasculares y cáncer en Argentina, frente a una carga mixta ",
    "en México donde conviven enfermedades transmisibles rezagadas con ENT en ascenso."
  )) |>
  body_add_break()

# ── IV. ESPERANZA DE VIDA Y ENVEJECIMIENTO SALUDABLE ──────────────────────────
doc <- doc |>
  add_h1("IV. Esperanza de Vida y Envejecimiento Saludable") |>
  add_p(paste0(
    "La esperanza de vida al nacer (EVN) es el indicador de síntesis más utilizado para ",
    "caracterizar el nivel de salud de una población. La tendencia desagregada por sexo revela ",
    "tanto la magnitud de la ventaja de sobrevivencia femenina como las trayectorias diferenciadas ",
    "de cada país."
  )) |>
  add_blank() |>
  add_h2("Figura 1 — Esperanza de Vida al Nacer: Tendencia de Población Total")
doc <- body_add_img(doc, src = img_g1, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_h2("Figura 2 — Esperanza de Vida al Nacer por Sexo: Comparación Más Reciente")
doc <- body_add_img(doc, src = img_g2, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La EVN de Argentina (", le_arg, " años en total; mujeres ", le_f_arg,
    " | hombres ", le_m_arg,
    ") refleja un patrón de estabilización: las ganancias se han desacelerado respecto a décadas ",
    "anteriores, con la carga cardiovascular y las lesiones como principales determinantes de la ",
    "sobremortalidad masculina."
  )) |>
  add_blank() |>
  add_p(paste0(
    "México presenta una EVN total de ", le_mex, " años (mujeres ", le_f_mex,
    " | hombres ", le_m_mex,
    "). El país registró ganancias sostenidas durante los años 2000 y 2010, con una perturbación ",
    "notable durante la pandemia de COVID-19. La brecha de género en México (", brecha_mex,
    " años) es más pronunciada que en Argentina (", brecha_arg,
    " años), reflejando la mayor carga de mortalidad masculina por causas externas y violencia."
  )) |>
  add_blank() |>
  add_h2("Figura 3 — Brecha de Género en la Esperanza de Vida a lo Largo del Tiempo")
doc <- body_add_img(doc, src = img_g3, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La ventaja de sobrevivencia femenina es una característica estructural consistente en ambos ",
    "países. Biológicamente, las mujeres se benefician de la protección hormonal frente a las ",
    "enfermedades cardiovasculares en el período premenopáusico. En términos sociales, los hombres ",
    "enfrentan mayores riesgos laborales, tasas más elevadas de consumo de tabaco y alcohol, y ",
    "mayor exposición a la violencia interpersonal — factores que amplifican las diferencias ",
    "biológicas hasta convertirse en una brecha de mortalidad medible."
  )) |>
  add_blank() |>
  add_nota("Recuadro de contexto", paste0(
    "Si la esperanza de vida masculina de México convergiera hacia la femenina, representaría ",
    "aproximadamente ", brecha_mex, " años adicionales de vida masculina — un dividendo de salud ",
    "pública alcanzable mediante políticas de prevención de la violencia, regulación del alcohol ",
    "y gestión de enfermedades no transmisibles."
  )) |>
  body_add_break()

# ── V. ANÁLISIS DE MORTALIDAD POR SEXO ────────────────────────────────────────
doc <- doc |>
  add_h1("V. Análisis de Mortalidad por Sexo") |>
  add_h2("5.1 Mortalidad Materna e Infantil: Línea de Base en Salud Reproductiva") |>
  add_p(paste0(
    "La mortalidad materna e infantil constituyen indicadores de referencia del desempeño del ",
    "sistema de salud en salud reproductiva. Estos indicadores son inherentemente sexo-específicos ",
    "y reflejan tanto la calidad de la atención obstétrica como los determinantes sociales ",
    "más amplios del bienestar materno."
  )) |>
  add_blank() |>
  add_h3("Figura 4 — Razón de Mortalidad Materna: Tendencia")
doc <- body_add_img(doc, src = img_g4, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_h3("Figura 5 — Tasa de Mortalidad Infantil: Tendencia")
doc <- body_add_img(doc, src = img_g5, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La razón de mortalidad materna de Argentina (", mat_arg,
    " por 100.000 nacidos vivos) es una preocupación persistente de política pública a pesar ",
    "de la inversión sostenida en el sistema de salud. El estancamiento en la reducción — en ",
    "comparación con descensos más acelerados en países pares — sugiere desigualdades geográficas ",
    "y socioeconómicas en el acceso a atención obstétrica de calidad, particularmente en las ",
    "provincias del Noroeste argentino (NOA) y Noreste argentino (NEA)."
  )) |>
  add_blank() |>
  add_p(paste0(
    "La razón de mortalidad materna de México (", mat_mex,
    " por 100.000 nacidos vivos) refleja una línea de base más elevada. Las mujeres indígenas ",
    "y rurales enfrentan riesgos desproporcionados debido a barreras geográficas en el acceso ",
    "a atención obstétrica calificada y a cuidados obstétricos de emergencia. Ambos países se ",
    "encuentran por encima de la meta del ODS 3.1 de menos de 70 defunciones maternas por ",
    "100.000 nacidos vivos para 2030."
  )) |>
  add_blank() |>
  add_p(paste0(
    "La mortalidad infantil en ambos países ha descendido de manera sustancial desde el año 2000. ",
    "Argentina (", inf_arg, " por 1.000 n.v.) y México (", inf_mex,
    " por 1.000 n.v.) reflejan mejoras en la atención neonatal, la expansión de la ",
    "vacunación y los programas de rehidratación oral. Sin embargo, importantes disparidades ",
    "subnacionales permanecen ocultas detrás de estos promedios nacionales."
  )) |>
  add_blank() |>
  add_h2("5.2 Mortalidad en Adultos: Causas Externas y la Desventaja Masculina") |>
  add_p(paste0(
    "Las causas externas — que comprenden accidentes, homicidios, suicidios y lesiones viales — ",
    "representan un determinante central de la desventaja de mortalidad masculina en ambos países. ",
    "El patrón es especialmente pronunciado en México, donde la violencia organizada e interpersonal ",
    "genera una carga estructural de mortalidad masculina sin equivalente en Argentina."
  )) |>
  add_blank() |>
  add_h3("Figura 6 — Mortalidad por Causas Externas (ajustada por edad, por 100.000 hab.)")
doc <- body_add_img(doc, src = img_g6, width = 6.0, height = 3.6)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La tasa de mortalidad por causas externas de México (", ext_mex,
    " por 100.000 hab.) supera ampliamente a la de Argentina (", ext_arg,
    " por 100.000 hab.). El diferencial en la tasa de homicidios es particularmente marcado: ",
    "México (", hom_mex, ") frente a Argentina (", hom_arg,
    ") por 100.000 habitantes. En México, el homicidio afecta predominantemente a hombres — ",
    "más del 85% de las víctimas son masculinas — y constituye el principal factor contribuyente ",
    "a la brecha de género en la esperanza de vida."
  )) |>
  add_blank() |>
  add_p(paste0(
    "El perfil de mortalidad de Argentina muestra un mayor peso relativo de las enfermedades ",
    "crónicas como causa de muerte prematura masculina, coherente con una transición epidemiológica ",
    "más avanzada. Las lesiones viales y el suicidio contribuyen de manera significativa a la ",
    "sobremortalidad masculina en Argentina, donde las políticas de salud mental han recibido ",
    "históricamente una inversión menor en relación con la infraestructura de salud física."
  )) |>
  add_blank() |>
  add_nota("Recuadro de contexto", paste0(
    "En México, la violencia es un problema de salud con dimensión de género. La feminización ",
    "de la inseguridad — en la que las mujeres soportan los costos indirectos de la violencia ",
    "a través del duelo, la sobrecarga de cuidados y la movilidad restringida por el miedo — ",
    "no queda capturada en las estadísticas de mortalidad, pero constituye una dimensión crítica ",
    "de la brecha de salud de género."
  )) |>
  body_add_break()

# ── VI. ENFERMEDADES NO TRANSMISIBLES (ENT) Y FACTORES DE RIESGO ──────────────
doc <- doc |>
  add_h1("VI. Enfermedades No Transmisibles (ENT) y Factores de Riesgo") |>
  add_p(paste0(
    "La transición epidemiológica en América Latina ha situado a las enfermedades no transmisibles ",
    "en el centro de la agenda de salud pública. Tanto Argentina como México enfrentan una carga ",
    "elevada y creciente de ENT, aunque la distribución de los factores de riesgo y de los grupos ",
    "poblacionales afectados difiere de maneras con implicancias para las políticas sanitarias."
  )) |>
  add_blank() |>
  add_h2("Figura 7 — Tasas de Mortalidad por ENT (ajustadas por edad, último año)")
doc <- body_add_img(doc, src = img_g7, width = 6.0, height = 3.6)
doc <- doc |>
  add_blank() |>
  add_h2("Figura 8 — Prevalencia de Factores de Riesgo para ENT en Adultos")
doc <- body_add_img(doc, src = img_g8, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La prevalencia de sobrepeso y obesidad en México (", obes_mex,
    "%) supera a la de Argentina (", obes_arg,
    "%), situando a México entre los países con mayor carga de este indicador a nivel global. ",
    "La transición alimentaria mexicana — caracterizada por la rápida adopción de alimentos ",
    "ultraprocesados, bebidas azucaradas y reducción de la adherencia a la dieta tradicional — ",
    "está bien documentada y es más avanzada que en Argentina."
  )) |>
  add_blank() |>
  add_p(paste0(
    "La prevalencia de diabetes mellitus (glucemia en ayunas elevada) sitúa a México en ",
    diab_mex, "% frente al ", diab_arg,
    "% de Argentina. Esta diferencia refleja tanto la mayor carga de obesidad como la ",
    "susceptibilidad genética en los grupos de población indígena de México. La dimensión de ",
    "género es significativa: en México, las mujeres en edad reproductiva y laboral presentan ",
    "una prevalencia de diabetes desproporcionadamente elevada, con consecuencias adversas ",
    "para los resultados obstétricos y la productividad económica."
  )) |>
  add_blank() |>
  add_p(paste0(
    "La hipertensión arterial (presión arterial elevada) es comparable en ambos países: México ",
    "con ", hbp_mex, "% y Argentina con ", hbp_arg,
    "%. En Argentina, la hipertensión es el principal contribuyente a la mortalidad cardiovascular ",
    "en hombres, reflejo de una mayor ingesta de sodio y menor actividad física en zonas urbanas."
  )) |>
  add_blank() |>
  add_h2("Figura 9 — Tendencia de Sobrepeso y Obesidad en Adultos")
doc <- body_add_img(doc, src = img_g9, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La tendencia de obesidad muestra trayectorias ascendentes sostenidas en ambos países desde ",
    "el año 2000. La tasa de México ha crecido más rápidamente y desde una base más elevada, ",
    "lo que sugiere que, sin intervenciones estructurales de política alimentaria, esta trayectoria ",
    "continuará impulsando la incidencia de diabetes, enfermedades cardiovasculares y cáncer ",
    "en las próximas décadas."
  )) |>
  add_blank() |>
  add_nota("Recuadro de contexto", paste0(
    "México introdujo un impuesto a las bebidas azucaradas en 2014, siendo uno de los primeros ",
    "países de América Latina en implementar esta medida fiscal. La evidencia disponible indica ",
    "una reducción modesta en el consumo, especialmente en hogares de menores ingresos. ",
    "Argentina aún no ha implementado una medida fiscal comparable a nivel nacional."
  )) |>
  body_add_break()

# ── VII. ENFERMEDADES TRANSMISIBLES Y VACUNACIÓN ──────────────────────────────
doc <- doc |>
  add_h1("VII. Enfermedades Transmisibles y Vacunación") |>
  add_p(paste0(
    "Si bien Argentina y México han avanzado considerablemente en la transición epidemiológica, ",
    "las enfermedades transmisibles mantienen relevancia en salud pública, en particular para ",
    "las poblaciones vulnerables y las regiones subnacionales con acceso limitado al sistema de salud."
  )) |>
  add_blank() |>
  add_h2("Figura 10 — VIH/SIDA y Tuberculosis (último año disponible)")
doc <- body_add_img(doc, src = img_g10, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "Incidencia de tuberculosis (TB): Argentina (", tb_arg,
    " por 100.000 hab.) frente a México (", tb_mex,
    " por 100.000 hab.). La tuberculosis sigue siendo una enfermedad asociada a la pobreza y el ",
    "hacinamiento. Ambos países muestran tendencias descendentes atribuibles a la expansión de los ",
    "programas DOTS/TAES (Tratamiento Acortado Estrictamente Supervisado) y la mejora en las ",
    "condiciones habitacionales. No obstante, la variación subnacional sigue siendo significativa: ",
    "las provincias del NOA en Argentina y los estados del sur de México concentran una carga ",
    "desproporcionada."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Tasa de nuevos diagnósticos de VIH: Argentina (", vih_arg,
    ") frente a México (", vih_mex, ") por 100.000 habitantes. ",
    "La dimensión de género del VIH/SIDA en América Latina refleja un patrón de creciente ",
    "transmisión heterosexual. Si bien los hombres que tienen sexo con hombres (HSH) continúan ",
    "siendo el grupo de mayor riesgo en ambos países, las mujeres — en especial las jóvenes — ",
    "enfrentan una vulnerabilidad creciente a través de vías de transmisión heterosexual vinculadas ",
    "a la violencia de pareja, la ausencia de poder de negociación y la dependencia socioeconómica."
  )) |>
  add_blank() |>
  add_h2("Figura 11 — Cobertura de Vacunación frente a la Meta del 95% de la OMS")
doc <- body_add_img(doc, src = img_g11, width = 6.0, height = 3.4)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La cobertura de vacunación actúa como indicador proxy del alcance y la equidad del sistema ",
    "de salud. El umbral del 95% de cobertura establecido por la OMS para la inmunidad de rebaño ",
    "es un parámetro crítico de referencia. Ambos países cuentan con sólidos programas nacionales ",
    "de inmunización, aunque la recuperación pospandemia en las tasas de cobertura aún está ",
    "incompleta en algunas jurisdicciones subnacionales."
  )) |>
  add_blank() |>
  add_nota("Recuadro de contexto", paste0(
    "La hesitancia vacunal ha emergido como un desafío en zonas urbanas de Argentina tras la ",
    "pandemia de COVID-19, afectando particularmente la cobertura de la vacuna Triple Viral ",
    "(MMR) en barrios de ingreso medio — un perfil de hesitancia atípico respecto a las ",
    "brechas de cobertura por acceso geográfico más habituales en zonas rurales de México."
  )) |>
  body_add_break()

# ── VIII. CAPACIDAD Y ACCESO AL SISTEMA DE SALUD ──────────────────────────────
doc <- doc |>
  add_h1("VIII. Capacidad y Acceso al Sistema de Salud") |>
  add_p(paste0(
    "Los resultados en salud están condicionados no solo por la exposición epidemiológica, sino ",
    "por la capacidad del sistema para detectar, tratar y prevenir enfermedades. Argentina y México ",
    "representan modelos contrastantes de organización del sistema de salud en América Latina."
  )) |>
  add_blank() |>
  add_h2("Figura 12 — Indicadores de Capacidad del Sistema de Salud (último año)")
doc <- body_add_img(doc, src = img_g12, width = 6.0, height = 3.6)
doc <- doc |>
  add_blank() |>
  add_p(paste0(
    "La densidad de médicos en Argentina (", doc_arg,
    " por 10.000 hab.) supera significativamente a la de México (", doc_mex,
    " por 10.000 hab.). Este diferencial refleja la larga tradición argentina de inversión en ",
    "formación médica y la concentración de especialistas en el Área Metropolitana de Buenos Aires ",
    "y las principales capitales provinciales. Sin embargo, la distribución geográfica inequitativa ",
    "implica que las comunidades rurales e indígenas en ambos países acceden a menos médicos de lo ",
    "que los promedios nacionales sugieren."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Camas hospitalarias: Argentina (", camas_arg,
    " por 1.000 hab.) frente a México (", camas_mex,
    " por 1.000 hab.). La mayor densidad de camas en Argentina refleja la inversión del sistema de ",
    "Obras Sociales en infraestructura de internación. La menor dotación de México, combinada con ",
    "un gasto de bolsillo elevado (", oop_mex,
    "% del gasto corriente en salud), incrementa el riesgo financiero para los hogares que ",
    "enfrentan enfermedades agudas — una carga que recae desproporcionadamente sobre las mujeres, ",
    "más proclives a carecer de cobertura formal por su mayor inserción en el empleo informal."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Gasto público en salud como porcentaje del PIB: Argentina (", pub_arg,
    "%) frente a México (", pub_mex,
    "%). La menor inversión pública de México — a pesar de la expansión del IMSS-Bienestar — ",
    "implica que una mayor proporción de los costos de salud recae sobre los hogares, generando ",
    "una dinámica de financiamiento regresiva que amplía las desigualdades sanitarias."
  )) |>
  add_blank() |>
  add_nota("Recuadro de contexto", paste0(
    "El gasto de bolsillo en salud en México (", oop_mex,
    "%) frente a Argentina (", oop_arg,
    "%) representa una diferencia estructural en la protección financiera. ",
    "El elevado gasto de bolsillo es un factor determinante del empobrecimiento por gastos ",
    "médicos, particularmente en el manejo de enfermedades crónicas donde los costos de ",
    "medicamentos se acumulan de forma sostenida."
  )) |>
  body_add_break()

# ── IX. DISCUSIÓN: LA BRECHA DE GÉNERO EN SALUD ───────────────────────────────
doc <- doc |>
  add_h1("IX. Discusión: La Brecha de Género en Salud") |>
  add_h2("9.1 La Ventaja de Sobrevivencia Femenina y sus Límites") |>
  add_p(paste0(
    "Las mujeres en Argentina y México viven más que los hombres — un patrón biológico universal ",
    "reforzado por factores conductuales y sociales. Sin embargo, esta ventaja de sobrevivencia no ",
    "se traduce en calidad de vida equivalente. Las mujeres enfrentan cargas específicas de salud ",
    "que las estadísticas de mortalidad agregadas ocultan: mortalidad materna, secuelas de la ",
    "violencia de género, trastornos de salud mental vinculados de manera desproporcionada al ",
    "trabajo de cuidado y a la dependencia económica, y el impacto físico del trabajo doméstico ",
    "y de cuidado no remunerado."
  )) |>
  add_blank() |>
  add_h2("9.2 Determinantes Sociales que Explican las Diferencias por Sexo") |>
  add_p(paste0(
    "Empleo y autonomía económica: En ambos países, la participación laboral femenina es inferior ",
    "y está concentrada en sectores informales sin seguro social. En México, aproximadamente el 55% ",
    "de las mujeres ocupadas trabaja en la informalidad, frente al 45% de los hombres. El empleo ",
    "informal excluye a las trabajadoras del IMSS — el principal asegurador público de salud — ",
    "generando una brecha sistemática de género en el acceso a la atención médica."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Carga del trabajo de cuidado: Las mujeres en Argentina y México realizan entre 3 y 4 veces ",
    "más trabajo de cuidado no remunerado que los hombres (encuestas de uso del tiempo de la CEPAL). ",
    "Esta asignación estructural del trabajo de cuidado limita el tiempo disponible de las mujeres ",
    "para conductas preventivas de salud, retrasa la búsqueda de atención médica y genera ",
    "exposiciones crónicas al estrés asociadas a hipertensión, trastornos de salud mental y ",
    "alteraciones del sistema inmunitario."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Violencia de género (VBG): La tasa de femicidios en Argentina y los alarmantemente elevados ",
    "registros de femicidio en México representan la dimensión más visible del riesgo sanitario de ",
    "género. Más allá de la mortalidad, la violencia de pareja íntima está asociada a un espectro ",
    "de resultados adversos en salud que incluyen depresión, trastorno de estrés postraumático, ",
    "infecciones de transmisión sexual y resultados obstétricos adversos. Estas vías causales están ",
    "sistemáticamente subrepresentadas en los conjuntos estándar de indicadores epidemiológicos."
  )) |>
  add_blank() |>
  add_h2("9.3 Carga de Mortalidad Masculina: Violencia y Enfermedad Crónica") |>
  add_p(paste0(
    "La sobremortalidad masculina adopta formas diferenciadas en cada país. En México, el homicidio ",
    "— concentrado en varones jóvenes de 15 a 44 años — es el principal determinante de la brecha ",
    "de género en la esperanza de vida. En Argentina, el determinante está más distribuido entre ",
    "enfermedades cardiovasculares, suicidio y lesiones viales — patrones más abordables mediante ",
    "políticas de manejo de enfermedades crónicas y salud mental."
  )) |>
  add_blank() |>
  add_p(paste0(
    "Los comportamientos de búsqueda de atención en salud entre los hombres constituyen un factor ",
    "transversal en ambos países: los varones concurren con menor frecuencia a la atención primaria, ",
    "adhieren menos a los tratamientos para enfermedades crónicas y se presentan con mayor frecuencia ",
    "en servicios de urgencias en estadios avanzados de enfermedad. Las normas de masculinidad que ",
    "asocian la búsqueda de ayuda con debilidad representan una barrera estructural para reducir la ",
    "desventaja sanitaria masculina."
  )) |>
  body_add_break()

# ── X. CONCLUSIONES Y RECOMENDACIONES ────────────────────────────────────────
doc <- doc |>
  add_h1("X. Conclusiones y Recomendaciones") |>
  add_h2("10.1 Síntesis de los Hallazgos Principales") |>
  add_blank() |>
  body_add_table(
    data.frame(
      Dimensión          = c("Esperanza de vida","Brecha de género","Mortalidad materna",
                             "Mortalidad por causas externas","Obesidad",
                             "Acceso al sistema de salud"),
      Argentina          = c(paste0(le_arg, " años"),
                             paste0(brecha_arg, " años (M>H)"),
                             paste0(mat_arg, " por 100k n.v."),
                             paste0(ext_arg, " por 100k"),
                             paste0(obes_arg, "%"),
                             paste0(doc_arg, " médicos por 10k")),
      México             = c(paste0(le_mex, " años"),
                             paste0(brecha_mex, " años (M>H)"),
                             paste0(mat_mex, " por 100k n.v."),
                             paste0(ext_mex, " por 100k"),
                             paste0(obes_mex, "%"),
                             paste0(doc_mex, " médicos por 10k")),
      Hallazgo_clave     = c(
        "ARG levemente superior; ambas en estabilización",
        "MEX brecha mayor — determinada por violencia",
        "Ambas superan la meta ODS; ARG estancada",
        "MEX significativamente más alta — carga por homicidio",
        "MEX más elevada; ambas sobre la mediana regional",
        "ARG mayor densidad pero con distribución inequitativa"
      ),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  add_h2("10.2 Recomendaciones para Argentina") |>
  add_p(paste0(
    "1. Salud materna: Invertir en infraestructura de emergencia obstétrica y atención ",
    "calificada del parto en las regiones NOA y NEA, donde la razón de mortalidad materna ",
    "supera en 2 a 3 veces el promedio nacional. Integrar modelos de salud intercultural para ",
    "reducir las barreras de acceso para mujeres indígenas."
  )) |>
  add_blank() |>
  add_p(paste0(
    "2. Salud mental masculina y prevención del suicidio: Implementar programas de salud mental ",
    "con perspectiva de género orientados a hombres en edad laboral, abordando las barreras de ",
    "búsqueda de ayuda a través de entornos de salud ocupacional e intervenciones comunitarias."
  )) |>
  add_blank() |>
  add_p(paste0(
    "3. Hipertensión y riesgo cardiovascular: Expandir las iniciativas de reducción de sodio en ",
    "alimentos procesados e intensificar el tamizaje comunitario de presión arterial, con foco ",
    "en estrategias de captación masculina en la atención primaria de salud."
  )) |>
  add_blank() |>
  add_h2("10.3 Recomendaciones para México") |>
  add_p(paste0(
    "1. Prevención de la violencia como política de salud pública: Encuadrar la reducción ",
    "del homicidio como prioridad de política sanitaria. Expandir los programas comunitarios ",
    "de prevención de violencia (modelo Cure Violence/Interruptores de Violencia) en municipios ",
    "de alta carga. Integrar la respuesta a la violencia de pareja íntima en el primer nivel de atención."
  )) |>
  add_blank() |>
  add_p(paste0(
    "2. Enfermedad crónica en mujeres: Desarrollar programas específicos de prevención y manejo ",
    "de la diabetes mellitus para mujeres en edad reproductiva, con énfasis en el tamizaje de ",
    "diabetes gestacional y el seguimiento metabólico posparto para interrumpir el ciclo ",
    "intergeneracional de riesgo metabólico."
  )) |>
  add_blank() |>
  add_p(paste0(
    "3. Protección financiera en salud: Acelerar la expansión de la cobertura del IMSS-Bienestar ",
    "para reducir el gasto de bolsillo, en particular para las trabajadoras del sector informal. ",
    "Establecer mecanismos de protección financiera ante enfermedades catastróficas para el manejo ",
    "continuo de ENT."
  )) |>
  add_blank() |>
  add_h2("10.4 Recomendaciones Transversales") |>
  add_p(paste0(
    "  \u2022  Ampliar la desagregación por sexo en los reportes de la OPS más allá de la ",
    "esperanza de vida, incluyendo la prevalencia de ENT, la salud mental y la utilización de ",
    "servicios de salud por sexo."
  )) |>
  add_p(paste0(
    "  \u2022  Adoptar un enfoque transformador de género en la política de salud, que aborde no ",
    "solo las diferencias biológicas de sexo sino los determinantes sociales que las producen."
  )) |>
  add_p(paste0(
    "  \u2022  Invertir en la desagregación subnacional de los datos para visibilizar a nivel ",
    "provincial/estatal los avances que los promedios nacionales ocultan, donde persisten las ",
    "mayores brechas de equidad."
  )) |>
  add_blank() |>
  body_add_break()

# ── NOTA TÉCNICA ──────────────────────────────────────────────────────────────
doc <- doc |>
  add_h1("Nota Técnica") |>
  add_p("Visualizaciones y Procesamiento de Datos") |>
  add_blank() |>
  add_p(paste0(
    "Todas las visualizaciones de este informe fueron generadas de manera programática mediante R ",
    "(ggplot2) a partir del mismo conjunto de datos de la OPS que alimenta el Panel de Indicadores ",
    "de Salud de las Américas. Los gráficos reflejan el año de datos disponible más reciente para ",
    "cada indicador y país. Los datos fueron procesados a través de un pipeline reproducible en R ",
    "(app.R) que excluye los valores negativos codificados por la OPS como datos suprimidos o ",
    "no confiables."
  )) |>
  add_blank() |>
  add_p("Indicadores utilizados:") |>
  body_add_table(
    data.frame(
      Sección     = c("III","IV","IV","V","V","V","VI","VI","VI","VII","VII","VIII"),
      Indicador   = c(
        "Población urbana (%), Razón de dependencia, Pob. 65+, Pob. <15 años",
        "Esperanza de vida al nacer (años) — total",
        "Esperanza de vida al nacer (años) — mujeres / hombres",
        "Razón de mortalidad materna (por 100.000 n.v.)",
        "Tasa de mortalidad infantil (por 1.000 n.v.)",
        "Causas externas, Homicidio, Lesiones viales, Suicidio — tasa de mortalidad",
        "ENT, Diabetes, Circulatorio, Respiratorio — tasas de mortalidad",
        "Prevalencia de sobrepeso y obesidad, diabetes, hipertensión en adultos (%)",
        "Tendencia de sobrepeso y obesidad en adultos",
        "Tasa de nuevos diagnósticos de VIH, incidencia de tuberculosis",
        "Cobertura de vacunación DTP3 y Triple Viral/MMR1 (%)",
        "Médicos por 10k hab., camas hospitalarias, gasto de bolsillo, gasto público"
      ),
      Tipo        = c("Contextual","Contextual","Desagregado por sexo",
                      "Específico femenino","Contextual","Predominio masculino",
                      "Agregado","Agregado","Tendencia agregada",
                      "Agregado","Contextual","Contextual"),
      stringsAsFactors = FALSE
    ),
    style = "Normal Table"
  ) |>
  add_blank() |>
  add_p(paste0(
    "Informe generado: 1 de abril de 2026 | ",
    "Panel de datos: https://ezequielbassa.shinyapps.io/3Epidemiology/ | ",
    "Datos: OPS/EIH Indicadores Básicos 2025"
  ))

# ── GUARDAR ────────────────────────────────────────────────────────────────────
print(doc, target = OUT_FILE)
cat("\nInforme guardado en:", OUT_FILE, "\n")
cat("Gráficos guardados en:", IMG_DIR, "\n")
