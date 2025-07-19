# Script: Análisis de datos COVID-19 desde Our World in Data

# 1. Cargar librerías necesarias
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# 2. Descargar datos actualizados de OWID
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
destfile <- "owid-covid-data.csv"
download.file(url, destfile, method = "libcurl")

# 3. Leer el archivo CSV
covid <- read_csv(destfile, show_col_types = FALSE)

# 4. Filtrar países por código ISO y últimos 2391 días
paises <- c("CHL", "USA", "BRA", "IND", "ESP")

datos_filtrados <- covid %>%
  filter(iso_code %in% paises) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= Sys.Date() - 2391)

# 5. Agregar columnas útiles
datos_filtrados <- datos_filtrados %>%
  mutate(
    tasa_mortalidad = ifelse(total_cases > 0, total_deaths / total_cases * 100, NA_real_),
    semana = floor_date(date, unit = "week")
  )

# 6. Resumen estadístico
cat("\nResumen de casos y muertes totales (últimos 2391 días):\n")

resumen <- datos_filtrados %>%
  group_by(location) %>%
  summarise(
    casos_totales = max(total_cases, na.rm = TRUE),
    muertes_totales = max(total_deaths, na.rm = TRUE),
    tasa_mortalidad = ifelse(casos_totales > 0, muertes_totales / casos_totales * 100, NA_real_)
  )

print(resumen)

# 7. Visualización: Casos nuevos por semana
grafico_casos <- ggplot(datos_filtrados, aes(x = semana, y = new_cases, color = location)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Casos nuevos de COVID-19 por semana",
    subtitle = "Últimos 2391 días",
    x = "Semana",
    y = "Nuevos casos diarios",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(grafico_casos)

# 8. Exportar resultados
write.csv(resumen, "resumen_covid.csv", row.names = FALSE)
ggsave("grafico_casos_covid.png", plot = grafico_casos, width = 10, height = 6, bg = "white")
