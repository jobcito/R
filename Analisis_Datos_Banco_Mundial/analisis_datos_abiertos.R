# Script para análisis de datos abiertos del Banco Mundial
# Descarga indicadores económicos y realiza análisis básico

# 1. Instalar y cargar paquetes necesarios (solo si no están instalados)
if (!require("WDI")) install.packages("WDI")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(WDI)       # Para acceder a datos del Banco Mundial
library(ggplot2)   # Para visualización
library(dplyr)     # Para manipulación de datos

# 2. Definir los indicadores y países de interés
indicadores <- c(
  "NY.GDP.PCAP.KD",  # PIB per cápita (constante 2015 US$)
  "SP.POP.TOTL",     # Población total
  "IT.NET.USER.ZS"  # Usuarios de Internet (% de población)
)

paises <- c("MX", "US", "BR", "ES", "CN", "IN", "CL") # Códigos de países ISO3

# 3. Descargar los datos (2000-2022)
datos <- WDI(
  country = paises,
  indicator = indicadores,
  start = 2000,
  end = 2022,
  extra = TRUE
)

# 4. Limpieza y transformación de datos
datos_procesados <- datos %>%
  rename(
    pib_per_capita = NY.GDP.PCAP.KD,
    poblacion = SP.POP.TOTL,
    usuarios_internet = IT.NET.USER.ZS
  ) %>%
  filter(!is.na(pib_per_capita)) %>%
  mutate(
    region = as.factor(region),
    pib_total = pib_per_capita * poblacion / 1e9  # PIB en miles de millones
  )

# 5. Análisis exploratorio
cat("\nResumen estadístico:\n")
print(summary(datos_procesados %>% select(pib_per_capita, usuarios_internet)))

cat("\nPaíses con mayor crecimiento de PIB per cápita (2000-2022):\n")
crecimiento_pib <- datos_procesados %>%
  group_by(country) %>%
  summarise(
    crecimiento = (last(pib_per_capita) - first(pib_per_capita)) / first(pib_per_capita) * 100
  ) %>%
  arrange(desc(crecimiento))

print(crecimiento_pib)

# 6. Visualizaciones con fondo blanco

# Gráfico 1: Evolución del PIB per cápita por país
ggplot(datos_procesados, aes(x = year, y = pib_per_capita, color = country)) +
  geom_line(size = 1) +
  labs(
    title = "Evolución del PIB per cápita (2000-2022)",
    subtitle = "Dólares constantes de 2015",
    x = "Año",
    y = "PIB per cápita (US$)",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Gráfico 2: Relación entre PIB per cápita y usuarios de Internet
ultimo_anio <- datos_procesados %>%
  filter(year == max(year))

ggplot(ultimo_anio, aes(x = pib_per_capita, y = usuarios_internet, size = poblacion, color = region)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = country), vjust = -1, size = 3) +
  scale_size(range = c(3, 10)) +
  labs(
    title = "Relación entre PIB per cápita y penetración de Internet",
    subtitle = paste("Año:", max(ultimo_anio$year)),
    x = "PIB per cápita (US$ constantes)",
    y = "Usuarios de Internet (% población)",
    size = "Población",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# 7. Exportar resultados con fondo blanco
write.csv(datos_procesados, "datos_banco_mundial_procesados.csv", row.names = FALSE)
ggsave("grafico_pib_per_capita.png", width = 10, height = 6, bg = "white")
ggsave("grafico_relacion_pib_internet.png", width = 10, height = 6, bg = "white")

# 8. Análisis adicional: Correlación entre variables
correlaciones <- datos_procesados %>%
  filter(year == max(year)) %>%
  select(pib_per_capita, usuarios_internet) %>%
  cor(use = "complete.obs")

cat("\nMatriz de correlación (último año disponible):\n")
print(correlaciones)