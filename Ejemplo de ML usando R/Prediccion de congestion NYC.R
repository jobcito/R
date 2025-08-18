# Manejo de datos
library(tidyverse)
# ML
library(caret)
# Para trabajar con fechas
library(lubridate)

library(RSocrata)

# Cargar datos directamente desde NYC Open Data

url <- "https://data.cityofnewyork.us/resource/ertz-hr4r.csv"
#traffic <- read.socrata(url)
traffic <- read.csv("Traffic_Volume_Counts_20250816.csv")
head(traffic)

# Paquetes
library(dplyr)
library(lubridate)
library(janitor)
library(stringr)
library(rlang)