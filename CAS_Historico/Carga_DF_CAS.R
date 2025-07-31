# Cargar las librerías necesarias, se hace de esta forma por el tamaño del archivo
# y para evitar problemas de memoria al cargar el dataframe completo.
# Se utiliza data.table para una carga más eficiente de grandes datasets.

library(data.table)

datos <- fread("004-Carga_Académica_Semestral_REP006_historico_utf8.csv")

# Mostrar las primeras filas del dataframe, su estructura y un resumen estadístico
head(datos)
str(datos)
summary(datos)

# Filtrar por RUT, sin filtrar por columna especifica
registros_filtrados <- datos[datos$RUT == "", ]

# Mostrar los registros filtrados en un grid
View(registros_filtrados)

