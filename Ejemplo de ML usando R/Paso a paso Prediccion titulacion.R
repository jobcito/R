# ======================================
# ML: Predicción de titulación a 6 años
# ======================================

# --------------------------
# 0) Librerías necesarias
# --------------------------
library(dplyr)
library(lubridate)
library(caret)
library(randomForest)
library(readr)  # Para read_csv2

# --------------------------
# 1) Cargar los datos
# --------------------------
matri <- read.csv2("data/extract/matricula/20230802_Matrícula_Ed_Superior_2024_PUBL_MRUN.csv")
titu  <- read.csv2("data/extract/titulados/20250718_Titulados_Ed_Superior_2024_WEB.csv")

# --------------------------
# 2) Parsear fecha de titulación
# --------------------------
titu <- titu %>%
  mutate(
    fecha_parsed = parse_date_time(
      fecha_obtencion_titulo,
      orders = c("Ymd","Y-m-d","dmY","d/m/Y","mdY","m/d/Y","Ymd HMS","Y-m-d HMS"),
      tz = "UTC"
    ),
    anio_tit = year(fecha_parsed)
  )

# --------------------------
# 3) Cohorte de ingreso 2019
# --------------------------
cohorte2019 <- matri %>%
  filter(anio_ing_carr_act == 2019) %>%
  distinct(codigo_unico, cod_inst, cod_carrera, .keep_all = TRUE)

# --------------------------
# 4) Unir con titulados
# --------------------------
base <- cohorte2019 %>%
  left_join(
    titu %>% select(codigo_unico, cod_carrera, anio_tit, dur_total_carr),
    by = c("codigo_unico", "cod_carrera")
  )

# --------------------------
# 5) Variable objetivo
# --------------------------
base <- base %>%
  mutate(
    titulado_6y = ifelse(!is.na(anio_tit) & (anio_tit - anio_ing_carr_act <= 6), 1, 0)
  )

# --------------------------
# 6) Selección de predictores
# --------------------------
model_data <- base %>%
  select(
    titulado_6y, gen_alu, rango_edad, tipo_inst_1,
    modalidad, jornada, area_conocimiento, region_sede,
    valor_arancel, valor_matricula
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(c(valor_arancel, valor_matricula),
                ~ ifelse(is.na(.x), median(.x, na.rm=TRUE), .x)))

# --------------------------
# 7) Convertir factores a dummies
# --------------------------
set.seed(123)

# Excluir variable de respuesta para dummyVars
predictors <- setdiff(names(model_data), "titulado_6y")
dummy <- dummyVars(~ ., data = model_data[, predictors], fullRank = TRUE)

train_matrix <- predict(dummy, model_data[, predictors])
y <- as.factor(model_data$titulado_6y)

# --------------------------
# 8) Entrenar Random Forest
# --------------------------
rf_model <- randomForest(x = train_matrix, y = y, ntree = 500, importance = TRUE)

print(rf_model)
importance(rf_model)

# --------------------------
# 9) Predecir nuevo estudiante
# --------------------------
nuevo <- data.frame(
  gen_alu = factor("M", levels = levels(model_data$gen_alu)),
  rango_edad = factor("18-20", levels = levels(model_data$rango_edad)),
  tipo_inst_1 = factor("Universidad", levels = levels(model_data$tipo_inst_1)),
  modalidad = factor("Presencial", levels = levels(model_data$modalidad)),
  jornada = factor("Diurna", levels = levels(model_data$jornada)),
  area_conocimiento = factor("Ingeniería", levels = levels(model_data$area_conocimiento)),
  region_sede = factor("Metropolitana", levels = levels(model_data$region_sede)),
  valor_arancel = 1000000,
  valor_matricula = 50000
)

nuevo_matrix <- predict(dummy, nuevo)
predict(rf_model, nuevo_matrix, type="prob")

# --------------------------
# 10) Validación del modelo (train/test split)
# --------------------------
library(pROC)

set.seed(123)
train_idx <- sample(seq_len(nrow(model_data)), size = 0.7 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

# Dummies para training y test
dummy_val <- dummyVars(~ ., data = train_data[, predictors], fullRank = TRUE)
train_matrix_val <- predict(dummy_val, train_data[, predictors])
test_matrix_val  <- predict(dummy_val, test_data[, predictors])

y_train <- as.factor(train_data$titulado_6y)
y_test  <- as.factor(test_data$titulado_6y)

# Entrenar Random Forest
rf_val_model <- randomForest(x = train_matrix_val, y = y_train, ntree = 500)

# Predicción en test
pred_prob <- predict(rf_val_model, test_matrix_val, type = "prob")[,2]
pred_class <- predict(rf_val_model, test_matrix_val, type = "response")

# Matriz de confusión y métricas
conf_matrix <- table(Predicted = pred_class, Actual = y_test)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 4), "\n")

roc_obj <- roc(as.numeric(y_test), pred_prob)
cat("AUC:", round(auc(roc_obj), 4), "\n")
