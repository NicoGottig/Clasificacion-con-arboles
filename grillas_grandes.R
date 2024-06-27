
# Herramientas de descriptiva y manipulación
library(tidyverse)
library(naniar)
library(knitr)
library(gridExtra)

# Modelos y ML
library(gbm)
library(xgboost)
library(lightgbm)
library(caret)
library(tidymodels)
library(pROC)
library(Metrics)

# Funciones y fuentes
source("funciones_utiles.R")

mate_pal <- c("#3B5998", "#FFA500", "#6B8E23", "#A52A2A", "#8A2BE2", "#DAA520", "#708090")

df <- read_delim("Data/deudores_limpio.txt", delim = "\t")
df_transformado <- read_delim("Data/deudores_limpio_transformaciones.txt", delim = "\t")

df$tipo_persona <- factor(df$tipo_persona)
df_transformado$tipo_persona <- factor(df_transformado$tipo_persona)
df <- data.frame(df)
df_transformado <- data.frame(df_transformado)
minmax = function(x) (x - min(x)) / (max(x) - min(x))

# escalamos
df_transformado = df_transformado %>%
  mutate_if(is.numeric, minmax) %>% 
  as.data.frame() 

# escalamos
df = df %>%
  mutate_if(is.numeric, minmax) %>% 
  as.data.frame() 

# Variables transformadas
set.seed(123)
train_index <- createDataPartition(df_transformado$default, p = 0.70, list = FALSE)
train_data <- df_transformado[train_index, ]
remaining_data <- df_transformado[-train_index, ]
test_index <- createDataPartition(remaining_data$default, p = 2/3, list = FALSE)
test_data <- remaining_data[test_index, ]
validation_data <- remaining_data[-test_index, ]

rm(train_index, remaining_data, test_index)

train_data$default <- factor(train_data$default, levels = c("0", "1"))

# Se borran las variables de plata
variables_dinero <- c("deuda_total_actual",
                      "deuda_con_garantia_actual",
                      "media_deuda_total",
                      "media_deuda_situacion_1",
                      "media_deuda_situacion_2",
                      "media_deuda_con_garantia",
                      "media_deuda_sin_garantia",
                      "media_deuda_en_default")

variables_inmediatas <- c("n_deudas_actual", 
                          "situacion_mes_actual",
                          "tiene_garantia_actual",
                          "mora_30_dias_mes_actual")

df <- df %>% 
  select(setdiff(colnames(df), variables_dinero)) 

df <- df %>% 
  select(setdiff(colnames(df), variables_inmediatas))

set.seed(123)
train_index_original <- createDataPartition(df$default, p = 0.70, list = FALSE)
train_data_original <- df[train_index_original, ]
remaining_data <- df[-train_index_original, ]
test_index_original <- createDataPartition(remaining_data$default, p = 2/3, list = FALSE)
test_data_original <- remaining_data[test_index_original, ]
validation_data_original <- remaining_data[-test_index_original, ]
rm(train_index_original, remaining_data, test_index_original)
train_data_original$default <- factor(train_data_original$default, levels = c("0", "1"))

# Modificaciones sobre transformadas
train_data <- train_data %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(es_27 = ifelse(tipo_persona == "27", 1,0),
         es_20 = ifelse(tipo_persona == "20", 1,0)) %>%
  select(-tipo_persona)

test_data <- test_data %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(es_27 = ifelse(tipo_persona == "27", 1,0),
         es_20 = ifelse(tipo_persona == "20", 1,0)) %>%
  select(-tipo_persona)

validation_data <- validation_data %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(es_27 = ifelse(tipo_persona == "27", 1,0),
         es_20 = ifelse(tipo_persona == "20", 1,0)) %>%
  select(-tipo_persona)

# Con tidy
if (any(train_data$default == 2)) {
  train_data$default <- ifelse(train_data$default == 2, 1, 0)
}

indice_default <- which(colnames(train_data) == "default")

train_x = as.matrix(train_data[, -indice_default])
train_y = as.numeric(as.character(train_data$default))
test_x  = as.matrix(test_data[, -indice_default])
test_y  = as.numeric(as.character(test_data$default))
val_x = as.matrix(validation_data[, -indice_default])
val_y = as.numeric(as.character(validation_data$default))

# # Crear el conjunto de datos DMatrix para XGBoost
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x, label  = test_y)

# # Lista de conjuntos de datos para monitorear
watchlist <- list(train = dtrain, test = dtest)


# Modificaciones sobre originales
train_data_original <- train_data_original %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(es_27 = ifelse(tipo_persona == "27", 1,0),
         es_20 = ifelse(tipo_persona == "20", 1,0)) %>%
  select(-tipo_persona)

test_data_original <- test_data_original %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(es_27 = ifelse(tipo_persona == "27", 1,0),
         es_20 = ifelse(tipo_persona == "20", 1,0)) %>%
  select(-tipo_persona)

validation_data_original <- validation_data_original %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(es_27 = ifelse(tipo_persona == "27", 1,0),
         es_20 = ifelse(tipo_persona == "20", 1,0)) %>%
  select(-tipo_persona)

# Con tidy
if (any(train_data_original$default == 2)) {
  train_data_original$default <- ifelse(train_data_original$default == 2, 1, 0)
}

indice_default <- which(colnames(train_data_original) == "default")

train_x_original = as.matrix(train_data_original[, -indice_default])
train_y_original = as.numeric(as.character(train_data_original$default))
test_x_original  = as.matrix(test_data_original[, -indice_default])
test_y_original  = as.numeric(as.character(test_data_original$default))
val_x_original = as.matrix(validation_data_original[, -indice_default])
val_y_original = as.numeric(as.character(validation_data_original$default))

# # Crear el conjunto de datos DMatrix para XGBoost
dtrain_original <- xgb.DMatrix(data = train_x_original, label = train_y_original)
dtest_original  <- xgb.DMatrix(data = test_x_original, label  = test_y_original)

# Ahora ajustamos modelo light gbm transformado
dtrain_lgb_standar <- lgb.Dataset(data = train_x, label = train_y)
dtest_lgb_standar  <- lgb.Dataset(data = test_x, label = test_y, reference = dtrain_lgb)

# Genero la grilla de hiperparámetros
grid_lgbm <- expand.grid(
  num_leaves       = c(50, 100, 300),
  learning_rate    = c(0.001, 0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.1),
  feature_fraction = c(0.05, 0.10, 0.5, 0.6),
  bagging_fraction = 0.8,
  max_depth        = c(10, 50, 100),
  min_data_in_leaf = c(1, 10),
  nrounds          = c(100, 500, 1000))

for (i in 1:nrow(grid_lgbm)) {
params <- list(
  objective        = "binary",
  metric           = "auc",
  num_leaves       = grid_lgbm$num_leaves[i],
  learning_rate    = grid_lgbm$learning_rate[i],
  feature_fraction = grid_lgbm$feature_fraction[i],
  bagging_fraction = grid_lgbm$bagging_fraction[i],
  max_depth        = grid_lgbm$max_depth[i],
  min_data_in_leaf = grid_lgbm$min_data_in_leaf[i],
  feature_pre_filter = FALSE
)


# Realizar el entrenamiento con validación cruzada
cv <- lgb.cv(
  params                = params,
  data                  = dtrain_lgb,
  nrounds               = grid_lgbm$nrounds[i],
  nfold                 = 5,
  verbose               = -1,
  early_stopping_rounds = 10,
  eval                  = "auc"
)

grid_lgbm$auc[i] <- cv$best_score

print(paste(round(i / nrow(grid_lgbm), 4) * 100, "%"))
}

mejor_lgbm_estandar <- grid_lgbm %>% arrange(-auc)

o# Light gbm para variables originales 
# # Lista de conjuntos de datos para monitorear
dtrain_lgb_original <- lgb.Dataset(data = train_x_original, label = train_y_original)
dtest_lgb_original  <- lgb.Dataset(data = test_x_original, label = test_y_original, reference = dtrain_lgb_original)

# Genero la grilla de hiperparámetros
grid_lgbm_original <- expand.grid(
  num_leaves       = c(50, 100, 300),
  learning_rate    = c(0.001, 0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.1),
  feature_fraction = c(0.05, 0.10, 0.5, 0.6),
  bagging_fraction = 0.8,
  max_depth        = c(10, 50, 100),
  min_data_in_leaf = c(1, 10),
  nrounds          = c(100, 500, 1000))

for (i in 1:nrow(grid_lgbm_original)) {
  params <- list(
    objective        = "binary",
    metric           = "auc",
    num_leaves       = grid_lgbm_original$num_leaves[i],
    learning_rate    = grid_lgbm_original$learning_rate[i],
    feature_fraction = grid_lgbm_original$feature_fraction[i],
    bagging_fraction = grid_lgbm_original$bagging_fraction[i],
    max_depth        = grid_lgbm_original$max_depth[i],
    min_data_in_leaf = grid_lgbm_original$min_data_in_leaf[i],
    feature_pre_filter = FALSE
  )
  
  
  # Realizar el entrenamiento con validación cruzada
  cv <- lgb.cv(
    params                = params,
    data                  = dtrain_lgb_original,
    nrounds               = grid_lgbm_original$nrounds[i],
    nfold                 = 5,
    verbose               = -1,
    early_stopping_rounds = 10,
    eval                  = "auc"
  )
  
  grid_lgbm_original$auc[i] <- cv$best_score
  
  print(paste(round(i / nrow(grid_lgbm_original), 4) * 100, "%"))
}

mejor_lgbm_estandar_original <- mejor_lgbm_estandar %>% arrange(-auc)

######
# CV #
######

# LGBM SIN VARIABLES ESTANDARIZADAS CON VARIABLES TRANSFORMADAS (ORIGINAL)
params_lgbm <- list(
  objective        = "binary",
  metric           = "auc",
  num_leaves       = 50,
  learning_rate    = 0.021,
  feature_fraction = 0.5,
  bagging_fraction = 0.8,
  max_depth        = 10,
  min_data_in_leaf = 1,
  feature_pre_filter = FALSE
)

cv_estandar <- lgb.cv(
  params                = params_lgbm,
  data                  = dtrain_lgb,
  nrounds               = 1000,
  nfold                 = 10,
  verbose               = -1,
  early_stopping_rounds = 10,
  eval                  = "auc"
)

cv_metrics <- cv_estandar[["record_evals"]][["valid"]]

metric_df <- data.frame(modelo = "LGBM Estandarizado c/variabes transformadas.", 
                        auc_mean = mean(cv_metrics$auc$eval %>% unlist()),
                        auc_sd = mean(cv_metrics$auc$eval_err %>% unlist()))

# LGBM CON 35 VARIABLES ESTANDARIZADAS

params_lgbm_estandar <- list(
  objective        = "binary",
  metric           = "auc",
  num_leaves       = 50,
  learning_rate    = 0.001,
  feature_fraction = 0.05,
  bagging_fraction = 0.8,
  max_depth        = 100,
  min_data_in_leaf = 1,
  feature_pre_filter = FALSE
)

# Realizar el entrenamiento con validación cruzada
cv_estandar <- lgb.cv(
  params                = params_lgbm_estandar,
  data                  = dtrain_lgb,
  nrounds               = 1000,
  nfold                 = 10,
  verbose               = -1,
  early_stopping_rounds = 10,
  eval                  = "auc"
)
cv_metrics <- cv_estandar[["record_evals"]][["valid"]]

metric_df <- rbind(metric_df, 
                   data.frame(modelo = "LGBM Est. c/variabes transformadas.", 
                              auc_mean = mean(cv_metrics$auc$eval %>% unlist()),
                              auc_sd = mean(cv_metrics$auc$eval_err %>% unlist())))

# LGBM CON VARIABLES ESTANDARIZADAS SIN VARIABLES TRANSFORMADAS

params_lgbm_estandar_original <- list(
  objective        = "binary",
  metric           = "auc",
  num_leaves       = 50,
  learning_rate    = 0.001,
  feature_fraction = 0.05,
  bagging_fraction = 0.8,
  max_depth        = 100,
  min_data_in_leaf = 1,
  feature_pre_filter = FALSE
)

# Realizar el entrenamiento con validación cruzada
cv_estandar <- lgb.cv(
  params                = params_lgbm_estandar_original,
  data                  = dtrain_lgb_original,
  nrounds               = 1000,
  nfold                 = 10,
  verbose               = -1,
  early_stopping_rounds = 10,
  eval                  = "auc"
)
cv_metrics <- cv_estandar[["record_evals"]][["valid"]]

metric_df <- rbind(metric_df, 
                   data.frame(modelo = "LGBM Est. sin variables transformadas", 
                              auc_mean = mean(cv_metrics$auc$eval %>% unlist()),
                              auc_sd = mean(cv_metrics$auc$eval_err %>% unlist())))

# guardo
write_delim(metric_df, file = "Data/data_variables_transformadas.txt", delim = "\t")
