# Herramientas de descriptiva y manipulación
library(tidyverse)
library(naniar)
library(knitr)
library(gridExtra)
library(rpart)
library(rpart.plot) 
library(partykit)
library(CHAID)
library(gbm)
library(xgboost)
library(lightgbm)
library(caret)
library(tidymodels)
library(pROC)
library(Metrics)
source("funciones_utiles.R")
mate_pal <- c("#3B5998", "#FFA500", "#6B8E23", "#A52A2A", "#8A2BE2", "#DAA520", "#708090")
df <- read_delim("Data/deudores_limpio.txt", delim = "\t")
# df$max_sit_mes_sin_garantia <- NULL
df$tipo_persona <- factor(df$tipo_persona)
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

# Loopeo para encontrar los mejores (con validación cruzada)

set.seed(123)
train_index <- createDataPartition(df$default, p = 0.70, list = FALSE)
train_data <- df[train_index, ]
remaining_data <- df[-train_index, ]
test_index <- createDataPartition(remaining_data$default, p = 2/3, list = FALSE)
test_data <- remaining_data[test_index, ]
validation_data <- remaining_data[-test_index, ]

rm(train_index, remaining_data, test_index)

train_data$default <- factor(train_data$default, levels = c("0", "1"))

# Modificaciones para XGBoost
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

dtrain_lgb <- lgb.Dataset(data = train_x, label = train_y)
dtest_lgb  <- lgb.Dataset(data = test_x, label = test_y, reference = dtrain_lgb)

# Genero la grilla de hiperparámetros
grid_lgbm <- expand.grid(
  num_leaves       = c(50, 100, 300),
  learning_rate    = c(0.001, 0.003, 0.005, 0.02, 0.03, 0.08),
  feature_fraction = c(0.10, 0.5, 0.6),
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


grid_lgbm %>% arrange(-auc) %>% head(n=1)
