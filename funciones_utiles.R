# Función para categorizar en deciles ####
categorizar <- function(df, var){
  
  # Obtener el nombre de la variable y aplicarlo a la columna categ
  df <- df %>%
    mutate(categ = case_when(
      get(var) <= quantile(get(var), 0.1, na.rm = TRUE) ~ "d_1",
      get(var) <= quantile(get(var), 0.2, na.rm = TRUE) ~ "d_2",
      get(var) <= quantile(get(var), 0.3, na.rm = TRUE) ~ "d_3",
      get(var) <= quantile(get(var), 0.4, na.rm = TRUE) ~ "d_4",
      get(var) <= quantile(get(var), 0.5, na.rm = TRUE) ~ "d_5",
      get(var) <= quantile(get(var), 0.6, na.rm = TRUE) ~ "d_6",
      get(var) <= quantile(get(var), 0.7, na.rm = TRUE) ~ "d_7",
      get(var) <= quantile(get(var), 0.8, na.rm = TRUE) ~ "d_8",
      get(var) <= quantile(get(var), 0.9, na.rm = TRUE) ~ "d_9",
      TRUE ~ "d_10"
    ))
  
  # Convertir la columna categ a factor ordenado
  df$categ <- factor(
    df$categ, 
    levels  = c("d_1", "d_2", "d_3", "d_4", "d_5", "d_6", "d_7", "d_8", "d_9", "d_10"),
    ordered = TRUE
  )
  
  # Cambiar el nombre de la nueva columna categorizada
  new_var_name <- paste0(var, "_cat")
  colnames(df)[colnames(df) == "categ"] <- new_var_name
  
  return(df[,new_var_name])
}

categorizar_deciles_columna <- function(col) {
  
  original <- col
  
  categorizado <- case_when(
  original <= quantile(original, 0.1, na.rm = TRUE) ~ "d_1",
  original <= quantile(original, 0.2, na.rm = TRUE) ~ "d_2",
  original <= quantile(original, 0.3, na.rm = TRUE) ~ "d_3",
  original <= quantile(original, 0.4, na.rm = TRUE) ~ "d_4",
  original <= quantile(original, 0.5, na.rm = TRUE) ~ "d_5",
  original <= quantile(original, 0.6, na.rm = TRUE) ~ "d_6",
  original <= quantile(original, 0.7, na.rm = TRUE) ~ "d_7",
  original <= quantile(original, 0.8, na.rm = TRUE) ~ "d_8",
  original <= quantile(original, 0.9, na.rm = TRUE) ~ "d_9",
  TRUE ~ "d_10")
  
  return(categorizado)
}


# Función para ajustar el modelo con test 
xgb_auc_test <- function(nrounds = 250,
                    max_depth = 3,               
                    eta = 0.1,
                    subsample = 1,               
                    colsample_bytree = 0.2,             
                    gamma = 0,       
                    min_child_weight = 1){
  
  params_xgb <- list(
    objective        = "binary:logistic", # para respuesta cuantitativa: "reg:squarederror"
    eval_metric      = "auc",             # para respuesta cuantitativa: "rmse"
    max_depth        = max_depth,    # Profundidad máxima de cada árbol
    eta              = eta, # Learnning rate - Tasa de aprendizaje
    gamma            = gamma,    # Mínimo de reducción de pérdida requerida para hacer una partición adicional
    subsample        = subsample,    # Proporción de muestras utilizadas para entrenar cada árbol
    colsample_bytree = colsample_bytree   # Proporción de características utilizadas para entrenar cada árbol
  )  
  
  # Ajustar el modelo XGBoost
  xgboost <- xgb.train(
    params    = params_xgb,
    data      = dtrain,
    nrounds   = nrounds,
    watchlist = watchlist,  # Lista de conjuntos de datos para monitorear
    verbose   = 0,           # Nivel de verbosidad
  )
  
  predictions <- predict(xgboost, test_x, type = "response")
  
  as.numeric(pROC::auc(test_y, predictions))
  
}

# Función para calcular métricas ####
calcular_metricas <- function(nombre = "modelo", y_real, y_prob, cutoff = 0.5) {
  
  # Raíz del Error Cuadrático Medio (RMSE)
  rmse_value <- Metrics::rmse(y_real, y_prob)
  
  # Área bajo la curva (AUC)
  roc_obj <- roc(response = y_real, predictor = y_prob)
  auc_value <- roc_obj$auc
  
  # Lift para el 10% de las probabilidades más altas
  n_top_10 <- ceiling(0.1 * length(y_prob))
  top_10_indices <- order(y_prob, decreasing = TRUE)[1:n_top_10]
  lift_value <- mean(y_real[top_10_indices]) / mean(y_real)
  
  # Predicciones binarizadas según el punto de corte
  y_pred <- ifelse(y_prob >= cutoff, 1, 0)
  
  # Matriz de confusión
  confusion_matrix <- table(y_real, y_pred)
  
  # Cálculo de las métricas
  accuracy    <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  recall      <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  precision   <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  f1_score    <- 2 * ((precision * recall) / (precision + recall))
  
  # Crear un data frame con las métricas
  metricas <- data.frame(
    Modelo      = nombre,
    RMSE        = rmse_value,
    AUC         = auc_value,
    Lift_10     = lift_value,
    Accuracy    = accuracy,
    Recall      = recall,
    Precision   = precision,
    Specificity = specificity,
    F1_Score    = f1_score
  )
  
  return(metricas)
}

# Función para ajustar el modelo con validation
xgb_auc_valid <- function(nrounds = 250,
                         max_depth = 3,               
                         eta = 0.1,
                         subsample = 1,               
                         colsample_bytree = 0.2,             
                         gamma = 0,       
                         min_child_weight = 1){
  
  params_xgb <- list(
    objective        = "binary:logistic", # para respuesta cuantitativa: "reg:squarederror"
    eval_metric      = "auc",             # para respuesta cuantitativa: "rmse"
    max_depth        = max_depth,    # Profundidad máxima de cada árbol
    eta              = eta, # Learnning rate - Tasa de aprendizaje
    gamma            = gamma,    # Mínimo de reducción de pérdida requerida para hacer una partición adicional
    subsample        = subsample,    # Proporción de muestras utilizadas para entrenar cada árbol
    colsample_bytree = colsample_bytree   # Proporción de características utilizadas para entrenar cada árbol
  )  
  
  # Ajustar el modelo XGBoost
  xgboost <- xgb.train(
    params    = params_xgb,
    data      = dtrain,
    nrounds   = nrounds,
    watchlist = watchlist,  # Lista de conjuntos de datos para monitorear
    verbose   = 0,           # Nivel de verbosidad
  )
  
  predictions <- predict(xgboost, val_x, type = "response")
  
  as.numeric(pROC::auc(val_y, predictions))
  
}

# Funcion de costo por mala clasificación

costear_predicciones <- function(y_original, y_pred, cutoff){
  
  predicciones <- ifelse(y_pred >= cutoff, 1, 0)
  
  confusion_matrix <- table(as.numeric(as.character(y_original)), 
                            predicciones)
  
  costo = 5 * confusion_matrix[2,1]
    
  return(costo)
  
}

it_predicciones <- function(y_original, y_pred, cutoff){
  
  predicciones <- ifelse(y_pred >= cutoff, 1, 0)
  
  confusion_matrix <- table(as.numeric(as.character(y_original)), 
                            predicciones)
  
  ingreso = confusion_matrix[1,1]
  costo = 5 * confusion_matrix[2,1]
  
  bt = ingreso - costo
  
  return(bt)
  
}

xgb_auc <- function(nrounds = 250,
                    max_depth = 3,               
                    eta = 0.1,
                    subsample = 1,               
                    colsample_bytree = 0.2,             
                    gamma = 0,       
                    min_child_weight = 1){
  
  params_xgb <- list(
    objective        = "binary:logistic", # para respuesta cuantitativa: "reg:squarederror"
    eval_metric      = "auc",             # para respuesta cuantitativa: "rmse"
    max_depth        = max_depth,    # Profundidad máxima de cada árbol
    eta              = eta, # Learnning rate - Tasa de aprendizaje
    gamma            = gamma,    # Mínimo de reducción de pérdida requerida para hacer una partición adicional
    subsample        = subsample,    # Proporción de muestras utilizadas para entrenar cada árbol
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight# Proporción de características utilizadas para entrenar cada árbol
  )  
  
  # Ajustar el modelo XGBoost
  xgboost <- xgb.train(
    params    = params_xgb,
    data      = dtrain,
    nrounds   = nrounds,
    watchlist = watchlist,  # Lista de conjuntos de datos para monitorear
    verbose   = 0,           # Nivel de verbosidad
  )
  
  predictions <- predict(xgboost, test_x, type = "response")
  
  as.numeric(pROC::auc(test_y, predictions))
}

# BT = INGRESO TOTAL - COSTO TOTAL
# SUPONIENDO 1 CREDITO APROBADO CORRECTAMENTE = 1 UNIDAD
# SUPONIENDO 1 MALA CLASIFICACION COSTO = 5 UNIDADES

