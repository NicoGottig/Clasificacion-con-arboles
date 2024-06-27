library(knitr)
library(gridExtra)
library(gbm)
library(xgboost)
library(lightgbm)
library(caret)
library(tidymodels)
library(pROC)
library(tidyverse)
library(Metrics)
source("funciones_utiles.R")

mate_pal <- c("#3B5998", "#FFA500", "#6B8E23", "#A52A2A", "#8A2BE2", "#DAA520", "#708090")
df_transformado <- read_delim("Data/deudores_limpio_transformaciones.txt", delim = "\t")
df_transformado$tipo_persona <- factor(df_transformado$tipo_persona)
df_transformado$tipo_persona <- factor(df_transformado$tipo_persona)
df_transformado <- data.frame(df_transformado)
df_transformado$max_sit_mes_sin_garantia <- NULL

# Dataframe sin transformaciones
df <- read.delim("Data/deudores_limpio.txt")

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

# Conjuntos para modelar
df$UNIFORME1 <- runif(n = nrow(df),
                        min = 0,
                        max = 1)
df$UNIFORME2 <- runif(n = nrow(df),
                        min = 2,
                        max = 5)

train_index <- createDataPartition(df$default, p = 0.70, list = FALSE)
train_data <- df[train_index, ]
remaining_data <- df[-train_index, ]
test_index <- createDataPartition(remaining_data$default, p = 2/3, list = FALSE)
test_data <- remaining_data[test_index, ]
validation_data <- remaining_data[-test_index, ]
rm(train_index, remaining_data, test_index)
train_data$default <- factor(train_data$default, levels = c("0", "1"), labels = c("0", "1"))

# Hagamos un cluster
cluster_cols <- c("prop_default_seg", "prop_mora_30_dias_seg",
                  "prop_tuvo_garantia", "media_prop_con_garantia", 
                  "media_prop_default", "media_prop_situacion_1",
                  "media_prop_situacion_2", "proxy_edad_actual")

cluster_data <- df[, cluster_cols]
cluster_data <- cluster_data %>% 
  mutate_if(is.numeric, scale)

# Cant. de clusters

# Kmeans
factoextra::fviz_nbclust(cluster_data, kmeans, method = "wss")
kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)

# Visualizar los clusters
factoextra::fviz_cluster(kmeans_result, data = cluster_data)

# Agrego clusters a dataframe
df$cluster <- kmeans_result$cluster
cluster <- data.frame(id = rownames(df), 
                      cluster = kmeans_result$cluster)
  
write_delim(cluster, "Data/clusters_id_18006.txt", delim = "\t")

df <- df %>% 
  select(-one_of(cluster_cols))

df$default <- factor(df$default)

# Modelo RF - Comparo algunos árboles
rf_resultados <- function(mtry, trees, min_n, maxnodes){
  rf = randomForest::randomForest(
    formula = default ~ .,
    mtry = mtry,
    trees = trees,
    maxnodes = maxnodes,
    min_n = min_n,
    data = df
  )
  pred = predict(rf, test_data, type = "prob")[,2]
  as.numeric(pROC::auc(test_data$default, pred))
}

grid_rf = expand.grid(
  trees = c(400, 1000),
  mtry = c(6,10),
  min_n = c(34, 100),
  maxnodes = c(30, 50, 150))

grid_rf$auc <- map_dbl(1:nrow(grid_rf),
                       ~rf_resultados(
                         mtry = grid_rf$mtry[.x],
                         trees = grid_rf$trees[.x],
                         min_n = grid_rf$min_n[.x],
                         maxnodes = grid_rf$maxnodes[.x]
                         ))

best_params <- grid_rf %>% arrange(-auc) %>% head(n=1)

# Ajuste del modelo final
rf_final = randomForest::randomForest(
  formula = default ~ .,
  mtry = best_params$mtry,
  trees = best_params$trees,
  min_n = best_params$min_n,
  max_nodes = best_params$maxnodes,
  data = train_data)

randomForest::varImpPlot(rf_final,
                         sort = TRUE,
                         main = "") %>% 
  data.frame() %>% 
  arrange(MeanDecreaseGini)

