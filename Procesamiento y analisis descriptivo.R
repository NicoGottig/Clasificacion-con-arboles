
# Procesamiento de la base de datos y análisis descriptivo
library(tidyverse)
library(janitor)
library(naniar)

# Importo la base original
df <- read_rds("Data/df_bcra_individuals.rds")

# Estudio nulos
miss_var_summary(df)

# Las variables 'max_sit_mes_con_garantia' y 'max_sit_mes_sin_garantia' tienen valores nulos
# Sobretodo con garantia, porque no muchos cuits tienen garantía

# Agrupo tipo de personas
df$tipo_persona <- case_when(df$tipo_persona %in% as.factor(c(23, 24)) ~ "Global", 
                             TRUE ~ df$tipo_persona)

# Se eliminan las columnas que no sirven
df$max_sit_mes_con_garantia <- NULL
df$peor_situacion_respuesta <- NULL
df$mora_mayor_30_dias <- NULL
df$id_individuo <- NULL

# Convierto caracteres a factor y filtro casos que no corresponden
df <- df %>% 
  mutate_if(is.character, as.factor)

df <- df %>% 
  filter(proxy_edad_actual < 600)

# Evalúo repetidos: elimino los 13 casos duplicados
df$duplicado <- duplicated(df)
df <- df %>% filter(duplicado == FALSE) %>% select(-duplicado)
duplicated(df) %>% table()

# Creo variables auxiliares

# 'tiene_default_historico' toma valores 1 si la persona entró alguna vez en default
# y 0 si nunca lo hizo
df$tiene_default_historico = if_else(df$media_prop_default == 0, 0, 1)

# 'tiene_garantia_historica' toma valor 1 si la persona tuvo garantía histórica y 0 si no lo tuvo
df$tiene_garantia_historica <- if_else(df$media_prop_con_garantia == 0, 0, 1)

# Ahora separamos los factores de las numéricas
contar_unicos <- function(x){
  p <- unique(x) %>% 
    length()
  return(p)
}
apply(df, 2, contar_unicos)

# Factores:
# tipo_persona
# situacion_mes_actual 
# tiene_garantia_actual
df$mora_30_dias_mes_actual <- factor(df$mora_30_dias_mes_actual, levels = c("0", "1"))
# max_situacion_mes
# max_sit_mes_sin_garantia
df$tiene_default_historico <- factor(df$tiene_default_historico, levels = c("0", "1"))
df$default <- factor(df$default, levels = c("0", "1"))
df$tiene_garantia_historica <- factor(df$tiene_garantia_historica, levels = c("0", "1"))

# Almaceno el dataframe final
write_delim(df, "Data/deudores_limpio.txt", delim = "\t")

para_transformar <- c("proxy_edad_actual", 
                      "prop_con_garantia_actual",
                      "n_meses_seg_bcra",
                      "media_prop_situacion_1",
                      "media_prop_situacion_2",
                      "media_prop_default",
                      "media_prop_con_garantia",
                      "prop_tuvo_garantia",
                      "prop_mora_30_dias_seg",
                      "prop_default_seg")

df$id <- rownames(df)

# Transformaciones numéricas
df_sqrt <- df %>% 
  select(c(id, any_of(para_transformar))) %>% 
  mutate(across(where(is.numeric), sqrt, .names = "{col}_sqrt")) %>% 
  select(c(id, ends_with("_sqrt")))

df_potencia <- df %>% 
  select(c(id, any_of(para_transformar))) %>% 
  mutate(across(where(is.numeric), ~.^2, .names = "{col}_pot")) %>% 
  select(c(id, ends_with("_pot")))

df <- merge(df, df_sqrt, by = "id")
df <- merge(df, df_potencia, by = "id")

df$id <- NULL

# Almaceno el dataframe final transformado
write_delim(df, "Data/deudores_limpio_transformaciones.txt", delim = "\t")
