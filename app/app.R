# Librerías necesarias
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(shinydashboard)
library(lightgbm)
library(bslib)
library(tidyverse)
library(gridExtra)
library(gganimate)
library(tidymodels)
library(pROC)
library(Metrics)
library(showtext)
library(shinyjs)

# Set-up ####

# Cargar la fuente Crimson Text desde Google Fonts
font_add_google("Roboto", "Roboto")
showtext_auto()

# Definir y aplicar el tema global
theme_set(
  theme_bw(base_family = "Roboto") +  # Comienza con theme_bw
    theme_minimal(base_family = "Roboto") +  # Luego aplica elementos de theme_minimal
    theme(
      plot.title = element_text(face = "bold"),  # Mantén el título en negrita
      panel.grid.major = element_blank(),  # Opcional: personalizar grid mayor
      panel.grid.minor = element_blank(),  # Opcional: ocultar grid menor
      panel.background = element_blank()  # Mantén el fondo en blanco
    )
)

# Colores y características de la página
mate_pal <- c("#3B5998", "#FFA500", "#6B8E23", "#A52A2A", "#8A2BE2", "#DAA520", "#708090")

# Importación de bases necesarias
# Gráfico de Random Forest
load("tuning_rf.RData")
tune_res_df <- tune_res %>%
  collect_metrics()
colnames(tune_res_df)[4] <- "metric"

# Gráfico de la evolución de los indicadores del light GBM
grid_lgbm <- read_delim("grid_lgbm.txt", delim = "\t")

# Optimización de punto de corte
optimizacion_p_modelo = read_delim("optimizacion_modelo.txt")

# Establecemos valores por default para que se modifiquen en toda la interface
proxy_edad_actuald = 387
prop_con_garantia_actuald = 0
n_meses_seg_bcrad = 0
max_situacion_mesd = 1
media_prop_situacion_1d = 0.99
media_prop_situacion_2d = 0.01
media_prop_defaultd = 0
media_prop_con_garantiad = 0
prop_tuvo_garantiad = 0
prop_mora_30_dias_segd = 0
prop_default_segd = 0
es_27d = 0
es_20d = 1
tiene_default_historicod = 0
tiene_garantia_historicad = 0

# Función para scorear para distintos valores de variables
scorear = function(proxy_edad_actual = proxy_edad_actuald,
                   prop_con_garantia_actual = prop_con_garantia_actuald,
                   n_meses_seg_bcra = n_meses_seg_bcrad, 
                   max_situacion_mes = max_situacion_mesd, 
                   media_prop_situacion_1 = media_prop_situacion_1d,
                   media_prop_situacion_2 = media_prop_situacion_2d,
                   media_prop_default = media_prop_defaultd,
                   media_prop_con_garantia = media_prop_con_garantiad,
                   prop_tuvo_garantia = prop_tuvo_garantiad,
                   prop_mora_30_dias_seg = prop_mora_30_dias_segd,
                   prop_default_seg = prop_default_segd,
                   es_27 = es_27d,
                   es_20 = es_20d,
                   tiene_default_historico = tiene_default_historicod,
                   tiene_garantia_historica = tiene_garantia_historicad){
  
  caso <- data.frame(
    proxy_edad_actual = proxy_edad_actual,
    prop_con_garantia_actual = prop_con_garantia_actual,
    n_meses_seg_bcra = n_meses_seg_bcra,            
    max_situacion_mes = max_situacion_mes,
    media_prop_situacion_1 = media_prop_situacion_1,
    media_prop_situacion_2 = media_prop_situacion_2,
    media_prop_default = media_prop_default,
    media_prop_con_garantia = media_prop_con_garantia,
    prop_tuvo_garantia = prop_tuvo_garantia,
    prop_mora_30_dias_seg = prop_mora_30_dias_seg,
    prop_default_seg =  prop_default_seg,
    
    proxy_edad_actual_sqrt = sqrt(proxy_edad_actual),
    prop_con_garantia_actual_sqrt = sqrt(prop_con_garantia_actual),
    n_meses_seg_bcra_sqrt = sqrt(n_meses_seg_bcra),
    media_prop_situacion_1_sqrt = sqrt(media_prop_situacion_1),
    media_prop_situacion_2_sqrt = sqrt(media_prop_situacion_2),  
    media_prop_default_sqrt = sqrt(media_prop_default),
    media_prop_con_garantia_sqrt = sqrt(media_prop_con_garantia),
    prop_tuvo_garantia_sqrt = sqrt(prop_tuvo_garantia),
    prop_mora_30_dias_seg_sqrt = sqrt(prop_mora_30_dias_seg),
    prop_default_seg_sqrt = sqrt(prop_default_seg),
    
    proxy_edad_actual_pot = proxy_edad_actual**2,
    prop_con_garantia_actual_pot = prop_con_garantia_actual**2,
    n_meses_seg_bcra_pot = n_meses_seg_bcra**2,
    media_prop_situacion_1_pot = media_prop_situacion_1**2,
    media_prop_situacion_2_pot = media_prop_situacion_2**2,  
    media_prop_default_pot = media_prop_default**2,
    media_prop_con_garantia_pot = media_prop_con_garantia**2,
    prop_tuvo_garantia_pot = prop_tuvo_garantia**2,
    prop_mora_30_dias_seg_pot = prop_mora_30_dias_seg**2,
    prop_default_seg_pot = prop_default_seg**2,
    es_27 = es_27,
    es_20 = es_20,
    tiene_default_historico = tiene_default_historico,
    tiene_garantia_historica = tiene_garantia_historica
  )
  
  nuevo_conjunto <- as.matrix(caso)
  caso$prob = predict(lightgbm, nuevo_conjunto, type = "response")
  caso
  
}

# df: Carga y tratamiento de datos ####
df <- read_delim("deudores_limpio.txt", delim = "\t")
df$max_sit_mes_sin_garantia <- NULL
df$tipo_persona <- factor(df$tipo_persona)

# Convierto las variables factores a factores 
df$tipo_persona <- as.factor(df$tipo_persona)
df$tiene_garantia_actual <- as.factor(df$tiene_garantia_actual)
df$mora_30_dias_mes_actual <- as.factor(df$mora_30_dias_mes_actual)
df$tiene_garantia_historica <- as.factor(df$tiene_garantia_historica)
df$default <- as.factor(df$default)
df$tiene_default_historico <- as.factor(df$tiene_default_historico)

# Cargar el modelo
load("lightgbm_modelo_1.RData")

# Actualización de nombres de variables para los bancarios
variable_choices <- c(
  "Número de Entidades con Deuda" = "n_deudas_actual",
  "Edad Aproximada (3 primeros dígitos del DNI)" = "proxy_edad_actual",
  "Monto Total de Deuda (en miles de pesos)" = "deuda_total_actual",
  "Monto de Deuda Garantizada (en miles de pesos)" = "deuda_con_garantia_actual",
  "Situación Crediticia Más Grave (Jun-2019)" = "situacion_mes_actual",
  "Proporción de Deuda Garantizada" = "prop_con_garantia_actual",
  "Meses en Seguimiento BCRA" = "n_meses_seg_bcra",
  "Promedio de Deuda Total" = "media_deuda_total",
  "Promedio de Deuda en Situación 1" = "media_deuda_situacion_1",
  "Promedio de Deuda en Situación 2" = "media_deuda_situacion_2",
  "Promedio de Deuda con Garantía" = "media_deuda_con_garantia",
  "Promedio de Deuda sin Garantía" = "media_deuda_sin_garantia",
  "Promedio de Deuda en Default" = "media_deuda_en_default",
  "Máxima Situación del Mes" = "max_situacion_mes",
  "Proporción en Situación 1" = "media_prop_situacion_1",
  "Proporción en Situación 2" = "media_prop_situacion_2",
  "Proporción en Default" = "media_prop_default",
  "Proporción con Garantía" = "media_prop_con_garantia",
  "Proporción de Meses con Garantía" = "prop_tuvo_garantia",
  "Proporción de Mora a 30 Días" = "prop_mora_30_dias_seg",
  "Proporción en Default en Seguimiento" = "prop_default_seg"
)

grupo_choices <- c(
  "Tipo de Persona (20: Hombres, 27: Mujeres)" = "tipo_persona",
  "Tiene Garantía Actual (Jun-2019)" = "tiene_garantia_actual",
  "Mora a 30 Días en Jun-2019" = "mora_30_dias_mes_actual",
  "Default Actual" = "default",
  "Tiene Default Histórico" = "tiene_default_historico",
  "Tiene Garantía Histórica" = "tiene_garantia_historica"
)

tabla.metricas = read_delim("metricas_finales_1.txt")
colnames(tabla.metricas) = c("Modelo", "RMSE", "AUC", "Lift", "Ac", "Recall", "Precision", "Specificity", "F1 Score")
tabla.metricas$Modelo = c("CART", "CHAID", "Reg. Logística", "Random Forest", "XGBoost", "LightGBM")

# Aplicacion ####

# Interfaz de usuario #####
ui = navbarPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Crimson+Text&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        font-size: 12px;  /* Ajusta el tamaño base del texto si es necesario */
      }
    "))
  ),
  

  title = div(
    tags$img(src = "banner_full.png", height = '30px', style = "margin-right: 10px; vertical-align: bottom;"),
    span("", style = "vertical-align: bottom; font-size: 22px;")
  ),
  
  header = tags$div(
    style = "position: absolute; right: 15px; top: 15px;",  # Ajuste la posición para mejor alineación
    tags$a(href = "https://github.com/NicoGottig", 
           tags$img(src = "git_logo.png", height = "30px", style = "vertical-align: bottom;"))
  ),
  
  tags$style(HTML("
  .navbar {
    background-color: #3B5998;
    border-color: #2c3e50;
    height: auto;  /* Allow the navbar height to adjust */
    padding-top: 5px;
    padding-bottom: 5px;
    font-size: 14px;
  }
  
  .navbar-brand {
    color: #FFFFFF !important;
    font-size: 12px;
    padding-top: 5px;  /* Adjust to align the title with the logo */
    padding-bottom: 0px;
  }
  
  .navbar-header {
    margin-left: 15px;
    padding-top: 10px;  /* Vertical alignment of logo and title */
    padding-bottom: 0px;
  }
  
  .container-fluid {
    max-width: 1200px;
    margin: auto;
  }
")),
  
  
  theme = bs_theme(bootswatch = "yeti", font_scale = 0.80),
  
  ## Pestaña de descripción ####
  tabPanel("Descripción",
           sidebarLayout(
             sidebarPanel(h4("Resumen"),
                          p(HTML("El <b>BCRA</b> a través de la central de deudores informa los montos adeudados y la clasificación de la deuda
                                 (del 1 al 5, siendo 1 la menos grave y 5 la más grave) de todos los tomadores de financiamiento. <br> 
                                 Se presenta a continuación información sobre 18.000 deudores para distintos instantes de tiempo. "), style = "text-align: justify;"),
                          p(HTML("Pueden seleccionarse todas las variables del conjunto de datos y segmentar los casos según distintos grupos. 
                            Los resultados se exponen en los gráficos y tablas a continuación. 
                            En la pestaña <b>Score crediticio</b> se puede otorgar, a una persona con determinadas características, la probabilidad
                            de encontrarse en <i>situación 3 o superior</i> en todas las deudas que posee <b>(de ahora en default)</b>. 
                            Información adicional del modelo se encuentra en la pestaña <i>Más</i>.<br>
                            <i>La experiencia de navegación mejora notablemente en computadoras.</i>"), style = "text-align: justify;"),
                          br(),
                          
                          h4("Análisis"),
                          selectInput("variable1", "Variable 1 (Eje Y):", choices = variable_choices, 
                                      selected = "media_deuda_situacion_2"),
                          selectInput("variable2", "Variable 2 (Eje X):", choices = variable_choices, 
                                      selected = "proxy_edad_actual"),
                          selectInput("grupo", "Grupo (Color):", choices = grupo_choices,
                                      selected = "default")),
             mainPanel(fluidRow(), fluidRow(
                       column(4, value_box("% de casos de default en la base de datos: ", 
                                           paste(round(100 * mean(as.numeric(as.character(df$default))), 2), "%"),
                                           width = 10,
                                           showcase = bsicons::bs_icon("info"),
                                           theme = "text-success",
                                           font_size = "50%")),
                       column(4, value_box("Prom. de deuda en situación 2: ", 
                                           paste("$", round(1000 * mean(as.numeric(as.character(df$media_deuda_situacion_2))), 1)),
                                           width = 10,
                                           showcase = bsicons::bs_icon("exclamation-triangle-fill"), 
                                           theme_color = "warning")),
                       column(4, value_box("Prom. de deuda en default: ", 
                                           paste("$", round(1000 * mean(as.numeric(as.character(df$media_deuda_en_default))), 1)),
                                           width = 10,
                                           showcase = bsicons::bs_icon("x-octagon-fill"),
                                           theme_color = "danger",
                                           style = "font-size: 9px;"))
                       ),
                       fluidRow(column(1),
                                column(10, 
                                       hr(),
                                       uiOutput("tableTitle"), 
                                       div(tableOutput("summaryTable"), style = "text-align:left;"),
                                       br(),
                                       uiOutput("plotTitle"),
                                       plotOutput("scatterPlot")),
                                column(1))
                       )
             )),
  
  ## Pestaña para calificar solicitud ####
  tabPanel("Score crediticio",
           sidebarLayout(
             
             sidebarPanel(
               h4("Probabilidad de default"),
               p(HTML("En este apartado se deben ingresar las características de la persona a la que se desea otorgar el <i>score</i>. 
                      Al hacer click en <i>estimar default</i> el modelo devuelve un valor entre 0 y 1: mientras que el 1 asegura que la persona entrará en default, 
                      el 0 indica asegura que no lo hará. <br> 
                      Además se indica si se debería otorgar o no un crédito. Esta indicación está basada
                      en una <b>función de costo ficticia</b> que tiene en cuenta el riesgo de entregar mal un crédito, aunque en cada organismo los
                      criterios son distintos). <br> Por último, con las ilustraciones se puede observar como varía la probabilidad de mora
                      de acuerdo a la cantidad relativa de deuda en cada situación. "), style = "text-align:justify;"),
               
               selectInput("genero", "Género:", choices = c("mujer", "varón")),
               selectInput("default_historico", "¿Tuvo default?:", choices = c("sí", "no"), selected = "no"),
               selectInput("garantia_historica", "¿Tuvo garantía?", choices = c("sí", "no"), selected = "no"),
               selectInput("max_situacion_mes", 
                           "Situación histórica más crítica:", 
                           choices = c("1","2")),
               numericInput("dni", "DNI (3 primeros dígitos):", value = proxy_edad_actuald),
               actionButton("predict", "Estimar default"),
               
               hr(),
               
               h4("Configuración adicional: "), 
               p(HTML("Si se posee información sobre el comportamiento histórico de la persona pueden utilizarse los deslizadores para mejorar la precisión
                      de la estimación. En caso contrario, se asigna automaticamente el mejor comportamiento crediticio."), style = "text-align:justify;"),
               
               # Luego los numericInput
               sliderInput("prop_con_garantia_actual", "Prop. con Garantía Actual:", min = 0, max = 1, value = prop_con_garantia_actuald),
               sliderInput("n_meses_seg_bcra", "N° Meses Seguimiento BCRA:", min = 0, max = 1, value = n_meses_seg_bcrad),
               p("La suma de la prop. de situación 1 y 2 debe ser 1:"),
               sliderInput("media_prop_sit_1", "Media Prop. Situación 1:", min = 0, max = 1, value = media_prop_situacion_1d),
               sliderInput("media_prop_sit_2", "Media Prop. Situación 2:", min = 0, max = 1, value = media_prop_situacion_2d),
               br(),
               sliderInput("media_prop_default", "Media Prop. Default:", min = 0, max = 1, value = media_prop_defaultd),
               sliderInput("media_prop_con_garantia", "Media Prop. con Garantía:", min = 0, max = 1, value = media_prop_con_garantiad),
               sliderInput("prop_tuvo_garantia", "Prop. Tuvo Garantía:", min = 0, max = 1, value = prop_tuvo_garantiad),
               sliderInput("prop_mora_30_dias_seg", "Prop. Mora 30 Días:", min = 0, max = 1, value = prop_mora_30_dias_segd),
               sliderInput("prop_default_seg", "Prop. Default Seguimiento:", min = 0, max = 1, value = prop_default_segd)
             ),
             
             mainPanel(fluidRow(),
                       fluidRow(column(1), column(5, plotOutput("prob_sit2"),),
                                column(6, 
                                       fluidRow(valueBoxOutput("default_prob_box", width = 12)),
                                       fluidRow(valueBoxOutput("reco_box", width = 12)))))
             
           )),
  
  ## Adicional ####
  navbarMenu("Más",
             tabPanel("Selección del modelo",
                      
                      fluidPage(column(12, h4("Resumen del Proceso de Selección del Modelo"),
                                p("El proceso de selección del modelo se centró en identificar el mejor algoritmo para la clasificación de deudores 
                                  incobrables basado en datos históricos. Se evaluaron múltiples algoritmos, incluyendo CART, CHAID, Random Forest (figura 1), 
                                  XGBoost y LightGBM (figura 2), utilizando un conjunto de datos históricos del BCRA. Para cada algoritmo, 
                                  se dividió el conjunto de datos en tres partes: entrenamiento (70%), prueba (20%), y validación (10%). 
                                  Se ajustaron los modelos iterando sobre distintos hiperparámetros para optimizar la capacidad predictiva, 
                                  medida principalmente a través del AUC (Area Under the Curve).", style = "text-align:justify;")),
                                
                                hr(),
                                fluidRow(column(6, plotOutput("random_forest")), column(6, plotOutput("lgbm_model"))),
                                hr(),
                                h4("Resultados de los Modelos"),
                                
                                p(HTML("El algoritmo LightGBM se destacó como el mejor clasificador, alcanzando un AUC de 0.78 en el conjunto de validación. Este resultado fue 
                                  ligeramente superior a otros modelos como XGBoost y la regresión logística, 
                                  que también mostraron un buen desempeño pero con AUCs menores. <br>
                                  El modelo final seleccionado fue LightGBM (con variables transformadas en potencia y raíz), éstas proporcionaron la mejor combinación entre
                                  precisión y eficiencia en el proceso de clasificación. 
                                  Para maximizar los beneficios, se optimizó el punto de corte del modelo seleccionado, 
                                  identificando que un valor de 0.20 era el más adecuado para equilibrar el costo de 
                                  falsos negativos con los beneficios obtenidos de las predicciones correctas. 
                                  El modelo LightGBM, con los parámetros optimizados, fue seleccionado como el más 
                                  efectivo para la clasificación de deudores en este estudio. 
                                  No obstante, se recomienda utilizar este modelo con precaución en evaluaciones 
                                  futuras, dado que se basa en características recientes de los CUITs analizados, además de 
                                  representar solo a deudores (personas con historial crediticio) y sin un diagnóstico robusto sobre
                                  la calidad de los datos (de acuerdo al objetivo) que provee BCRA. A continuación se describe el resumen de la iteración y la optimización del valor de cut-off."), 
                                  style = "text-align:justify;"),
                                hr(),
                                
                                fluidRow(column(6, plotOutput("optimizacion_p")), column(1)) 
                                
                                )),
             
             tabPanel("Acerca De", 
                      fluidPage(column(1),
                                column(10,
                                       h4("Clasificación de Deudores con Datos del BCRA"),
                                       p(HTML("Este proyecto, realizado en el marco de la Maestría en Estadística Aplicada, 
                                              se enfoca en la clasificación de deudores utilizando datos del Banco Central de la
                                              República Argentina (BCRA). A través de una muestra de CUITs de personas físicas con 
                                              deudas en el sistema financiero, se analizaron y compararon diferentes algoritmos de 
                                              clasificación para predecir casos de default. <br>
                                              El BCRA publica mensualmente un informe consolidado de deudas actuales e históricas, 
                                              lo que permite realizar un análisis detallado del comportamiento crediticio. 
                                              En este proyecto, se exploraron las características generales de los datos y se ajustaron
                                              modelos basados en algoritmos como CART, CHAID, Random Forest, XGBoost y LightGBM para identificar 
                                              el mejor clasificador. <br>
                                              En el <i>análisis descriptivo</i>, se identificaron patrones clave en los CUITs que presentaron default. 
                                              Por ejemplo, se observó que la proporción de deuda en situación 1 es un buen indicador de cumplimiento. 
                                              Además, se eliminaron variables con valores nulos significativos y se crearon variables auxiliares 
                                              para mejorar la precisión. <br>
                                              Se destacó el algoritmo <i>LightGBM</i> con un AUC de 0.78 siendo el mejor clasificador en el conjunto de
                                              validación. Otros modelos como XGBoost y regresión logística también mostraron buenos resultados, 
                                              aunque con ligeras diferencias en el desempeño. <br>
                                              Cabe destacar que debido a la naturaleza de los datos, el modelo no puede utilizarse para clasificar 
                                              futuros créditos, ya que se basa en comportamientos recientes de personas con historial financiero. <br>"), style = "text-align:justify;"),
                                       h4("Contacto"),
                                       p(HTML('<b>Correo:</b> gottig.nicolas@uader.edu.ar <br> <b>GitHub:</b> 
                                              <a href=https://github.com/NicoGottig>NicoGottig</a>'))),
                                column(1)))
             
             )
  )


# Servidor ####
server <- function(input, output, session){
  
  # Gráfico de puntos
  output$plotTitle <- renderUI({
    req(input$variable1, input$variable2, input$grupo)
    
    # Puedes personalizar el título aquí
    titulo <- paste("Distribución conjunta de", names(variable_choices)[which(variable_choices == input$variable1)], 
                    "y", names(variable_choices)[which(variable_choices == input$variable2)], 
                    "por", names(grupo_choices)[which(grupo_choices == input$grupo)])
    
    tags$p(titulo, style = "color: #3B5998; font-weight: bold;")
  })
  output$scatterPlot <- renderPlot({
    req(input$variable1, input$variable2, input$grupo)
    
    df_modificado <- df %>%
      mutate_at(vars(input$grupo), 
                ~ case_when(
                  . == 27 ~ "mujer",
                  . == 20 ~ "hombre",
                  . == 1 ~ "sí",
                  . == 0 ~ "no",
                  TRUE ~ as.character(.)
                ))
    
    ggplot(df_modificado, aes_string(x = input$variable2, y = input$variable1, color = input$grupo)) +
      geom_point(size = 3) +
      scale_color_manual(values = mate_pal) +
      labs(x = names(variable_choices)[which(variable_choices == input$variable2)],
           y = names(variable_choices)[which(variable_choices == input$variable1)],
           color = names(grupo_choices)[which(grupo_choices == input$grupo)]) 
      
  })
  
  # Tabla de resumen
  output$tableTitle <- renderUI({
    req(input$variable1, input$variable2, input$grupo)
    
    # Puedes personalizar el título aquí
    titulo <- paste("Resumen de", names(variable_choices)[which(variable_choices == input$variable1)], 
                    "y", names(variable_choices)[which(variable_choices == input$variable2)], 
                    "por", names(grupo_choices)[which(grupo_choices == input$grupo)])
    
    tags$p(titulo, style = "color: #3B5998; font-weight: bold;")
  })
  output$summaryTable <- renderTable({
    req(input$variable1, input$variable2, input$grupo)
    titulo = names(grupo_choices)[which(grupo_choices == input$grupo)]
    mediav1 = paste0("Media de ", names(variable_choices)[which(variable_choices == input$variable1)])
    mediav2 = paste0("Media de ", names(variable_choices)[which(variable_choices == input$variable2)])
    sdv1 = paste0("Desvio de ", names(variable_choices)[which(variable_choices == input$variable1)])
    sdv2 = paste0("Desvio de ", names(variable_choices)[which(variable_choices == input$variable2)])
    
    df_modificado <- df %>%
      mutate_at(vars(input$grupo), 
                ~ case_when(
                  . == 27 ~ "mujer",
                  . == 20 ~ "hombre",
                  . == 1 ~ "sí",
                  . == 0 ~ "no",
                  TRUE ~ as.character(.)
                ))
    
    table = df_modificado %>%
      group_by_at(input$grupo) %>%
      summarise(
        Grupo = first(.data[[input$grupo]]),
        `Media de la Variable 1` = mean(.data[[input$variable1]], na.rm = TRUE),
        `Desviación Estándar de la Variable 1` = sd(.data[[input$variable1]], na.rm = TRUE),
        `Media de la Variable 2` = mean(.data[[input$variable2]], na.rm = TRUE),
        `Desviación Estándar de la Variable 2` = sd(.data[[input$variable2]], na.rm = TRUE)
      ) %>%
      select(Grupo, everything())
    
    colnames(table)[1] = titulo
    colnames(table)[3] = mediav1
    colnames(table)[4] = sdv1
    colnames(table)[5] = mediav2
    colnames(table)[6] = sdv2
      
    table = table[,-2]
    table
  })
  
  # Modelo
  observeEvent(input$predict, {
    genero_27 <- if_else(input$genero == "mujer", 1, 0)
    genero_20 <- if_else(input$genero == "varón", 1, 0)
    default_historico <- if_else(input$default_historico == "sí", 1, 0)
    garantia_historica <- if_else(input$garantia_historica == "sí", 1, 0)
    
    resultado <- scorear(
      proxy_edad_actual = input$dni,
      prop_con_garantia_actual = input$prop_con_garantia_actual,
      n_meses_seg_bcra = input$n_meses_seg_bcra,            
      max_situacion_mes = input$max_situacion_mes,
      media_prop_situacion_1 = input$media_prop_sit_1,
      media_prop_situacion_2 = input$media_prop_sit_2,
      media_prop_default = input$media_prop_default,
      media_prop_con_garantia = input$media_prop_con_garantia,
      prop_tuvo_garantia = input$prop_tuvo_garantia,
      prop_mora_30_dias_seg = input$prop_mora_30_dias_seg,
      prop_default_seg = input$prop_default_seg,
      es_27 = genero_27,
      es_20 = genero_20,
      tiene_default_historico = default_historico,
      tiene_garantia_historica = garantia_historica
    )
    
    output$prediction <- renderPrint({ resultado$prob })
    
    output$prob_sit2 <- renderPlot({
      
      prob_plot1 <- scorear(
        proxy_edad_actual = input$dni,
        es_27 = genero_27,
        es_20 = genero_20,
        tiene_default_historico = default_historico,
        tiene_garantia_historica = garantia_historica,
        prop_con_garantia_actual = input$prop_con_garantia_actual,
        n_meses_seg_bcra = input$n_meses_seg_bcra,
        max_situacion_mes = input$max_situacion_mes,
        media_prop_situacion_1 = input$media_prop_sit_1,
        media_prop_situacion_2 = seq(0.01, 0.9, 0.01),
        media_prop_default = input$media_prop_default,
        media_prop_con_garantia = input$media_prop_con_garantia,
        prop_tuvo_garantia = input$prop_tuvo_garantia,
        prop_mora_30_dias_seg = input$prop_mora_30_dias_seg,
        prop_default_seg = input$prop_default_seg
      )
      
      pl1 = ggplot(prob_plot1) + 
        aes(x = media_prop_situacion_2, y = prob) +
        geom_line(col = mate_pal[1]) +
        theme_bw() +
        xlab("% medio en sit. 2") +
        ylab("Probabilidad de default") +
        scale_x_continuous(labels = scales::percent) + 
        labs(title = "Probabilidad de default", subtitle = "según % de deuda en situación 2")
      
      prob_plot2 <- scorear(
        proxy_edad_actual = input$dni,
        es_27 = genero_27,
        es_20 = genero_20,
        tiene_default_historico = default_historico,
        tiene_garantia_historica = garantia_historica,
        prop_con_garantia_actual = input$prop_con_garantia_actual,
        n_meses_seg_bcra = input$n_meses_seg_bcra,
        max_situacion_mes = input$max_situacion_mes,
        media_prop_situacion_1 = seq(0.01, 0.9, 0.01),
        media_prop_situacion_2 = input$media_prop_sit_2,
        media_prop_default = input$media_prop_default,
        media_prop_con_garantia = input$media_prop_con_garantia,
        prop_tuvo_garantia = input$prop_tuvo_garantia,
        prop_mora_30_dias_seg = input$prop_mora_30_dias_seg,
        prop_default_seg = input$prop_default_seg
      )
      
      pl2 = ggplot(prob_plot2) + 
        aes(x = media_prop_situacion_1, y = prob) +
        geom_line(col = mate_pal[2]) +
        theme_bw() +
        xlab("% medio en sit. 1") +
        ylab("Probabilidad de default") +
        scale_x_continuous(labels = scales::percent) +
        labs(title = "Probabilidad de default", subtitle = "según % de deuda en situación 1")
      
      
      grid.arrange(pl2, pl1, ncol = 1, nrow = 2)
    })
    
    output$plotEdad = renderPlot({
      
      prob_edad <- scorear(
        proxy_edad_actual = c(0:500),
        es_27 = genero_27,
        es_20 = genero_20,
        tiene_default_historico = default_historico,
        tiene_garantia_historica = garantia_historica,
        prop_con_garantia_actual = input$prop_con_garantia_actual,
        n_meses_seg_bcra = input$n_meses_seg_bcra,
        max_situacion_mes = input$max_situacion_mes,
        media_prop_situacion_1 = input$media_prop_sit_1,
        media_prop_situacion_2 = input$media_prop_sit_2,
        media_prop_default = input$media_prop_default,
        media_prop_con_garantia = input$media_prop_con_garantia,
        prop_tuvo_garantia = input$prop_tuvo_garantia,
        prop_mora_30_dias_seg = input$prop_mora_30_dias_seg,
        prop_default_seg = input$prop_default_seg
      )
      
      pl3 = ggplot(prob_edad) + 
        aes(x = proxy_edad_actual, y = prob) +
        geom_line(col = mate_pal[2]) +
        theme_bw() +
        xlab("% medio en sit. 1") +
        ylab("Probabilidad de default") +
        scale_x_continuous(labels = scales::percent) +
        labs(title = HTML("<b>Probabilidad de default según proxy de edad (3 núm. del DNI)</b"))
      
      
      pl3
      
    })
    
    
    output$default_prob_box <- renderValueBox({
      valueBox(
        subtitle = "(Probabilidad de default estimada)",
        value = HTML(paste0('<span style="color: orange;"><b>', round(resultado$prob, 2), '</b></span>')),
        color = "purple")
    })
    
    valor_reco = if_else(resultado$prob >= 0.25, "No otorgar préstamo", "Otorgar préstamo")
    
    output$reco_box <- renderValueBox({
      valueBox(
        subtitle = "Recomendación (Val. Crítico: 0.25)",
        value = HTML(paste0('<span style="color: orange;"> <b>', valor_reco, '</b></span>')),
        color = "purple")
    })
    
    output$resultado = renderTable({
      resultado
      
    })
    
    
  })
  
  # Pestaña de especificación del modelo
  
  output$random_forest = renderPlot({
    tune_res_df %>%
      select(mtry, trees, min_n, metric, mean) %>%
      mutate(metric = if_else(metric == "f_meas", "F1 Promedio", "AUC")) %>%
      pivot_longer(-c(metric, mean), names_to = "parametro", values_to = "valor") %>%
      ggplot() +
      aes(x = valor, y = mean, col = factor(if_else(metric == "AUC", round(mean, 2), NA_real_))) +
      scale_color_manual(values = mate_pal) +
      geom_point(size = 2) +
      facet_grid(metric ~ parametro, scales = "free") +
      xlab("Valor del parámetro") +
      ylab("Promedio de la métrica") +
      labs(title = "Fig. 1 - Análisis de modelo Random Forest") + 
      theme(legend.position = "none") 
  })
  
  
  output$lgbm_model = renderPlot({
      grid_lgbm %>%
      filter(feature_fraction == 0.5) %>%
      mutate(max_depth = factor(paste0("max_depth: ", max_depth), levels = c("max_depth: 10", "max_depth: 50","max_depth: 100"), ordered = T)) %>%
      ggplot() +
      aes(x = learning_rate, y = auc, alpha = factor(nrounds), col = factor(num_leaves), shape = factor(min_data_in_leaf)) +
      geom_line(lwd = .85) +
      geom_point(size = 1.2) +
      facet_wrap(~ max_depth) +
      scale_color_manual(values = mate_pal) +
      xlab("L. Rate") +
      ylab("AUC") +
      guides(color = guide_legend(title = "Num leaves: "),
             alpha = guide_legend(title = "Nrounds: "),
             shape = guide_legend(title = "Min data in leaf: ")) +
      labs(title = "Fig. 2 - Análisis del modelo Light GBM", subtitle = "Para distintos valores de hojas, árboles y mínima data por hoja") +
      theme(legend.position = "none") 

  })
  
  output$optimizacion_p = renderPlot({
      
    optimizacion_p_modelo %>%
      pivot_longer(-p, names_to = "metrica", values_to = "valor") %>%
      ggplot(aes(x = p, y = valor, col = metrica)) +
      geom_line(lwd = .95) +
      scale_color_manual(values = mate_pal) +
      facet_wrap(~metrica, scales = "free") +
      guides(color = guide_legend(title="Función: ")) +
      labs(title = "Fig. 3 - Optimización del criterio de clasificación", 
           subtitle = "Para función de costo-beneficio exógena")

  })
  
}

# Función para ejecutar la app
shinyApp(ui, server)
