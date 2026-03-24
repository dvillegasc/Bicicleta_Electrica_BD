library(readr)
library(dplyr)
library(plotly)
library(ggplot2)


datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

# Metroplus - Laboratorio
# Filtro del dia 21 de septiembre de 2022
datos_dia <- datos_bici %>% filter(fecha == 20220921)

# Saber que experimentos
unique(datos_dia$exp)

# Contar anomalias
numfallas <- datos_dia %>% 
  filter(anomaly == -1)
nrow(numfallas)

# ==============================================================================
# Funcion para Serie de Tiempo 

graficar_serie <- function(datos, experimento) {
  
  # Filtrar el experimento  y ordenar por tiempo
  df_exp <- datos %>% 
    filter(exp == experimento) %>% 
    arrange(new_time)
  
  df_fallas <- df_exp %>% 
    filter(anomaly == -1)
  
  # Grafica 1: Temperatura
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_MOTOR, name = "Temp Motor", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_MOTOR,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temp (°C)"))
  
  # Grafica 2: Corriente
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_A_CALC, name = "Corriente A", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_A_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (A)"))
  
  # Grafica 3: Voltaje
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_A, name = "Voltaje A", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_A,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (V)"))
  
  # Unir las tres gráficas en un solo panel alineado por el tiempo
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20220921 | Exp:", experimento),
           xaxis = list(title = "Linea de Tiempo (new_time)"),
           hovermode = "x unified")
}


# ==============================================================================
# Funcion para Matrices de Correlacion

# Variables criticas
vars_criticas <- c("CURRENT_A_CALC", "CURRENT_B_CALC", "CURRENT_C_CALC", "CURRENT_D_CALC",
                   "VOLTAGE_A", "VOLTAGE_B", "VOLTAGE_C", "VOLTAGE_D", 
                   "TEMPERATURE_MOTOR", "anomaly_score")


crear_matriz <- function(datos, titulo) {
  matriz_cor <- cor(datos, use = "complete.obs")
  df_cor <- as.data.frame(as.table(matriz_cor))
  
  # Graficar
  ggplot(df_cor, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    geom_text(aes(label = round(Freq, 2)), color = "black", size = 3.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(face = "bold", size = 14)) +
    labs(title = titulo, x = "", y = "", fill = "Corr")
}

matriz_cor <- function(datos_dia, experimento) {
  
  df_exp <- datos_dia %>% 
    filter(exp == experimento)
  
  df_sanos <- df_exp %>% 
    filter(anomaly == 1) %>% 
    select(all_of(vars_criticas))
  
  df_anomalos <- df_exp %>%
    filter(anomaly == -1) %>% 
    select(all_of(vars_criticas))
  
  matriz_sin_anomalias <- crear_matriz(df_sanos, paste("Matriz sin anomalias - Exp:", experimento, "- 21 sep"))
  matriz_anomalias <- crear_matriz(df_anomalos, paste("Matriz con anomalias - Exp:", experimento, "- 21 sep"))
  
  return(list(matriz_sin_anomalias = matriz_sin_anomalias, matriz_anomalias = matriz_anomalias))
  
}

matriz_cor1 <- function(datos_dia, experimento) {
  
  df_exp <- datos_dia %>% 
    filter(exp == experimento)
  
  df_sanos <- df_exp %>% 
    filter(anomaly == 1) %>% 
    select(all_of(vars_criticas))
  
  
  matriz_sin_anomalias <- crear_matriz(df_sanos, paste("Matriz sin anomalias - Exp:", experimento, "- 21 sep"))

  return(list(matriz_sin_anomalias = matriz_sin_anomalias))
  
}



# ==============================================================================
# EXPERIMENTO E01 - Metroplus

# Series de tiempo
serie_E01 <- graficar_serie(datos_dia, "E01")
print(serie_E01)
# Respecto a este experimento, en la bitacora reporto:

# - Los datos en cayenne aparecen sólo A las 10:38 am se desconecta para revisar 
#   que los sensores estén tomando datos y a las 11:02 am se vuelve a conectar


#-------------Analisis-----------

# En el tiempo 154 hubo una caida en la corriente y una subida 
# en el voltaje, pareciendo como si hubiera dejado de tener que hacer tanto esfuerzo mantenido
# hasta el tiempo 276, en el que vuelve a subir.Se puede pensar que hubo una bajada 
# En funcion de la inercia mas que todo, y que por eso la cirriente bajo y el voltaje subio

# No se presentaron anomalias.

# Matrices de correlacion
matrices_E01 <- matriz_cor1(datos_dia, "E01")
print(matrices_E01$matriz_sin_anomalias)
# Se observa:

# -La correlacion de current con los voltajes A,B,C,D esta mal, es practicamente nula,
# -Las correlaciones entre los current es muy baja.

# De resto esta bien.


# ==============================================================================
# EXPERIMENTO E02 - Laboratorio

# Series de tiempo
serie_E02 <- graficar_serie(datos_dia, "E02")
print(serie_E02)
# Respecto a este experimento, en la bitacora reporto:

# - La prueba se hace en el laboratorio por las condiciones, sin peso. A las 5:34 pm 
#   se desestabiliza. A las 5:38 pm se sube Juan Camilo a la bici

#-------------Analisis-----------

# Hubo una cantidad abismal de anomalias (63) y comportamientos extraños.
# En 329 cayo la temperatura hasta 0, la corriente hasta 0.2 y el voltaje subio
# rapidamente. Luego de eso hubo un comportamiento poco oscilante hasta 405, en donde 
# volvio a volverse mas oscilante.
# En 1639 hubo otra caida casi hasta 0, con las mismas repercusiones que en la primer caida


# Matrices de correlacion
matrices_E02 <- matriz_cor(datos_dia, "E02")
print(matrices_E02$matriz_sin_anomalias)
# Se observa:

# Las correlaciones muestran estar bien, lo unico a mencionar es que las correlaciones
# de current A con los demas current y voltaje son mas bajas de lo normal.

print(matrices_E02$matriz_anomalias)
# Las anomalias muestran que las correlaciones de current_A con los demas current 
# estan mal, son negativas.
# las correlaciones de los current son mas bajas de lo normal, lo mismo para los voltajes
# Hay algunas correlaciones entre corrent y voltage que son muy bajas, y no esta bien.


