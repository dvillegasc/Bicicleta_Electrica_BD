library(readr)
library(dplyr)
library(plotly)
library(ggplot2)

datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

# METROPLUS

# Filtro del dia 16 de octubre de 2022
datos_dia <- datos_bici %>% filter(fecha == 20221016)

# Para saber la ruta
unique(datos_dia$lugar)

# Para saber que experimentos
unique(datos_dia$exp)

# Para contar anomalias globales del día
numfallas <- datos_dia %>% 
  filter(anomaly == -1)
nrow(numfallas)

# ==============================================================================
# Funciones para las Series de Tiempo 

# Serie de tiempo - Variables A-----------------------------------
graficar_serie1 <- function(datos, experimento) {
  
  df_exp <- datos %>% 
    filter(exp == experimento) %>% 
    arrange(new_time)
  
  df_fallas <- df_exp %>% 
    filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_MOTOR, name = "Temp Motor", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_MOTOR,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temp (°C)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_A_CALC, name = "Corriente A", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_A_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (A)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_A, name = "Voltaje A", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_A,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (A)"))
  
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20221016 | Exp:", experimento),
           xaxis = list(title = "Linea de Tiempo (new_time)"),
           hovermode = "x unified")
}

# Series de Tiempo CURRENT B,C,D-----------------------------
graficar_serie2 <- function(datos, experimento) {
  
  df_exp <- datos %>% 
    filter(exp == experimento) %>% 
    arrange(new_time)
  
  df_fallas <- df_exp %>% 
    filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_B_CALC, name = "Corriente B", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_B_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (B)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_C_CALC, name = "Corriente C", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_C_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (C)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_D_CALC, name = "Corriente D", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_D_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (D)"))
  
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20221016 | Exp:", experimento),
           xaxis = list(title = "Linea de Tiempo (new_time)"),
           hovermode = "x unified")
}

# Series de Tiempo VOLTAGE B,C,D --------------------
graficar_serie3 <- function(datos, experimento) {
  
  df_exp <- datos %>% 
    filter(exp == experimento) %>% 
    arrange(new_time)
  
  df_fallas <- df_exp %>% 
    filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_B, name = "Voltage B", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_B,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (B)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_C, name = "Voltage C", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_C,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (C)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_D, name = "Voltage D", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_D,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (D)"))
  
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20221016 | Exp:", experimento),
           xaxis = list(title = "Linea de Tiempo (new_time)"),
           hovermode = "x unified")
}

# ==============================================================================
# Funcion para Matrices de Correlacion

vars_criticas <- c("CURRENT_A_CALC", "CURRENT_B_CALC", "CURRENT_C_CALC", "CURRENT_D_CALC",
                   "VOLTAGE_A", "VOLTAGE_B", "VOLTAGE_C", "VOLTAGE_D", 
                   "TEMPERATURE_MOTOR", "anomaly_score")

crear_matriz <- function(datos, titulo) {
  matriz_cor <- cor(datos, use = "complete.obs")
  df_cor <- as.data.frame(as.table(matriz_cor))
  
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
  
  df_exp <- datos_dia %>% filter(exp == experimento)
  df_sanos <- df_exp %>% filter(anomaly == 1) %>% select(all_of(vars_criticas))
  df_anomalos <- df_exp %>% filter(anomaly == -1) %>% select(all_of(vars_criticas))
  
  matriz_sin_anomalias <- crear_matriz(df_sanos, paste("Matriz sin anomalias - Exp:", experimento, "- 16 oct"))
  
  if(nrow(df_anomalos) > 0) {
    matriz_anomalias <- crear_matriz(df_anomalos, paste("Matriz con anomalias - Exp:", experimento, "- 16 oct"))
    return(list(matriz_sin_anomalias = matriz_sin_anomalias, matriz_anomalias = matriz_anomalias))
  } else {
    return(list(matriz_sin_anomalias = matriz_sin_anomalias))
  }
}

# ==============================================================================
# EXPERIMENTO E01 

serie1_E01 <- graficar_serie1(datos_dia, "E01")
serie2_E01 <- graficar_serie2(datos_dia, "E01")
serie3_E01 <- graficar_serie3(datos_dia, "E01")

print(serie1_E01)
print(serie2_E01)
print(serie3_E01)

matrices_E01 <- matriz_cor(datos_dia, "E01")
print(matrices_E01$matriz_sin_anomalias)
print(matrices_E01$matriz_anomalias)

# ==============================================================================
# EXPERIMENTO E02 

serie1_E02 <- graficar_serie1(datos_dia, "E02")
serie2_E02 <- graficar_serie2(datos_dia, "E02")
serie3_E02 <- graficar_serie3(datos_dia, "E02")

print(serie1_E02)
print(serie2_E02)
print(serie3_E02)

matrices_E02 <- matriz_cor(datos_dia, "E02")
print(matrices_E02$matriz_sin_anomalias)
print(matrices_E02$matriz_anomalias)
