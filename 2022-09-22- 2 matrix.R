library(readr)
library(dplyr)
library(plotly)
library(ggplot2)


datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

# METROPLUS

# Filtro del dia 22 de septiembre de 2022
datos_dia <- datos_bici %>% filter(fecha == 20220922)

# Saber ruta
datos_dia$lugar[1]

# Saber que experimentos
unique(datos_dia$exp)

# Contar anomalias
numfallas <- datos_dia %>% 
  filter(anomaly == -1)
nrow(numfallas)

# ==============================================================================
# Funcion para Serie de Tiempo CURRENT

graficar_serie <- function(datos, experimento) {
  
  # Filtrar el experimento  y ordenar por tiempo
  df_exp <- datos %>% 
    filter(exp == experimento) %>% 
    arrange(new_time)
  
  df_fallas <- df_exp %>% 
    filter(anomaly == -1)
  
  # Grafica 1: Temperatura
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_B_CALC, name = "Temp Motor", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_B_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temp (°C)"))
  
  # Grafica 2: Corriente
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_C_CALC, name = "Corriente A", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_C_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (A)"))
  
  # Grafica 3: Voltaje
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_D_CALC, name = "Voltaje A", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_D_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (V)"))
  
  # Unir las tres gráficas en un solo panel alineado por el tiempo
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20220922 | Exp:", experimento),
           xaxis = list(title = "Linea de Tiempo (new_time)"),
           hovermode = "x unified")
}


# ==============================================================================
# Funcion para Serie de Tiempo VOLTAGE

graficar_serie2 <- function(datos, experimento) {
  
  # Filtrar el experimento  y ordenar por tiempo
  df_exp <- datos %>% 
    filter(exp == experimento) %>% 
    arrange(new_time)
  
  df_fallas <- df_exp %>% 
    filter(anomaly == -1)
  
  # Grafica 1: Temperatura
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_B, name = "Temp Motor", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_B,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temp (°C)"))
  
  # Grafica 2: Corriente
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_C, name = "Corriente A", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_C,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (A)"))
  
  # Grafica 3: Voltaje
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_D, name = "Voltaje A", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_D,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalia",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (V)"))
  
  # Unir las tres gráficas en un solo panel alineado por el tiempo
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20220922 | Exp:", experimento),
           xaxis = list(title = "Linea de Tiempo (new_time)"),
           hovermode = "x unified")
}




# ==============================================================================
# EXPERIMENTO E01

# Series de tiempo
serie_E01 <- graficar_serie(datos_dia, "E01")
print(serie_E01)

serie_E02 <- graficar_serie2(datos_dia, "E01")
print(serie_E02)






