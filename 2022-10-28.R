library(readr)
library(dplyr)
library(plotly)
library(ggplot2)


datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

#LABORATORIO
# Filtro del dia 28 de octubre de 2022
datos_dia <- datos_bici %>% filter(fecha == 20221028)

# Saber que experimentos
unique(datos_dia$exp)

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
    layout(title = paste("La Serie de Tiempo del Día - Fecha: 20221028 | Exp:", experimento),
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
  
  matriz_sin_anomalias <- crear_matriz(df_sanos, paste("Matriz sin anomalias - Exp:", experimento, "- 28 Oct"))
  matriz_anomalias <- crear_matriz(df_anomalos, paste("Matriz con anomalias - Exp:", experimento, "- 28 Oct"))

  return(list(matriz_sin_anomalias = matriz_sin_anomalias, matriz_anomalias = matriz_anomalias))
  
}



# ==============================================================================
# EXPERIMENTO E01

# Series de tiempo
serie_E01 <- graficar_serie(datos_dia, "E01")
print(serie_E01)
# Respecto a este dia, en la bitacora no se reporto nada.

# En el tiempo 611 hubo una caida en la temperatura, en la corriente y una subida 
# en el voltaje, pareciendo como si hubo una desconexion del sensor mientras iba en una 
# pendiente simulada en el laboratorio, por dicha razon cayeron la temperatura y la corriente
# pero el voltaje subio porque se recupero de la caida de tension, volviendo a su valor en 
# reposo. A raiz de esto se reportaron algunas anomalias, las cuales pueden deberse a esta 
# falla mayor en el tiempo 611.

# Matrices de correlacion
matrices_E01 <- matriz_cor(datos_dia, "E01")
print(matrices_E01$matriz_sin_anomalias)
# La matriz presenta un comportamiento normal, tal como deberia ser

print(matrices_E01$matriz_anomalias)
# - Presenta fallas en la correlacion entre la temperatura y los voltajes, 
#  esta deberia ser grande y negativa 
# -Fallan las correlaciones entre todas las conexiones A,B,C,D
# -Falla en la correlacion entre las corrientes y los voltajes, deberia ser
# mayor a -0.55 

# ==============================================================================
# EXPERIMENTO E02 

# Series de tiempo
serie_E02 <- graficar_serie(datos_dia, "E02")
print(serie_E02)
# Respecto a este dia, en la bitacora no se reporto nada.

# En el tiempo 63 se presenta una anomalia, la temperatura cae derrepentemente a 0,
# La corriente que de por si tambien era pequeña tambien cae y el voltaje si sube,
# Es como si estuviera sometido a una caida de tension debido a alguna pendiente y luego 
# De superar una pendiente recuperara su valor en reposo. Siendo asi como en el caso 
# Anterior del E01, un porbable error en el sensor dio paso a anomalias causadas por este.
# Ademas de que no tiene sentido que la temperatura inicie en 16° aprox y que justo en un
# Segundo caiga de repente a 0.

# Matrices de correlacion
matrices_E02 <- matriz_cor(datos_dia, "E02")
print(matrices_E02$matriz_sin_anomalias)
print(matrices_E02$matriz_anomalias)
# En las dos matrices pasa exactamente lo mismo que en el escenario anterior.
# Solo conserva algunas pocas relaciones validas, como en el caso del voltaje
# Con la corriente


