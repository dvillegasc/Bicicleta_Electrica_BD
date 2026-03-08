library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Carga de datos
#datos_fallas <- read.csv2("C:/Users/Joven Investigador/OneDrive - Universidad de Antioquia/Escritorio/data_bicicleta_con_fallas.csv)
datos_fallas <- read.csv("C:/Users/davil/Desktop/data_bicicleta_con_fallas.csv", sep = ";")


datos_fallas <- datos_fallas %>%
  mutate(fila_original = row_number())

#Filtro anomalos
datos_anomalos <- datos_fallas %>%
  filter(anomaly == -1)


# ==============================================================================
# Matriz de correlacion - MOTOR


# Seleccionamos variables del motor y el score
motor_cols <- c("CURRENT_B_CALC", "CURRENT_C_CALC", "CURRENT_D_CALC", 
                "POWER_B", "POWER_C", "POWER_D",
                "VOLTAGE_B", "VOLTAGE_C", "VOLTAGE_D", 
                "TEMPERATURE_MOTOR", "anomaly_score")

matriz_motor <- cor(datos_anomalos %>% select(all_of(motor_cols)), use = "complete.obs")
df_corr_motor <- as.data.frame(as.table(matriz_motor))

plot_motor <- ggplot(df_corr_motor, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matriz Motor - Anomalias", x = "", y = "", fill = "Corr")

print(plot_motor)


# ==============================================================================
# Matriz de correlacion - GENERAL

datos_general <- datos_anomalos %>% 
  select(-anomaly, -fila_original) 

matriz_general <- cor(datos_general, use = "complete.obs")
df_corr_general <- as.data.frame(as.table(matriz_general))

plot_general <- ggplot(df_corr_general, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 2.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Matriz Global - Anomalías", x = "", y = "", fill = "Corr")

print(plot_general)


# ==============================================================================
# Graficas de dispersion

# Se toman las que tengan mayor correlacion con el anomaly_score
corr_anomalias <- abs(matriz_general["anomaly_score", ])
corr_anomalias <- sort(corr_anomalias, decreasing = TRUE)

# Se ignora la cor con si mismo
variables_top <- names(corr_anomalias)[2:5]

cat("Las variables que más rigen la falla internamente son:\n")
print(variables_top)

# Función estandarizada para las graficas
grafica_dispersion <- function(data, variable_y) {
  plot_ly(data = data, 
          x = ~anomaly_score, 
          y = ~get(variable_y), 
          type = 'scatter', 
          mode = 'markers',
          marker = list(color = 'darkred', opacity = 0.6, size = 6),
          text = ~paste("Fila Original:", fila_original,
                        "<br>Anomaly Score:", round(anomaly_score, 4),
                        "<br>", variable_y, ":", round(get(variable_y), 2)),
          hoverinfo = 'text') %>%
    layout(title = paste("Comportamiento:", variable_y, "vs Anomaly Score"),
           xaxis = list(title = "Anomaly Score (Extremos anómalos)"),
           yaxis = list(title = variable_y))
}

# Aplicar la funcion con las var mas criticas
grafica_dispersion_1 <- grafica_dispersion(datos_anomalos, variables_top[1])
print(grafica_dispersion_1)

grafica_dispersion_2 <- grafica_dispersion(datos_anomalos, variables_top[2])
print(grafica_dispersion_2)

grafica_dispersion_3 <- grafica_dispersion(datos_anomalos, variables_top[3])
print(grafica_dispersion_3)

grafica_dispersion_4 <- grafica_dispersion(datos_anomalos, variables_top[4])
print(grafica_dispersion_4)

