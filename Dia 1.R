library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Carga de datos
#datos <- read.csv2("C:/Users/Joven Investigador/OneDrive - Universidad de Antioquia/Escritorio/Datos_Bici_Imputados_Motor.csv")
datos <- read.csv2("C:/Users/davil/Desktop/Datos_Bici_Imputados_Motor.csv")


# Filtrar día 20220909
dia_analisis <- datos %>%
  filter(fecha == 20220909)


# Filtros


# Anomalias--------------------------------------------------------------------------------------

anomalias <- dia_analisis %>%
  mutate(
    # Batería 36V (Vacía < 30V, Llena > 42.5V)
    falla_voltaje = if_else(VOLTAGE_A < 28 | VOLTAGE_A > 43, TRUE, FALSE),
    
    # Motor de 350W (Picos máx ~18-20A)
    falla_corriente = if_else(CURRENT_A_CALC < 0 | CURRENT_A_CALC > 20, TRUE, FALSE),
    
    # Límite térmico del Litio-ion y clima de Medellín
    falla_temperatura = if_else(TEMPERATURE_A < (ENV_TEMPERATURE - 2) | (TEMPERATURE_A - ENV_TEMPERATURE) > 35, TRUE, FALSE),
    
    # Es imposible tener un pico alto de corriente sin que el voltaje caiga (Voltage Sag)
    # Si la corriente pasa de 15A y el voltaje sigue por encima de 40V, el sensor miente.
    falla_dinamica = if_else(CURRENT_A_CALC > 15 & VOLTAGE_A > 40, TRUE, FALSE)
  ) %>%
  
  filter(falla_voltaje | falla_corriente | falla_temperatura | falla_dinamica)


cat("Anomalias detectadas el 2022-09-09:", nrow(anomalias), "\n")
if(nrow(anomalias) > 0) {
  print(head(anomalias %>% select(new_time, VOLTAGE_A, CURRENT_A_CALC, TEMPERATURE_A)))
}

# Continuidad---------------------------------------------------------------------------------

datos_continuidad <- dia_analisis %>%
  mutate(
    # Verificacion para tiempo
    ok_tiempo = tiempo == lag(tiempo, default = first(tiempo) - 1) + 1,
    
    # Verificacion para new_time
    ok_new_time = new_time == lag(new_time, default = first(new_time) - 1) + 1,
    
    # Identificar filas donde hubo cualquier ruptura
    hay_salto = !ok_tiempo | !ok_new_time
  )

# Para ver solo las filas donde la secuencia se rompio:
errores <- datos_continuidad %>% 
  filter(hay_salto)


cat("Filas discontinuas:", nrow(errores), "\n")
cat("Lista de discontinuas")
errores

if(nrow(errores) > 0) {
  print(head(errores %>% select(new_time, Tiempo)))
}

# Cambios abruptos sin sentido en la temperatura -----------------------------------------------

umbral_cambio <- 2
dia_analisis$ENV_TEMPERATURE <- as.numeric(dia_analisis$ENV_TEMPERATURE)

cambios_temp <- dia_analisis %>%
  mutate(
    # Diferencia con la fila anterior
    dif_temp = abs(ENV_TEMPERATURE - lag(ENV_TEMPERATURE)),
    
    # Es cambio si supera el umbral
    alerta_temp = if_else(dif_temp > umbral_cambio, "Anomalía", "Normal")) %>%
  
  # Para que la primera si tenga anterior
  mutate(alerta_temp = if_else(is.na(alerta_temp), "Inicio", alerta_temp))

# Para visualizar solo los problemas
anomalias <- cambios_temp %>% 
  filter(alerta_temp == "Anomalia")


cat("Las anomalias son:")

if(nrow(anomalias) > 0) {
  print(head(errores %>% select(ENV_TEMPERATURE)))
}


#----------------------------------- Graficas--------------------------------------

grafica_interactiva <- plot_ly(data = dia_analisis, 
                               x = ~new_time, 
                               y = ~CURRENT_A_CALC, 
                               type = 'scatter', 
                               mode = 'lines',
                               line = list(color = 'red', width = 1.5),
                               name = 'Corriente A') %>%
  layout(
    title = "Serie de Tiempo Interactiva: Corriente de Batería (Día 20220909)",
    xaxis = list(title = "Tiempo (new_time)", 
                 rangeslider = list(visible = TRUE)), # Barra de desplazamiento temporal
    yaxis = list(title = "Corriente A (Amperios)", 
                 range = c(-2, 400)) 
  )

grafica_interactiva


# -------------------------------------------------------------------------

datos_correlacion <- dia_analisis %>%
  transmute(
    VOLTAGE_A       = as.numeric(VOLTAGE_A), 
    CURRENT_A_CALC  = as.numeric(CURRENT_A_CALC), 
    POWER_A         = as.numeric(POWER_A), 
    TEMPERATURE_A   = as.numeric(TEMPERATURE_A),
    
    VOLTAGE_B       = as.numeric(VOLTAGE_B), 
    CURRENT_B_CALC  = as.numeric(CURRENT_B_CALC), 
    TEMPERATURE_B   = as.numeric(TEMPERATURE_B),
    
    ENV_TEMPERATURE = as.numeric(ENV_TEMPERATURE), 
    ENV_HUMIDITY    = as.numeric(ENV_HUMIDITY), 
    ACCELERATION_Z  = as.numeric(ACCELERATION_Z)
  )

# Matriz de correlación de Pearson 
matriz_corr <- cor(datos_correlacion, use = "complete.obs")

# Transfortmacion para poder usar ggplot
df_corr <- as.data.frame(as.table(matriz_corr))
colnames(df_corr) <- c("Variable1", "Variable2", "Correlacion")

# Grafica - Con mapa de calor
grafica_correlacion <- ggplot(df_corr, aes(x = Variable1, y = Variable2, fill = Correlacion)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1), space = "Lab", 
    name = "Correlación\n(-1 a 1)"
  ) +
  geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 3) + 
  theme_minimal() +
  labs(
    title = "Matriz de Correlación",
    subtitle = "Relación lineal entre las variables"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    panel.grid.major = element_blank() 
  ) +
  coord_fixed() 

print(grafica_correlacion)

