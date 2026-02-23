library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Carga de datos
datos <- read.csv2("C:/Users/Joven Investigador/OneDrive - Universidad de Antioquia/Escritorio/Datos_Bici_Imputados_Motor.csv")


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
    falla_temperatura = if_else(TEMPERATURE_A < 10 | TEMPERATURE_A > 65, TRUE, FALSE),
    
    # Es imposible tener un pico alto de corriente sin que el voltaje caiga (Voltage Sag)
    # Si la corriente pasa de 15A y el voltaje sigue por encima de 40V, el sensor miente.
    falla_dinamica = if_else(CURRENT_A_CALC > 15 & VOLTAGE_A > 40, TRUE, FALSE)
  ) %>%
  # Filtrar solo las filas que tengan al menos un error
  filter(falla_voltaje | falla_corriente | falla_temperatura | falla_dinamica)

# Imprimir errores detectados
cat("Anomalías físicas detectadas el 20220909:", nrow(anomalias), "\n")
if(nrow(anomalias) > 0) {
  print(head(anomalias %>% select(new_time, VOLTAGE_A, CURRENT_A_CALC, TEMPERATURE_A)))
}

# Continuidad---------------------------------------------------------------------------------

datos_continuidad <- dia_analisis %>%
  mutate(
    # Verificación para 'tiempo'
    ok_tiempo = tiempo == lag(tiempo, default = first(tiempo) - 1) + 1,
    
    # Verificación para 'new_time'
    ok_new_time = new_time == lag(new_time, default = first(new_time) - 1) + 1,
    
    # Identificar filas donde hubo cualquier ruptura
    hay_salto = !ok_tiempo | !ok_new_time
  )

# Para ver solo las filas donde la secuencia se rompió:
errores <- datos_continuidad %>% 
  filter(hay_salto)


cat("Filas discontinuas:", nrow(errores), "\n")
cat("Lista de discontinuas")
errores

# Cambios abruptos sin sentido en la temperatura -----------------------------------------------

umbral_cambio <- 2
dia_analisis$ENV_TEMPERATURE <- as.numeric(dia_analisis$ENV_TEMPERATURE)

cambios_temp <- dia_analisis %>%
  mutate(
    # diferencia con la fila anterior
    dif_temp = abs(ENV_TEMPERATURE - lag(ENV_TEMPERATURE)),
    
    # es cambio si supera el umbral
    alerta_temp = if_else(dif_temp > umbral_cambio, "Anomalía", "Normal")) %>%
  
  # Reemplazamos el NA de la primera fila (que no tiene anterior)
  mutate(alerta_temp = if_else(is.na(alerta_temp), "Inicio", alerta_temp))

# Para visualizar solo los registros problemáticos
anomalias <- cambios_temp %>% 
  filter(alerta_temp == "Anomalía")


cat("Las anomalias son:")
anomalias


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
                 rangeslider = list(visible = TRUE)), # Agrega una barra de desplazamiento temporal
    yaxis = list(title = "Corriente A (Amperios)", 
                 range = c(-2, 400)) # AQUÍ ESTÁ LA MAGIA: Límites físicos del eje Y
  )

# Mostrar la gráfica
grafica_interactiva


