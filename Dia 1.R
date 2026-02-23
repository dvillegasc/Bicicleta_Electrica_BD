library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Carga de datos
datos <- read.csv2("C:/Users/davil/Desktop/Datos_Bici_Imputados_Motor.csv")


# Filtrar día 20220909
dia_analisis <- datos %>%
  filter(fecha == 20220909)


# FILTROS DE ANOMALÍAS (Sanity Checks físicos con dplyr)
# booleano
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


