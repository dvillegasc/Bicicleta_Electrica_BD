library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Carga de datos
datos <- read.csv2("C:/Users/davil/Desktop/Datos_Bici_Imputados_Motor.csv")


# Filtrar día 20220909
dia_analisis <- datos %>%
  filter(fecha == 20220909)


# 3. FILTROS DE ANOMALÍAS (Sanity Checks físicos con dplyr)
# Se crean banderas booleanas (TRUE/FALSE) si el dato rompe las leyes del hardware
anomalias <- dia_analisis %>%
  mutate(
    # Límite físico de la batería 36V (Vacía < 30V, Llena > 42.5V)
    falla_voltaje = if_else(VOLTAGE_A < 28 | VOLTAGE_A > 43, TRUE, FALSE),
    
    # Límite del controlador para motor de 350W (Picos máx ~18-20A)
    falla_corriente = if_else(CURRENT_A_CALC < 0 | CURRENT_A_CALC > 20, TRUE, FALSE),
    
    # Límite térmico del Litio-ion y clima de Medellín
    falla_temperatura = if_else(TEMPERATURE_A < 10 | TEMPERATURE_A > 65, TRUE, FALSE),
    
    # Falla dinámica: Es imposible tener un pico alto de corriente sin que el voltaje caiga (Voltage Sag)
    # Si la corriente pasa de 15A y el voltaje sigue por encima de 40V, el sensor miente.
    falla_dinamica = if_else(CURRENT_A_CALC > 15 & VOLTAGE_A > 40, TRUE, FALSE)
  ) %>%
  # Filtrar solo las filas que tengan al menos un error
  filter(falla_voltaje | falla_corriente | falla_temperatura | falla_dinamica)

# Imprimir el recuento de errores detectados
cat("Anomalías físicas detectadas el 20220909:", nrow(anomalias), "\n")
if(nrow(anomalias) > 0) {
  print(head(anomalias %>% select(new_time, VOLTAGE_A, CURRENT_A_CALC, TEMPERATURE_A)))
}

# 4. VISUALIZACIÓN EXPLORATORIA
# Pivotear los datos para alinear las gráficas temporalmente (comparación directa)
dia_grafico <- dia_analisis %>%
  select(new_time, VOLTAGE_A, CURRENT_A_CALC, TEMPERATURE_A) %>%
  pivot_longer(cols = -new_time, names_to = "Variable", values_to = "Valor")

# Graficar usando facetas
ggplot(dia_grafico, aes(x = new_time, y = Valor, color = Variable)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(
    title = "Análisis de Telemetría: Batería (Canal A) - Fecha: 20220909",
    subtitle = "Cruzar caídas de voltaje con picos de corriente",
    x = "Tiempo Estándar (new_time)",
    y = "Valor medido"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  )








