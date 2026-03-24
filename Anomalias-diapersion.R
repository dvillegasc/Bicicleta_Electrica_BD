library(readr)
library(dplyr)
library(plotly)

datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

# ==============================================================================
# Busqueda de variables mas criticas (Anomalias)

# Filtro de solo las fallas (anomaly == -1)
datos_anomalos <- datos_bici %>%
  filter(anomaly == -1)

cat("Total de datos anómalos:", nrow(datos_anomalos))

# ==============================================================================
# Graficos intercativos

# Función para crear los scatter plots con súper-detalle en el hover
crear_scatter_anomalia <- function(data, variable_y, titulo_y) {
  plot_ly(data = data, 
          x = ~anomaly_score, 
          y = ~get(variable_y), 
          type = 'scatter', 
          mode = 'markers',
          marker = list(color = 'darkred', opacity = 0.5, size = 6),
          text = ~paste("Fecha:", fecha,
                        "<br>Exp:", exp,
                        "<br>Ruta:", lugar,
                        "<br>New Time:", new_time,
                        "<br>Score:", round(anomaly_score, 4),
                        "<br>", variable_y, ":", round(get(variable_y), 2)),
          hoverinfo = 'text') %>%
    layout(title = paste(variable_y, "vs Anomaly Score"),
           xaxis = list(title = "Anomaly Score"),
           yaxis = list(title = titulo_y))
}

# Gráfica 1: Temperatura vs Score
scatter_temp <- crear_scatter_anomalia(datos_anomalos, "TEMPERATURE_MOTOR", "Temperatura Motor (°C)")
print(scatter_temp)
# Temperatura: Mientras mas negativo es el score mas temperaturas
# cercanas a 0 se presentan, esto solo es normal cuando recien se 
# Arranca la bicicleta. Cuando el score es negativo pero no tanto,
# Se suelen presentar temperaturas extrañamente altas.


# Gráfica 2: Voltaje Batería vs Score
scatter_voltaje <- crear_scatter_anomalia(datos_anomalos, "VOLTAGE_A", "Voltaje Batería (V)")
print(scatter_voltaje)
# Voltaje: Cuando el score es mas negativo se presenta un voltaje mas alto,
# Algo que es normal cuando la bicicleta recien empieza el experimento, esta bien cargada
# Y no esta en condiciones de caida de tension, osea subiendo alguna pendiente, sin embargo,
# Es raro que haya bastante voltaje luego de haber recorrido gran parte 
# Del trayecto, se puede pensar que es como si la corriente fallara y por eso mismo
# el voltaje volviera a su estado de reposo Otro comportamiento raro es un voltaje de 30, 
# esto es imposible ya que es el umbral maximo de descarga, y este no llega hasta ese punto.

# Gráfica 3: Corriente Batería vs Score
scatter_corriente <- crear_scatter_anomalia(datos_anomalos, "CURRENT_A_CALC", "Corriente Batería (A)")
print(scatter_corriente)
# Corriente: Para los scores más negativos, 
# la corriente cae bruscamente a 0. Aunque un valor de 0 es normal cuando la 
# bicicleta va a velocidad constante por inercia, en este contexto de anomalia 
# puede representar un corte repentino de energia o una falla del 
# sensor asi como un apagon del sistema.

# ==============================================================================
# Matriz de correlacion con datos anomalos







