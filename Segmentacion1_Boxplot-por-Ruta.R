library(readr)
library(dplyr)
library(plotly)



datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

# ==============================================================================
# Boxplot por RUTA

# Cambiar comas por puntos solo en variables numéricas
cols_a_limpiar <- setdiff(names(datos_bici), c("fecha", "exp", "lugar", "duracion", "tiempo", "new_time"))
datos_bici <- datos_bici %>%
  mutate(across(all_of(cols_a_limpiar), ~ as.numeric(gsub(",", ".", as.character(.)))))

# Filtro de no anomalias (anomaly == 1)
datos_sanos <- datos_bici %>%
  filter(anomaly == 1)

# ==============================================================================
# Graficas

# Gráfica A: Esfuerzo (Corriente Total de la Batería)
box_corriente <- plot_ly(datos_sanos, x = ~lugar, y = ~CURRENT_A_CALC, type = "box",
                         color = ~lugar, colors = c("#2ca02c", "#ff7f0e", "#1f77b4")) %>%
  layout(title = "Corriente por Ruta (Sano)",
         yaxis = list(title = "Amperios Extraídos (CURRENT_A)"),
         xaxis = list(title = "Rutas"))
print(box_corriente)
# Nota: El laboratorio y el metroplus tienen un comportamiento muy parecido
# Mientras que en palmas la corriente se encierra en un intervalo de variacion muy pequeño
# Esto se puede explicar con que la capacidad de la bicicleta es una autonomia de 
# Maximo 25 km/h, asi que al subir una falda esta corriente por lo general aumenta
# a un valor de 2.12 aprox, manteniendose mas constante, a diferencia de las otras rutas
# En las cuales la corriente por lo general es menor y tiende a aumentar en ciertos casos
# especificos pero de forma momentanea al encontrarse con un bache, policia o una pequeña
# Inclinacion repentina.


# Gráfica B: Caída de Tensión (Voltage Sag de la Batería)
box_voltaje <- plot_ly(datos_sanos, x = ~lugar, y = ~VOLTAGE_A, type = "box",
                       color = ~lugar, colors = c("#2ca02c", "#ff7f0e", "#1f77b4")) %>%
  layout(title = "Voltaje por Ruta (Sano)",
         yaxis = list(title = "Voltios de la Batería (VOLTAGE_A)"),
         xaxis = list(title = "Rutas"))
print(box_voltaje)
#Nota: En el caso del voltaje en el lab y en metroplus son muy parecidas, en palmas tambien
# Se presenta este parecido, con la diferencia de que su mediana y cuantiles q1, q3 son
# Mas pequeños, esto se puede explicar con que por lo general como se esta en una pendiente
# La caida de tension es mas constante y por eso el voltaje promedio se mantiene más
# Bajo que en los demas casos, en los cuales no hay caidas de tension tan constantes como
# En palmas.

# Gráfica C: Estrés Térmico (Delta T del Motor)
box_temp <- plot_ly(datos_sanos, x = ~lugar, y = ~TEMPERATURE_MOTOR, type = "box",
                    color = ~lugar, colors = c("#2ca02c", "#ff7f0e", "#1f77b4")) %>%
  layout(title = "Calentamiento del Motor (Sano)",
         yaxis = list(title = "Incremento de Temp (°C sobre el ambiente)"),
         xaxis = list(title = "Rutas"))
print(box_temp)
# Nota: El laboratorio y metroplus tienen gran parecido, con la diferencia de que en metroplus
# Hay unos outliers mas alejados y un limite superior más grande. Tambien se resalta la 
# Ruta de palmas, esta no tiene outliers, tiene un rango de maximo 26° y su variacion es
# Más bien poquita, ¿esto como se explica?, pues hay que tener en cuenta que en la ruta
# De metroplus hay mas semaforos, hay baches, hay mas tiempo de duracion de la 
# bateria, más exposicion al sol y hay que frenar y arrancar mas veces  con alguien que pesa 
# 112 kg aprox, cosa que hace que la temperatura aumente más por estres 
# Termico, mientras que en palmas la pendiente es más constante, y como hay
# Mas esfuerzo mantenido la bateria se descarga mas rapido pero la temperatura 
# tiende a mantenerse mas constante.

# ==============================================================================
# Matriz de correlacion con datos sanos





