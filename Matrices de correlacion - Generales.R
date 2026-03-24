library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")


vars_criticas <- c("CURRENT_A_CALC", "CURRENT_B_CALC", "CURRENT_C_CALC", "CURRENT_D_CALC",
                   "VOLTAGE_A", "VOLTAGE_B", "VOLTAGE_C", "VOLTAGE_D", 
                   "TEMPERATURE_MOTOR", "anomaly_score")
# Decidi hacerla sin power, ya que esta es derivada de current y de voltage
# Asi que antes de llegar a algo raro en power, eso se vera en las otras variables

# Sanos y anomalos
datos_sanos <- datos_bici %>% filter(anomaly == 1) %>% 
  select(all_of(vars_criticas))

datos_anomalos <- datos_bici %>% filter(anomaly == -1) %>% 
  select(all_of(vars_criticas))

# ==============================================================================
# Matrices de correlacion

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


matriz_sin_anomalias <- crear_matriz(datos_sanos, "A. Matriz de Correlación: sin anomalia (anomaly = 1)")
print(matriz_sin_anomalias)
# Aqui se aprecia la fisica real de las correlaciones entre las diferentes variables.

# Nota: La correlacion entre la temperatura del motor y la corriente A dice ser de 0, sin 
# embargo esto no es del todo cierto, pues la corriente A si afecta a la temperatura, 
# pero la corriente a es una variable que fluctua mucho y cambia rapido, mientras que
# la temperatura no, esta cambia lentamente.

matriz_anomalias <- crear_matriz(datos_anomalos, "B. Matriz de Correlación: con anomalia (anomaly = -1)")
print(matriz_anomalias)
# Respecto al comportamiento real, mostrado en la matriz sin anomalias, se presentan problemas:
# La correlacion de la corriente A vs las corrientes B,C y D son muy pequeñas y/o nulas,
# Se supone que si el motor necesita corriente la bateria le entrega a las 3 fases lo mismo.

# La correlacion entre las corrientes B,C,D, es muy baja para lo que deberia ser.

# La relacion entre la corriente y el voltaje es casi nula, eso es falso, la verdadera
# correlacion es negativa y aprox de -40, esto se explica con las caidas de tension.

# Las correlaciones negativas entre la temperatura del motor y los voltajes son muy altas 
# Para lo que deberian ser, esta correlacion deberia ser de -0.64 aprox.

# La temperatura del motor vs las corrientes B,C,D deberian ser positivas de aprox 0.20,
# En su lugar vemos que son negatricas y de -0.18. Ademas la correlacion entre el motor 
# Y la corriente A es negativa y de 0.30, cuando esta deberia ser nula.

# Los valores del anomaly score vs todas las variables se invierte en la matriz con 
# Anomalias, volviendose incoherente.







