library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)


datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")


datos_100 <- datos_bici %>%
  arrange(fecha, exp, new_time) %>%
  group_by(fecha, exp) %>%
  slice_head(n = 100) %>%
  mutate(
    punto_x = row_number(),
    grupo_id = paste(fecha, exp, sep = "_"),
    fecha_date = as.Date(as.character(fecha), format = "%Y%m%d"),
    etiqueta_hover = paste("Fecha:", fecha_date, 
                           "<br>Exp:", exp, 
                           "<br>Punto X:", punto_x, 
                           "<br>Temp:", TEMPERATURE_MOTOR, "°C")
  ) %>%
  ungroup()


grafico_base <- ggplot(datos_100, aes(x = punto_x, y = TEMPERATURE_MOTOR, 
                                      group = grupo_id, 
                                      color = as.numeric(fecha_date),
                                      text = etiqueta_hover)) + 
  geom_point(alpha = 0.5, size = 1.5) +
  geom_line(alpha = 0.3) +
  scale_color_viridis_c(
    option = "plasma", # "plasma" o "magma" o "viridis"
    name = "Fecha",
    labels = function(x) as.Date(x, origin = "1970-01-01")
  ) +
  labs(
    title = "Evolución de la Temperatura",
    x = "Puntos 1 a 100",
    y = "Temperatura (°C)"
  ) +
  theme_minimal() +
  theme(legend.position = "right") 


grafico_interactivo <- ggplotly(grafico_base, tooltip = "text")


grafico_interactivo
