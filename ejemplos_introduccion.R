library(ggplot2)
library(gridExtra)
library(survival)
library(reshape2)

# ==============================================================================
# CARGA DE DATOS (Mismos contextos definidos previamente)
# ==============================================================================
fatiga_data <- c(70, 90, 96, 97, 99, 100, 103, 104, 104, 105, 107, 108, 108, 108, 109, 
                 109, 112, 112, 113, 114, 114, 114, 116, 119, 120, 120, 120, 121, 121, 
                 123, 124, 124, 124, 124, 124, 128, 128, 129, 129, 130, 130, 130, 131, 
                 131, 131, 131, 131, 132, 132, 132, 133, 134, 134, 134, 134, 134, 136, 
                 136, 137, 138, 138, 138, 139, 139, 141, 141, 142, 142, 142, 142, 142, 
                 142, 144, 144, 145, 146, 148, 148, 149, 151, 151, 152, 155, 156, 157, 
                 157, 157, 157, 158, 159, 162, 163, 163, 164, 166, 166, 168, 170, 174, 
                 196, 212)
pulmon_limpio <- na.omit(lung$time)
viento_limpio <- na.omit(airquality$Wind)
factura_limpia <- na.omit(tips$total_bill)

# ==============================================================================
# FUNCIÓN DE GRAFICACIÓN OPTIMIZADA PARA PÓSTER (Sin leyendas, fuentes grandes)
# ==============================================================================
graficar_datos <- function(datos, titulo, xlabel, color) {
  
  mu <- mean(datos, na.rm = TRUE)
  sigma <- sd(datos, na.rm = TRUE)
  
  ggplot(data.frame(x = datos), aes(x = x)) +
    
    # Sombra empírica
    geom_density(fill = color, alpha = 0.7, color = "#2c3e50", linewidth = 0.8, adjust = 1.2) +
    
    # Línea Normal gruesa y contrastante
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                  color = "#c0392b", linetype = "dashed", linewidth = 1.5) +
    
    # Sin subtítulos, solo lo esencial
    labs(title = titulo, x = xlabel, y = "") +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5), # Título más grande
      axis.title.x = element_text(size = 12, face = "bold"),            # Nombre del eje más grande
      axis.text.x = element_text(size = 10),                            # Números más legibles
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# GENERACIÓN DE LAS GRÁFICAS Y GUARDADO EN ALTA CALIDAD
# ==============================================================================
g1_datos <- graficar_datos(fatiga_data, "Ingeniería: Fatiga", "Ciclos de vibración", "#FF9999")
g2_datos <- graficar_datos(pulmon_limpio, "Medicina: Supervivencia", "Días de supervivencia", "#99CCFF")
g3_datos <- graficar_datos(viento_limpio, "Ambiente: Velocidad del Viento", "Millas por hora (mph)", "#99FF99")
g4_datos <- graficar_datos(factura_limpia, "Economía: Facturación de Consumo", "Total de la Factura ($)", "#FFCC99")

# Guardar directamente en PDF vectorial para que en LaTeX se vea perfecto
grafica_final <- arrangeGrob(g1_datos, g2_datos, g3_datos, g4_datos, ncol = 2)
ggsave("ejemplos_introduccion.pdf", plot = grafica_final, width = 10, height = 7, units = "in")
