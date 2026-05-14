library(shiny)
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)

# Ruta
datos_bici <- read.csv("C:/Users/davil/Desktop/Dataset_BicicletasElectricas.csv", sep = ";")

# ==============================================================================
#          Series de tiempo

graficar_serie1 <- function(df_exp, experimento, fecha_str) {
  df_fallas <- df_exp %>% filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_MOTOR, name = "Temperatura motor", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_MOTOR,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temperatura (°C)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_A_CALC, name = "Corriente A", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_A_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (A)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_A, name = "Voltaje A", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_A,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (V)"))
  
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("Comportamiento de variables A - Fecha:", fecha_str, "| Experimento:", experimento),
           xaxis = list(title = "Línea de tiempo (new_time)"),
           hovermode = "x unified")
}

graficar_serie2 <- function(df_exp, experimento, fecha_str) {
  df_fallas <- df_exp %>% filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_B_CALC, name = "Corriente B", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_B_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (B)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_C_CALC, name = "Corriente C", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_C_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (C)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~CURRENT_D_CALC, name = "Corriente D", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~CURRENT_D_CALC,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Corriente (D)"))
  
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("Corrientes B, C y D - Fecha:", fecha_str, "| Experimento:", experimento),
           xaxis = list(title = "Línea de tiempo (new_time)"),
           hovermode = "x unified")
}

graficar_serie3 <- function(df_exp, experimento, fecha_str) {
  df_fallas <- df_exp %>% filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_B, name = "Voltaje B", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_B,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (B)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_C, name = "Voltaje C", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_C,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (C)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~VOLTAGE_D, name = "Voltaje D", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~VOLTAGE_D,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Voltaje (D)"))
  
  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("Voltajes B, C y D - Fecha:", fecha_str, "| Experimento:", experimento),
           xaxis = list(title = "Línea de tiempo (new_time)"),
           hovermode = "x unified")
}

graficar_serie4 <- function(df_exp, experimento, fecha_str) {
  df_fallas <- df_exp %>% filter(anomaly == -1)
  
  p1 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_A, name = "Temperatura A", line = list(color = 'orange')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_A,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temperatura (A)"))
  
  p2 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_B, name = "Temperatura B", line = list(color = 'green')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_B,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temperatura (B)"))
  
  p3 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_C, name = "Temperatura C", line = list(color = 'blue')) %>%
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_C,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temperatura (C)"))
  
  p4 <- plot_ly(df_exp, x = ~new_time) %>%
    add_lines(y = ~TEMPERATURE_D, name = "Temperatura D", line = list(color = 'purple')) %>% 
    add_markers(data = df_fallas, x = ~new_time, y = ~TEMPERATURE_D,
                marker = list(color = 'red', size = 6, symbol = 'x'), name = "Anomalía",
                text = ~paste("Score:", round(anomaly_score, 4)), hoverinfo = "text") %>%
    layout(yaxis = list(title = "Temperatura (D)"))
  
  subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("Temperaturas - Fecha:", fecha_str, "| Experimento:", experimento),
           xaxis = list(title = "Línea de tiempo (new_time)"),
           hovermode = "x unified")
}

crear_matriz <- function(df, titulo) {
  matriz_cor <- cor(df, use = "complete.obs")
  df_cor <- as.data.frame(as.table(matriz_cor))
  
  ggplot(df_cor, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#0277BD", high = "#43A047", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    #scale_fill_gradient2(low = "#0277BD", high = "#43A047", mid = "white", midpoint = 0, limit = c(-1, 1))
    geom_text(aes(label = round(Freq, 2)), color = "black", size = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(face = "bold", size = 16)) +
    labs(title = titulo, x = "", y = "", fill = "Corr")
}

# =========================================================================
#                Ui

ui <- fluidPage(
  
  titlePanel(tags$h2("Visualización interactiva y detección de anomalías en micromovilidad eléctrica mediante Shiny", 
                     style = "font-weight: bold; color: #2E7D32;")),
  
  sidebarLayout(
    sidebarPanel(
      # Logo de la bicicleta
      img(src = "Bici_png.jpg", width = "425px", style = "display: block; margin: 0 auto 20px auto;"),
      
      selectInput("sel_fecha", "1. Seleccionar fecha:", 
                  choices = sort(unique(datos_bici$fecha), decreasing = FALSE)),
      
      uiOutput("ui_experimentos"),
      
      selectInput("sel_grafica", "3. Seleccionar gráfica:",
                  choices = c("Serie 1 (Variables A)",
                              "Serie 2 (Corrientes B, C y D)",
                              "Serie 3 (Voltajes B, C y D)",
                              "Serie 4 (Temperaturas)",
                              "Matriz correlación: Sin anomalías",
                              "Matriz correlación: Con anomalías")),
      
      actionButton("btn_generar", "Graficar", class = "btn-primary", width = "100%"),
      
      hr(),
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Resumen del día", style = "margin-top: 0;"),
          htmlOutput("info_resumen")
      )
    ),
    
    mainPanel(
      uiOutput("area_grafico")
    )
  )
)

# ============================================================================
#        Server

server <- function(input, output, session) {
  
  datos_dia <- reactive({
    datos_bici %>% filter(fecha == input$sel_fecha)
  })
  
  output$ui_experimentos <- renderUI({
    req(datos_dia())
    exps_disponibles <- unique(datos_dia()$exp)
    selectInput("sel_exp", "2. Seleccionar experimento:", choices = exps_disponibles)
  })
  
  output$info_resumen <- renderUI({
    df <- datos_dia()
    req(nrow(df) > 0)
    req(input$sel_exp)
    
    rutas <- paste(unique(df$lugar), collapse = ", ")
    experimentos <- paste(unique(df$exp), collapse = ", ")
    anomalias_totales <- nrow(df %>% filter(anomaly == -1))
    
    anomalias_exp <- nrow(df %>% filter(exp == input$sel_exp, anomaly == -1))
    
    HTML(paste0(
      "<b>Ruta(s):</b> ", rutas, "<br>",
      "<b>Experimentos:</b> ", experimentos, "<br>",
      "<b>Anomalías registradas en ", input$sel_exp, ":</b> ", anomalias_exp
    ))
  })
  
  grafica_seleccionada <- eventReactive(input$btn_generar, {
    req(input$sel_exp)
    
    df_exp <- datos_dia() %>% 
      filter(exp == input$sel_exp) %>% 
      arrange(new_time)
    
    validate(need(nrow(df_exp) > 0, "No hay datos para este experimento."))
    
    tipo <- input$sel_grafica
    fecha_str <- as.character(input$sel_fecha)
    
    # Nombres en español para las matrices
    vars_crit <- c("CURRENT_A_CALC", "CURRENT_B_CALC", "CURRENT_C_CALC", "CURRENT_D_CALC",
                   "VOLTAGE_A", "VOLTAGE_B", "VOLTAGE_C", "VOLTAGE_D", 
                   "TEMPERATURE_MOTOR")
    
    nombres_esp <- c("Corriente A", "Corriente B", "Corriente C", "Corriente D",
                         "Voltaje A", "Voltaje B", "Voltaje C", "Voltaje D", 
                         "Temp. motor")
    
    if (tipo == "Serie 1 (Variables A)") {
      return(graficar_serie1(df_exp, input$sel_exp, fecha_str))
      
    } else if (tipo == "Serie 2 (Corrientes B, C y D)") {
      return(graficar_serie2(df_exp, input$sel_exp, fecha_str))
      
    } else if (tipo == "Serie 3 (Voltajes B, C y D)") {
      return(graficar_serie3(df_exp, input$sel_exp, fecha_str))
      
    } else if (tipo == "Serie 4 (Temperaturas)") {
      return(graficar_serie4(df_exp, input$sel_exp, fecha_str))
      
    } else if (tipo == "Matriz correlación: Sin anomalías") {
      
      df_sanos <- df_exp %>% filter(anomaly == 1) %>% select(all_of(vars_crit))
      colnames(df_sanos) <- nombres_esp
      
      validate(need(nrow(df_sanos) > 1, "No hay suficientes datos regulares para calcular correlaciones."))
      titulo <- paste("Matriz de correlación sin anomalías - Fecha:", fecha_str, "| Experimento:", input$sel_exp)
      return(crear_matriz(df_sanos, titulo))
      
    } else if (tipo == "Matriz correlación: Con anomalías") {
      
      df_anomalos <- df_exp %>% 
        filter(anomaly == -1) %>% select(all_of(vars_crit))
      
      colnames(df_anomalos) <- nombres_esp
      
      # Validación
      validate(need(nrow(df_anomalos) > 1, "El experimento seleccionado no cuenta con los datos atípicos suficientes para generar la matriz."))
      
      titulo <- paste("Matriz de correlación con anomalías - Fecha:", fecha_str, "| Experimento:", input$sel_exp)
      return(crear_matriz(df_anomalos, titulo))
    }
  })
  
  output$area_grafico <- renderUI({
    req(input$sel_grafica) 
    
    if (grepl("Matriz", input$sel_grafica)) {
      plotOutput("plot_estatico", height = "700px")
    } else {
      plotlyOutput("plot_interactivo", height = "800px")
    }
  })
  
  output$plot_interactivo <- renderPlotly({
    req(grepl("Serie", input$sel_grafica))
    grafica_seleccionada()
  })
  
  output$plot_estatico <- renderPlot({
    req(grepl("Matriz", input$sel_grafica))
    grafica_seleccionada()
  })
}

shinyApp(ui = ui, server = server)


