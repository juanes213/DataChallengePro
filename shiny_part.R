library(shiny)
library(plotly)
library(vegan)

generar_cuadro <- function(fecha) {
  mes_aleatorio <- sample(unique(df_clean$month), 1)
  especies_mes <- nrow(subset(df_clean, month == mes_aleatorio))
  especies_endemicas <- nrow(Plantas_Endemicas) + nrow(Aves_Endemicas)
  cuadro <- paste("Fecha:", fecha, "<br>",
                  "Especies Registradas:", especies_mes, "<br>",
                  "Especies Endémicas:", especies_endemicas, "<br>")
  return(cuadro)
}


dataset_names <- c("Monitoreos en áreas de compensación por pérdida de biodiversidad relacionadas al Gasoducto Loop San Mateo",
                   "Monitoreos en áreas de compensación por pérdida de biodiversidad relacionadas al Gasoducto Mamonal-Paiva",
                   "Biodiversidad asociada al gasoducto Loop San Mateo-Mamonal", 
                   "Biodiversidad asociada al gasoducto Loop Mamonal-Paiva",
                   "Biodiversidad asociada al gasoducto Paiva-Caracolí",
                   "Biodiversidad asociada al gasoducto regional Zona Bananera",
                   "Biodiversidad asociada a un Estudio de Impacto Ambiental entre Jobo (Sahagún) y Sebastopol (Medellín)",
                   "Biodiversidad asociada al gasoducto Loop Jobo-Majaguas",
                   "Monitoreo de fauna amenazada y cambios poblacionales asociados a las fichas del PMA del Gasoducto Paiva-Caracolí",
                   "Monitoreos de fauna y flora en áreas de influencia directa e indirecta relacionadas al cumplimiento del PMA y PMS del gasoducto Paiva-Caracolí",
                   "Diversidad de briófitos, liquenes y hongos en las zonas de enriquecimiento del gasoducto Jobo-Majaguas y San Mateo-Mamonal, departamento de Sucre",
                   "Monitoreos en áreas de compensación relacionadas al Gasoducto Jobo Majaguas (Variante Matecaña)")


ui <- fluidPage(
  titlePanel("Explorador de Datos"),
  
  # Cambios visuales: Estilos CSS
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: #f2f2f2;
        }
        
        .sidebar {
          background-color: #ffffff;
          border-right: 1px solid #e6e6e6;
        }
        
        .main-panel {
          background-color: #ffffff;
          border-left: 1px solid #e6e6e6;
        }
        
        h1 {
          color: #333333;
          font-size: 24px;
          margin-top: 20px;
          margin-bottom: 20px;
        }
        
        .selectize-input {
          border-radius: 0;
        }
        
        .btn-primary {
          background-color: #4c8be2;
          border-color: #4c8be2;
          color: #ffffff;
          font-weight: bold;
        }
        
        .btn-primary:hover {
          background-color: #4076d4;
          border-color: #4076d4;
        }
        
        .plot-container {
          margin-top: 20px;
          margin-bottom: 20px;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        "
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Seleccionar conjunto de datos:",
                  choices = dataset_names,
                  selected = dataset_names[1]),
      selectInput("chartType", "Seleccionar tipo de gráfico:",
                  choices = c("Histograma", "Gráfico de Barras", "Especie / tiempo", "Especies más comunes"),
                  selected = "Histograma"),
      dateInput("fecha", "Seleccionar fecha:", value = Sys.Date()),
      
      # Cambios visuales: Estilos CSS
      tags$style(
        HTML(
          "
          .sidebar-panel {
            padding: 20px;
          }
          
          .form-group {
            margin-bottom: 20px;
          }
          
          label {
            font-weight: bold;
            font-size: 14px;
            color: #333333;
            margin-bottom: 10px;
          }
          
          .selectize-control.single .selectize-input {
            height: 36px;
            line-height: 34px;
            font-size: 14px;
            border-color: #e6e6e6;
          }
          
          .selectize-dropdown {
            border-color: #e6e6e6;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
            font-size: 14px;
            color: #333333;
            border-radius: 0;
          }
          
          .form-control {
            height: 36px;
            font-size: 14px;
            border-color: #e6e6e6;
          }
          
          .btn {
            border-radius: 3px;
            font-size: 14px;
          }
          
          .btn-primary {
            padding: 8px 16px;
          }
          "
        )
      )
    ),
    
    mainPanel(
      div(
        id = "plot-container",
        class = "plot-container",
        plotlyOutput("plot")
      ),
      div(
        id = "cuadro-container",
        class = "plot-container",
        verbatimTextOutput("cuadro")
      )
    )
  )
)

# Define el servidor de la aplicación Shiny
server <- function(input, output) {
  observeEvent(input$dataset, {
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    
    output$plot <- renderPlotly({
      if (input$chartType == "Histograma") {
        plot_ly(selectedData, x = ~month) %>%
          add_histogram(marker = list(color = "blue", opacity = 0.7), 
                        hovertemplate = "Mes: %{x}<br>Registros: %{y}") %>%
          layout(xaxis = list(title = "Mes"), yaxis = list(title = "Conteo de registros"), 
                 title = "Conteo de Registros por Mes")
      } else if (input$chartType == "Gráfico de Barras") {
        colores_reinos <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2")
        plot_ly(selectedData, x = ~kingdom, type = "histogram", marker = list(color = colores_reinos)) %>%
          layout(xaxis = list(title = "Reino"), yaxis = list(title = "Número de especies"), 
                 title = "Distribución de especies por Reino")
      } else if (input$chartType == "Especie / tiempo"){
        plot_ly(selectedData, x = ~eventDate, type = "histogram") %>%
          layout(xaxis = list(title = "Fecha"), yaxis = list(title = "Número de especies"), 
                 title = "Número de especies encontradas a lo largo del tiempo")
      } else if (input$chartType == "Especies más comunes"){

        species_counts <- table(selectedData$scientificName)
        top_species <- head(sort(species_counts, decreasing = TRUE), 10)
        df_top_species <- selectedData[selectedData$scientificName %in% names(top_species), ]
        
        df_top_species$scientificName <- factor(df_top_species$scientificName, levels = rev(names(top_species)))
        
        plot_ly(df_top_species, x = ~scientificName, type = "histogram") %>%
          layout(xaxis = list(title = "Especie"), yaxis = list(title = "Número de registros"), 
                 title = "Las 10 especies más comunes")
      }
    })
  })
  
  output$cuadro <- renderPrint({
    fecha <- input$fecha
    generar_cuadro(fecha)
  })
}

# Ejecuta la aplicación Shiny
shinyApp(ui, server)
