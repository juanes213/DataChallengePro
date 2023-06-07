library(shiny)
library(plotly)
library(vegan)

generar_cuadro <- function(fecha) {
  mes_aleatorio <- sample(unique(df_clean$month), 1)
  especies_mes <- nrow(subset(df_clean, month == mes_aleatorio))
  especies_endemicas <- nrow(Plantas_Endemicas) + nrow(Aves_Endemicas)
  cuadro <- paste("Fecha:", fecha, "<br>",
                  "Especies Registradas:", especies_mes, "<br>",
                  "Especies End�micas:", especies_endemicas, "<br>")
  return(cuadro)
}


dataset_names <- c("Monitoreos en �reas de compensaci�n por p�rdida de biodiversidad relacionadas al Gasoducto Loop San Mateo",
                   "Monitoreos en �reas de compensaci�n por p�rdida de biodiversidad relacionadas al Gasoducto Mamonal-Paiva",
                   "Biodiversidad asociada al gasoducto Loop San Mateo-Mamonal", 
                   "Biodiversidad asociada al gasoducto Loop Mamonal-Paiva",
                   "Biodiversidad asociada al gasoducto Paiva-Caracol�",
                   "Biodiversidad asociada al gasoducto regional Zona Bananera",
                   "Biodiversidad asociada a un Estudio de Impacto Ambiental entre Jobo (Sahag�n) y Sebastopol (Medell�n)",
                   "Biodiversidad asociada al gasoducto Loop Jobo-Majaguas",
                   "Monitoreo de fauna amenazada y cambios poblacionales asociados a las fichas del PMA del Gasoducto Paiva-Caracol�",
                   "Monitoreos de fauna y flora en �reas de influencia directa e indirecta relacionadas al cumplimiento del PMA y PMS del gasoducto Paiva-Caracol�",
                   "Diversidad de bri�fitos, liquenes y hongos en las zonas de enriquecimiento del gasoducto Jobo-Majaguas y San Mateo-Mamonal, departamento de Sucre",
                   "Monitoreos en �reas de compensaci�n relacionadas al Gasoducto Jobo Majaguas (Variante Mateca�a)")


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
      selectInput("chartType", "Seleccionar tipo de gr�fico:",
                  choices = c("Histograma", "Gr�fico de Barras", "Especie / tiempo", "Especies m�s comunes"),
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

# Define el servidor de la aplicaci�n Shiny
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
      } else if (input$chartType == "Gr�fico de Barras") {
        colores_reinos <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2")
        plot_ly(selectedData, x = ~kingdom, type = "histogram", marker = list(color = colores_reinos)) %>%
          layout(xaxis = list(title = "Reino"), yaxis = list(title = "N�mero de especies"), 
                 title = "Distribuci�n de especies por Reino")
      } else if (input$chartType == "Especie / tiempo"){
        plot_ly(selectedData, x = ~eventDate, type = "histogram") %>%
          layout(xaxis = list(title = "Fecha"), yaxis = list(title = "N�mero de especies"), 
                 title = "N�mero de especies encontradas a lo largo del tiempo")
      } else if (input$chartType == "Especies m�s comunes"){

        species_counts <- table(selectedData$scientificName)
        top_species <- head(sort(species_counts, decreasing = TRUE), 10)
        df_top_species <- selectedData[selectedData$scientificName %in% names(top_species), ]
        
        df_top_species$scientificName <- factor(df_top_species$scientificName, levels = rev(names(top_species)))
        
        plot_ly(df_top_species, x = ~scientificName, type = "histogram") %>%
          layout(xaxis = list(title = "Especie"), yaxis = list(title = "N�mero de registros"), 
                 title = "Las 10 especies m�s comunes")
      }
    })
  })
  
  output$cuadro <- renderPrint({
    fecha <- input$fecha
    generar_cuadro(fecha)
  })
}

# Ejecuta la aplicaci�n Shiny
shinyApp(ui, server)