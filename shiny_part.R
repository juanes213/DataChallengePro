library(rgbif)
library(shiny)
library(vegan)
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(nortest)
library(rredlist)
library(leaflet)
library(googleway)
library(stringr)
library(plotly)
library(forecast)
library(nnet)
library(pROC)
library(treemap)
library(plotly)
library(packcircles)
library(viridis)
library(ggiraph)
library(d3treeR)
library(d3tree2)
library(shinyWidgets)
library(shinydashboard)
library(shinymaterial)


# CARGAR LOS DATOS DE PROMIGAS
orgs <- organizations(country = "CO", limit = 60)
promigas <- orgs$data$key[grep("Promigas", orgs$data$title)]
promigas_dt <- dataset_search(publishingOrg = promigas)
df_pro <- promigas_dt$data
dataset_keys <- df_pro$datasetKey
df_list <- list()

for (i in 1:length(dataset_keys)) {
  dtset <- occ_download(pred("datasetKey", dataset_keys[i]), user = "juanes200", pwd = "juanalbis200", email = "albisj@uninorte.edu.co")
  
  occ_download_wait(dtset)
  
  df <- occ_download_get(dtset) %>% occ_download_import()
  df_list[[i]] <- df
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


informacion_datasets <- c("Objetivo: compensación por el proyecto del gasoducto de loop San Mateo (Bosque seco tropical)",
                          "Objetivo: compensación ambiental asociadas al Gasoducto Mamonal - Paiva.",
                          
                          "Objetivo: estudio de impacto ambiental en la zona para la construcción del gaseoducto.",
                          
                          "Objetivo: construcción del gasoducto Loop Mamonal-Paiva.",
                          
                          "Objetivo: construcción del gaseoducto Paiva-Caracalí",
                          
                          "Objetivo: Gasoducto Regional Zona Bananera" consistirá en un sistema de transporte de gas natural con una tubería de 10'' de diámetro, una longitud de 50,61 km, el cual inicia en la vereda Jolonura, municipio de Ciénaga, y finaliza en la Vereda El Trébol, también del municipio de Ciénaga.",
                          
                          "Objetivo: Estudio de Impacto Ambiental que evaluó fauna, flora y comunidades hidrobiológicas, un inventario forestal del área de influencia directa junto a las especies de plantas epífitas asociadas. ",
                          
                          "Objetivo: Construcción y Operación del Gasoducto Loop Jobo-Mamonal, Estudio de Impacto Ambiental (EIA) para la modificación del plan de manejo ambiental (Resolución 751 de 30 de junio de 2017) para la construcción y operación del gasoducto Jobo-Mamona.",
                          
                          "Objetivo: monitoreo y seguimiento del Plan de Manejo Ambiental (PMA), comparar los resultados de riqueza y abundancia de fauna amenazada en 2022 con los obtenidos en la línea base del EIA del 2017, para establecer variaciones a causa de la ejecución del proyecto y para cumplir los lineamientos establecidos para su monitoreo en las ficha 15 y 18E relacionadas con la conservación y manejo de fauna silvestre, la fauna amenazada y la endémica.",
                          
                          "Objetivo: establecimiento y caracterización de la vegetación en una parcela para los tipos de cobertura: vegetación secundaria alta, vegetación secundaria baja, arbustal denso, arbustal bajo, y bosque ripario.",
                          
                          "Objetivo: caracterización de epífitas líquenes y plantas no vasculares presentes en las zonas de enriquecimiento destinadas para crear hábitats que favorezcan la colonización de epífitas no vasculares ubicadas en las áreas de Influencia indirecta de los gasoductos de Promigas S.A._ESP que operan en los municipios de San Onofre (Gasoducto Loop San Mateo-Mamonal) y Sincelejo (Gasoducto Loop Jobo-Majaguas)",
                          
                          "El presente trabajo tiene como finalidad obtener el primer monitoreo de los 	indicadores de flora, fauna y coberturas vegetales en el marco del proyecto de 		construcción y operación del gasoducto Loop Jobo-Mamonal: primera etapa jobo-	majaguas "variante mata de caña"- Sampués, Sucre")


paleta <- c("#85B84A", "#408C4C", "#29907D", "#4E8855", "#B8C290", "#B3C93A",
                     "#A2D977", "#5B7B4E", "#7CC1AB", "#8BA649", "#629970", "#77AD5B",
                     "#64B18B", "#43875E", "#5FBF9D", "#7CCF5A", "#69AF9B", "#86D88E",
                     "#A0C970", "#589E67", "#76B176", "#7FBF9D", "#9BCF7D", "#B5C564",
                     "#6EAB8B", "#96D487", "#A5D672", "#4A9066", "#7FBB4E", "#54A168",
                     "#7ED48A", "#98C05A", "#C6E568", "#92D4A4", "#4C9A6C", "#9ED06A",
                     "#5F8E62", "#80B685", "#D2E252", "#A8C482", "#85AA6D", "#B3DB51",
                     "#5F9544", "#84B57D", "#BCCE76", "#C4D581", "#A9D754", "#6AB462",
                     "#A7C06A", "#D2DB53", "#77A459", "#67A68A", "#4E7F56", "#8FA75B",
                     "#6CB971", "#B6CF79", "#7EB16E", "#6F9970", "#B1C97B", "#8FB876",
                     "#A5CA65", "#6EA05B", "#A1D279")


############################ UI ################################# 


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Datos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos", tabName = "graficos"),
      menuItem("Modelo", tabName = "modelo"),
      menuItem("Análisis de compensación", tabName = "analisis")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          .metric-box {
            background-color: #4E8855;
            padding: 10px;
            border: 1px solid #ccc;
            text-align: center;
            margin-bottom: 10px;
            height: 120px;
          }
          
          .box {
            background-color: #43824D;
            color: white;
            border: none;
          }
          
          .content-wrapper, .right-side {
            background-color: #C2D7BB;
          }
        ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "graficos",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Selección de conjunto de datos",
              selectInput(
                "dataset", "Seleccionar conjunto de datos:",
                choices = dataset_names,
                selected = dataset_names[1]
              )
            ),
            box(
              title = "Selección de tipo de gráfico",
              selectInput(
                "chartType", "Seleccionar tipo de gráfico:",
                choices = c(
                  "Histograma",
                  "Gráfico de Barras",
                  "Reinos y Clases",
                  "Especie a lo largo del tiempo",
                  "Especies más comunes",
                  "Registros por Año y Mes",
                  "Registros por Categoría Taxonómica",
                  "Mapa de Registros",
                  "Numero especies por clase Animalia y Plantae",
                  "Categoría de Amenaza",
                  "Circle Chart",
                  "Mapa Aves Endemicas"
                ),
                selected = "Especies más comunes"
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              uiOutput("numOccurrences"),
              uiOutput("numKingdoms"),
              uiOutput("numGenera"),
              uiOutput("numClases"),
              uiOutput("riqueza_especies"),
              uiOutput("shannon")
            ),
            column(
              width = 9,
              plotlyOutput("plot", height = "600px")
            )
          )
        )
      ),
      tabItem(
        tabName = "modelo",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Explicación del Modelo",
              tags$div(
                style = "background-color: #f9f9f9; padding: 20px; border-radius: 5px; border: 1px solid #dddddd;",
                verbatimTextOutput("explicacion_modelo")
              )
            )
          ),
          column(
            width = 7,
            box(
              title = "Información de la Variable IUCN",
              tableOutput("modelo_info")
            )
          ),
          column(
            width = 9,
            box(
              title = "Accuracy",
              verbatimTextOutput("modelo_accuracy")
            ),
            box(
              title = "Coeficientes",
              plotOutput("coeficientes_plot")
            )
          )
        )
      ),
      tabItem(
        tabName = "analisis",
        fluidRow(
          box(
            title = "Análisis de compensación San Mateo / Paiva",
            p("En este apartado conoceremos algunos análisis planteados para evaluar la compensación que hubo en alguno de los estudios realizados por promigas"),
            h2("Análisis compensación del reino plantae"),
            p("Se quiere verificar si hay una diferencia entre las proporciones de plantas entre el estudio de biodiversidad y el estudio por compensación"),
            p("Se realizó una prueba de hipótesis, donde se estableció una hipótesis alternativa y una hipótesis nula"),
            p("Hipótesis alternativa: Hay diferencia significativa entre la proporción de plantas de la biodiversidad en contraste con la compensación"),
            p("Hipótesis alternativa: No hay diferencia significativa entre la proporción de plantas de la biodiversidad en contraste con la compensación"),
            verbatimTextOutput("resultado_plantae5"),
            verbatimTextOutput("texto_plantae5"),
            h2("Análisis compensación del reino Animalia"),
            p("Se quiere verificar si hay una diferencia entre las proporciones de animales entre el estudio de biodiversidad y el estudio por compensación"),
            p("Se realizó una prueba de hipótesis, donde se estableció una hipótesis alternativa y una hipótesis nula"),
            p("Hipótesis alternativa: Hay diferencia significativa entre la proporción de animales de la biodiversidad en contraste con la compensación"),
            p("Hipótesis alternativa: No hay diferencia significativa entre la proporción de animales de la biodiversidad en contraste con la compensación"),
            verbatimTextOutput("resultado_animalia5"),
            verbatimTextOutput("texto_animalia5")
          ),
          box(
            title = "Análisis de compensación Mamonal / Paiva",
            p("En este apartado conoceremos algunos análisis planteados para evaluar la compensación que hubo en alguno de los estudios realizados por promigas"),
            h2("Análisis compensación del reino Bacteria"),
            p("Se quiere verificar si hay una diferencia entre las proporciones de bacteria entre el estudio de biodiversidad y el estudio por compensación"),
            p("Se realizó una prueba de hipótesis, donde se estableció una hipótesis alternativa y una hipótesis nula"),
            p("Hipótesis alternativa: Hay diferencia significativa entre la proporción de bacterias de la biodiversidad en contraste con la compensación"),
            p("Hipótesis alternativa: No hay diferencia significativa entre la proporción de bacterias de la biodiversidad en contraste con la compensación"),
            verbatimTextOutput("resultado_bacteria"),
            verbatimTextOutput("texto_bacteria"),
            h2("Análisis compensación del reino Protozoa"),
            p("Se quiere verificar si hay una diferencia entre las proporciones de protozoos entre el estudio de biodiversidad y el estudio por compensación"),
            p("Se realizó una prueba de hipótesis, donde se estableció una hipótesis alternativa y una hipótesis nula"),
            p("Hipótesis alternativa: Hay diferencia significativa entre la proporción de protozoos de la biodiversidad en contraste con la compensación"),
            p("Hipótesis alternativa: No hay diferencia significativa entre la proporcióón de protozoos de la biodiversidad en contraste con la compensación"),
            verbatimTextOutput("resultado_protozoa"),
            verbatimTextOutput("texto_protozoa"),
            h2("Análisis compensación del reino Chromista"),
            p("Se quiere verificar si hay una diferencia entre las proporciones de Chromistas entre el estudio de biodiversidad y el estudio por compensación"),
            p("Se realizó una prueba de hipótesis, donde se estableció una hipótesis alternativa y una hipótesis nula"),
            p("Hipótesis alternativa: Hay diferencia significativa entre la proporción de chromistas de la biodiversidad en contraste con la compensación"),
            p("Hipótesis alternativa: No hay diferencia significativa entre la proporción de Chromistas de la biodiversidad en contraste con la compensación"),
            verbatimTextOutput("resultado_chromista"),
            verbatimTextOutput("texto_chromista")
          )
        )
      )
    )
  )
)



################################### SERVIDOR ################################


server <- function(input, output) {
  
  numOccurrences <- reactive({
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    nrow(selectedData)
  })
  
  output$numOccurrences <- renderValueBox({
    valueBox(numOccurrences(), "Número de ocurrencias", width = 10)
  })
  
  numKingdoms <- reactive({
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    length(unique(selectedData$kingdom))
  })
  
  output$numKingdoms <- renderValueBox({
    valueBox(numKingdoms(), "Reinos", width = 10)
  })
  
  numGenera <- reactive({
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    length(unique(selectedData$genus))
  })
  
  output$numGenera <- renderValueBox({
    valueBox(numGenera(), "Genus", width = 10)
  })
  
  numClasses <- reactive({
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    length(unique(selectedData$class))
  })
  
  output$numClasses <- renderValueBox({
    valueBox(numClasses(), "Clases", width = 10)
  })
  riqueza_especies <- reactive({
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    riqueza_especies <- length(unique(selectedData$species))
    
  })
  
  output$riqueza_especies <- renderValueBox({
    valueBox(riqueza_especies(), "Riqueza de especies", width = 10)
  })
  shannon <- reactive({
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    proporciones <- table(selectedData$species) / length(selectedData$species)
    Shannon <- -sum(proporciones * log(proporciones, base = exp(1)))
    round(Shannon,2)
  })
  output$shannon <- renderValueBox({
    valueBox(shannon(), "Shannon", width = 10)
  })

  observeEvent(input$dataset, {
    selectedData <- df_list[[which(dataset_names == input$dataset)]]
    
    eliminar_characters<- function(texto) {
      texto <- gsub(",.*|\\(.*\\)", "", texto)
      return(trimws(texto))                     
    }
    selectedData$scientificName <- sapply(selectedData$scientificName, eliminar_characters)
  
    ######################## GRAFICAS#############################################
    
    
    
    output$plot <- renderPlotly({
      if (input$chartType == "Histograma") {
        plot_ly(selectedData, x = ~month) %>%
          add_histogram(
            marker = list(color = paleta, opacity = 0.7),
            hovertemplate = "Mes: %{x}<br>Registros: %{y}"
          ) %>%
          layout(
            xaxis = list(title = "Mes"),
            yaxis = list(title = "Conteo de registros"),
            title = "Conteo de Registros por Mes"
          )
      } else if (input$chartType == "Gráfico de Barras") {
        plot_ly(selectedData, x = ~species, type = "histogram", marker = list(color = paleta)) %>%
          layout(
            xaxis = list(title = "Especie"),
            yaxis = list(title = "Número de ocurrencias"),
            title = "Distribución de especies por Especie"
          )
      } else if (input$chartType == "Reinos y Clases") {
        reino_04 <- selectedData$kingdom
        clase_04 <- selectedData$class
        valor_04 <- rep(1, length(selectedData$kingdom))
        data_04 <- data.frame(reino_04, clase_04, valor_04)
        
        prueba_04 <- treemap(data_04,
                             index = c("reino_04", "clase_04"),
                             vSize = "valor_04",
                             type = "index",
                             palette = paleta,
                             bg.labels = "white",
                             align.labels = list(c("center", "center"), c("right", "bottom"))
        )
        
        plot_ly(prueba_04)  

        
      } else if (input$chartType == "Especie a lo largo del tiempo") {
        plot_ly(df_clean, x = df_clean$eventDate, type = "histogram",marker = list(color = "#408c4c", opacity = 0.7)) %>%
          layout(xaxis = list(title = "Fecha"), yaxis = list(title = "Número de especies"), 
                 title = "Número de especies encontradas a lo largo del tiempo")
        
      } else if (input$chartType == "Especies más comunes") {
        species_counts <- table(selectedData$species) %>%
          sort(decreasing = TRUE)
        top_species <- head(species_counts, n = 10)
        top_species <- top_species[order(-top_species)]
        species_names <- names(top_species)
        
        plot_ly(
          x = ~factor(species_names, levels = species_names),
          y = top_species,
          type = "bar",
          marker = list(color = paleta)
        ) %>%
          layout(
            xaxis = list(
              title = "Especie",
              categoryorder = "array",
              categoryarray = species_names
            ),
            yaxis = list(title = "Número de ocurrencias"),
            title = "Especies más comunes"
          )
      } else if (input$chartType == "Registros por Año y Mes"){
        selectedData$year_month <- format(as.Date(paste(selectedData$year, selectedData$month, "01", sep = "-")), "%Y-%b")
        scatter_data <- table(selectedData$year_month)
        
        plot_ly(x = names(scatter_data), y = as.vector(scatter_data), type = "scatter", mode = "markers",
                marker = list(size = 8, color = "#FFA500", opacity = 0.7)) %>%
          layout(xaxis = list(title = "Año y Mes"), yaxis = list(title = "Conteo de Registros"), 
                 title = "Registros por Año y Mes")
      } else if (input$chartType == "Registros por Categoría Taxonómica"){
        taxon_counts <- table(selectedData$taxonRank)
        labels <- names(taxon_counts)
        values <- as.vector(taxon_counts)
        
        plot_ly(y = labels, x = values, type = "bar", orientation = "h", 
                marker = list(color = "#6CB971", opacity = 0.7)) %>%
          layout(yaxis = list(title = "Categoría Taxonómica"), xaxis = list(title = "Conteo de Registros"), 
                 title = "Conteo de Registros por Categoría Taxonómica")
        
      } else if (input$chartType == "Mapa de Registros"){
        colombia_bounds <- list(
          lon = c(-79, -66),  
          lat = c(-5, 13)    
        )
        map_data <- selectedData %>%
          dplyr::select(decimalLongitude, decimalLatitude, scientificName)
        
        plot_ly(map_data, x = ~decimalLongitude, y = ~decimalLatitude, type = "scattermapbox",
                       mode = "markers", marker = list(color = "blue", opacity = 0.7),
                       text = ~scientificName) %>%
          layout(mapbox = list(
            style = "carto-positron",
            zoom = 5,
            center = list(lon = -73, lat = 4),
            bearing = 0,
            pitch = 0,
            bounds = colombia_bounds
          ),
          title = "Registros"
          )
      } else if (input$chartType == "Numero especies por clase Animalia y Plantae") {
        
        # Calcular el conteo de especies por clase y reino
        species_counts <- selectedData %>%
          group_by(class, kingdom) %>%
          summarize(num_species = n()) %>%
          arrange(desc(num_species))
        
        # Filtrar las clases con un conteo mayor a 550
        filtered_classes <- species_counts %>%
          filter(num_species > 500)
        
        # Ajustar los nombres de las clases para mejorar la legibilidad
        filtered_classes$class <- ifelse(nchar(filtered_classes$class) > 20, substr(filtered_classes$class, 1, 20), filtered_classes$class)
        
        # Gráfico de barras para el conteo de especies por clase y reino
        plot_ly(filtered_classes, x = ~reorder(class, num_species), y = ~num_species, color = ~kingdom, type = "bar") %>%
          add_text(x = ~reorder(class, num_species), y = ~num_species, text = ~num_species, textposition = "top", textfont = list(size = 10)) %>%
          layout(xaxis = list(title = "Clase"), yaxis = list(title = "Número de especies"), barmode = "group", title = "Número de especies por clase en Animalia y Plantae") %>%
          colorbar(colors = paleta, title = "Reino")
        
      } else if (input$chartType == "Categoría de Amenaza") {
        # Obtener información sobre la categoría de amenaza y distribución de las especies
        iucn_data <- selectedData %>%
          select(scientificName, iucnRedListCategory, stateProvince) %>%
          na.omit() %>%
          distinct()
        
        # Contar el número de especies en cada categoría de amenaza
        iucn_counts <- iucn_data %>%
          group_by(iucnRedListCategory) %>%
          summarize(num_species = n_distinct(scientificName)) %>%
          slice(-1)
        
        # Gráfico de barras para el número de especies en cada categoría de amenaza
        plot_ly(iucn_counts, x = ~iucnRedListCategory, y = ~num_species, type = "bar", marker = list(color = paleta)) %>%
          layout(title = "Número de especies en cada categoría de amenaza", xaxis = list(title = "Categoría de amenaza"), yaxis = list(title = "Número de especies"), showlegend = FALSE)
      }else if (input$chartType == "Circle Chart"){
        species_counts <- table(selectedData$scientificName)
        top_species <- head(sort(species_counts, decreasing = TRUE), 10)
        df_top_species <- selectedData[selectedData$scientificName %in% names(top_species), ]
        
        data_p <- data.frame(top_species)
        colnames(data_p) <- c("Nombre", "valor")
      
        packing3 <- circleProgressiveLayout(data_p$valor, sizetype = 'area')
        data_p <- cbind(data_p, packing3)
        dat.gg3 <- circleLayoutVertices(packing3, npoints = 50)
        
        pp <- ggplot() + 
          geom_polygon(data = dat.gg3, aes(x, y, group = id, fill = id), color = "black", alpha = 0.6) +
          scale_fill_viridis() +
          geom_text(data = data_p, aes(x, y, label = Nombre), size = 2, color = "black") +
          theme_void() + 
          theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
          coord_equal()

        ggplotly(pp, tooltip = "text")
        
      }else if (input$chartType == "Mapa Aves Endemicas"){
        df_unique <- selectedData[order(selectedData$scientificName), ]
        
        nombre_cientificos <- unique(df_unique$scientificName)
        
        coincidencia <- intersect(nombre_cientificos, nombre_endemica)
        
        # Filtrar las especies endémicas presentes en las aves observadas
        especies_coincidentes <- selectedData %>%
          filter(scientificName %in% coincidencia)
        
        nombres_especies <- unique(especies_coincidentes$scientificName)
        latitudes <- especies_coincidentes$decimalLatitude
        longitudes <- especies_coincidentes$decimalLongitude
        
        # Crear un dataframe con las ubicaciones y nombres de las especies
        ubicaciones <- data.frame(nombre = nombres_especies, lat = latitudes, lon = longitudes)
        
        colombia_bounds <- list(
          lon = c(-79, -66),
          lat = c(-5, 13)
        )
        
        mapa <- plot_ly(ubicaciones, x = ~lon, y = ~lat, text = ~nombre, type = "scattermapbox",
                        mode = "markers", marker = list(color = "darkgreen", opacity = 0.7)) %>%
          layout(mapbox = list(style = "carto-positron",
                               zoom = 5,
                               center = list(lon = -73, lat = 4),
                               bearing = 0,
                               pitch = 0,
                               bounds = colombia_bounds),
                 title = "Especies Endémicas en Colombia")
        
        # Imprimir el mapa
        mapa
      }
    })
  })
  
  
  ####################### MODELO #################################
  
  
  
  output$modelo_info <- renderTable({
    Tabla <- df_clean %>%
      dplyr::group_by(iucnRedListCategory) %>%
      dplyr::summarise(Total = n()) %>%
      dplyr::filter(iucnRedListCategory != "") %>%
      dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 2))
    
    Tabla
  })
  
  output$modelo_accuracy <- renderText({
    predictions <- predict(modelo, newdata = var_predict_complete, type = "class")
    confusion_matrix <- table(predictions, var_obj)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    coeficientes <- coef(modelo)
    
    paste("Accuracy:", accuracy)
  })
  
  output$coeficientes_plot <- renderPlot({
    coeficientes <- coef(modelo)
    
    barplot(coeficientes, col = paleta, legend.text = levels(var_obj))
  })
  
  
  ########################### COMPENSACION PRUEBA DE HIPOTESIS ############################### 
  
  
  # Realizar la prueba de proporciones para Plantae
  resultadoplantae5 <- prop.test(
    x = c(45843, 10343), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para Plantae
  output$resultado_plantae5 <- renderPrint({
    resultadoplantae5
  })
  
  # Mostrar el texto para Plantae
  output$texto_plantae5 <- renderText({
    "Con un P- Value de 2.2e-16, se concluye que sí hay diferencia significativa entre la proporción Plantae entre la biodiversidad
    del territorio y la compensación, por lo tanto la compensación de plantas no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para Animalia
  resultadoanimalia5 <- prop.test(
    x = c(2329, 724), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para Animalia
  output$resultado_animalia5 <- renderPrint({
    resultadoanimalia5
  })
  
  # Mostrar el texto para Animalia
  output$texto_animalia5 <- renderText({
    "Con un P-Value de 2.2e-16, sí hay diferencia significativa entre la proporción Animalia entre la biodiversidad del territorio
    y la compensación, por lo tanto la compensación subsecuente de animales no fue suficiente."
  })
  objetivo_dataset <- reactive({
    dataset_index <- which(dataset_names == input$dataset)
    informacion_datasets[dataset_index]
  })
  
  output$cuadro <- renderPrint({
    objetivo_dataset()
  })
  
  ######################### COMPENSACION PRUEBA DE HIPOTESIS 2 ####################################
  
  # Realizar la prueba de proporciones para Plantae
  resultadoplantae <- prop.test(
    x = c(45843, 10343), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para Plantae
  output$resultado_plantae <- renderPrint({
    resultadoplantae
  })
  
  # Mostrar el texto para Plantae
  output$texto_plantae <- renderText({
    "Con un P-Value de 2.2e-16,se concluye que sí hay diferencia significativa entre la proporciín Plantae entre la biodiversidad 
    del territorio y la compensación, por lo tanto la compensaciín de plantas no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para Animalia
  resultadoanimalia <- prop.test(
    x = c(2329, 724), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para Animalia
  output$resultado_animalia <- renderPrint({
    resultadoanimalia
  })
  
  # Mostrar el texto para Animalia
  output$texto_animalia <- renderText({
    "Con un P-Value de 0.0001337, se concluye que sí hay diferencia significativa entre la proporción Animalia 
    entre la biodiversidad del territorio y la compensación, por lo tanto la compensación subsecuente de animales no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para bacteria
  resultadobacteria <- prop.test(
    x = c(17, 17), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para bacteria
  output$resultado_bacteria <- renderPrint({
    resultadobacteria
  })
  
  # Mostrar el texto para bacteria
  output$texto_bacteria <- renderText({
    "Con un P-Value de 0.00404, se concluye que sí hay diferencia significativa entre la proporción de bacterias entre la biodiversidad 
    del territorio y la compensación, por lo tanto la presencia de bacterias es menor a la debida en el territorio compensado."
  })
  
  # Realizar la prueba de proporciones para protozoa
  resultadoprotozoa <- prop.test(
    x = c(51, 51), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para protozoa
  output$resultado_protozoa <- renderPrint({
    resultadoprotozoa
  })
  
  # Mostrar el texto para protozoa
  output$texto_protozoa <- renderText({
    "Con un P-Value de 1.928e-07, se concluye que sí hay diferencia significativa entre la proporción Protozoa entre la biodiversidad
    del territorio y la compensaciÃ³n, por lo tanto la compensaciÃ³n protozoos no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para chromista
  resultadochromista <- prop.test(
    x = c(113, 113), # NÃºmero de Ã©xitos en cada dataset
    n = c(48353, 17629), # NÃºmero total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para chromista
  output$resultado_chromista <- renderPrint({
    resultadochromista
  })
  
  # Mostrar el texto para chromista
  output$texto_chromista <- renderText({
    "Con un P-Value de 4.218e-15, se concluye que, sí hay diferencia significativa entre la proporción Chromista entre la biodiversidad 
    del territorio y la compensación, por lo tanto la compensaciÃ³n de chromista no fue suficiente."
  })
  
  output$explicacion_modelo <- renderText({
    "La importancia relativa de las variables se calculó utilizando los coeficientes absolutos y se representó en un gráfico de 
    barras horizontales. Se realizaron predicciones utilizando el modelo ajustado y se generó una matriz de confusión para comparar 
    las predicciones con los valores reales.Se calculó la exactitud del modelo, que fue de 0.77. 
    La exactitud es una medida que representa la proporción de predicciones correctas en relación con el total de predicciones realizadas. 
    Un valor de exactitud de 0.77 indica que el modelo clasificó correctamente el 77% de los casos.
    Para evaluar la importancia relativa de las variables en el modelo multinomial, se calcularon los coeficientes absolutos. 
    Estos coeficientes representan la magnitud del efecto de cada variable en la predicción de las diferentes categorías de la variable objetivo.
    Los coeficientes se ordenaron en orden descendente y se representaron en un gráfico de barras horizontales."
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

