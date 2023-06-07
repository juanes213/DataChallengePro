# Instalar paquetes y cargarlos
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
# Cargar los datos de las especies endemicas


Plantas_Endemicas <- read_excel("C:/Users/pc/Downloads/Plantas_Endemicas.xlsx")
Aves_Endemicas <- read_excel("C:/Users/pc/Downloads/Aves_Endemicas_Colombia_Tablas_Anexos (1).xlsx", 
                                                       sheet = "Anexo 1")
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

# FUNCIONES 
eliminar_var_nula <- function(datos) {
  for (col in colnames(datos)) {
    if (all(is.na(datos[[col]]))) {
      datos <- datos[, -which(names(datos) == col)]
    }
  }
  return(datos)
}


eliminar_characters<- function(texto) {
  texto <- gsub(",.*|\\(.*\\)", "", texto)
  return(trimws(texto))                     
}

# LIMPIAR DATASET
no_null_df <- eliminar_var_nula(df_list[[1]])
names(no_null_df)
df_clean <- subset(no_null_df, select = -c(institutionID,occurrenceID, countryCode,publishingCountry
                                           ,level0Gid, level0Name))
df_clean$scientificName <- sapply(df_clean$scientificName, eliminar_characters)

names(df_clean)

# Graficas ----

plot_ly(df_clean, x = ~month) %>%
  add_histogram(marker = list(color = "blue", opacity = 0.7), 
                hovertemplate = "Mes: %{x}<br>Registros: %{y}") %>%
  layout(xaxis = list(title = "Mes"), yaxis = list(title = "Conteo de registros"), 
         title = "Conteo de Registros por Mes")

colores_reinos <- c("#1f77b4", "#ff7f0e")
plot_ly(df_clean, x = ~kingdom, type = "histogram", marker = list(color = colores_reinos)) %>%
  layout(xaxis = list(title = "Reino"), yaxis = list(title = "N�mero de especies"), 
         title = "Distribuci�n de especies por Reino")

plot_ly(df_clean, x = ~eventDate, type = "histogram") %>%
  layout(xaxis = list(title = "Fecha"), yaxis = list(title = "N�mero de especies"), 
         title = "N�mero de especies encontradas a lo largo del tiempo")


# Contar la frecuencia de cada especie
species_counts <- table(df_clean$scientificName)
top_species <- head(sort(species_counts, decreasing = TRUE), 10)
df_top_species <- df_clean[df_clean$scientificName %in% names(top_species), ]

# Gr�fico de barras de las 5 especies m�s comunes
plot_ly(df_top_species, x = ~scientificName, type = "histogram") %>%
  layout(xaxis = list(title = "Especie"), yaxis = list(title = "N�mero de registros"), 
         title = "Las 10 especies m�s comunes")



df_clean$year_month <- format(as.Date(paste(df_clean$year, df_clean$month, "01", sep = "-")), "%Y-%b")
scatter_data <- table(df_clean$year_month)

plot_ly(x = names(scatter_data), y = as.vector(scatter_data), type = "scatter", mode = "markers",
        marker = list(size = 8, color = "#FFA500", opacity = 0.7)) %>%
  layout(xaxis = list(title = "A�o y Mes"), yaxis = list(title = "Conteo de Registros"), 
         title = "Registros por A�o y Mes")

taxon_counts <- table(df_clean$taxonRank)
labels <- names(taxon_counts)
values <- as.vector(taxon_counts)

plot_ly(y = labels, x = values, type = "bar", orientation = "h", 
        marker = list(color = "#00BFFF", opacity = 0.7)) %>%
  layout(yaxis = list(title = "Categor�a Taxon�mica"), xaxis = list(title = "Conteo de Registros"), 
         title = "Conteo de Registros por Categor�a Taxon�mica")


colombia_bounds <- list(
  lon = c(-79, -66),  
  lat = c(-5, 13)    
)
plot_ly(df_clean, x = ~decimalLongitude, y = ~decimalLatitude, type = "scattermapbox",
        mode = "markers", marker = list(color = "blue", opacity = 0.7)) %>%
  layout(mapbox = list(style = "carto-positron",
                       zoom = 5,
                       center = list(lon = -73, lat = 4),
                       bearing = 0,
                       pitch = 0,
                       bounds = colombia_bounds),
         title = "Registros")



grouped_data <- df_clean %>%
  group_by(decimalLatitude, decimalLongitude, eventDate) %>%
  summarize(num_species = n_distinct(scientificName))

change_data <- grouped_data %>%
  group_by(eventDate) %>%
  summarize(num_species_diff = n_distinct(num_species) - lag(n_distinct(num_species), default = 0))

plot_ly(change_data, x = ~eventDate, y = ~num_species_diff, type = "scatter", mode = "lines") %>%
  layout(xaxis = list(title = "Fecha"), yaxis = list(title = "Cambio en el n�mero de especies encontradas"), 
         title = "Cambio diario en el n�mero de especies encontradas")


# Calcular el conteo de especies por clase y reino
species_counts <- df_clean %>%
  group_by(class, kingdom) %>%
  summarize(num_species = n()) %>%
  arrange(desc(num_species))

# Filtrar las clases con un conteo mayor a 550
filtered_classes <- species_counts %>%
  filter(num_species > 500)

# Ajustar los nombres de las clases para mejorar la legibilidad
filtered_classes$class <- ifelse(nchar(filtered_classes$class) > 20, substr(filtered_classes$class, 1, 20), filtered_classes$class)

# Gr�fico de barras para el conteo de especies por clase y reino
ggplot(filtered_classes, aes(x = reorder(class, num_species), y = num_species, fill = kingdom)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = num_species), vjust = -0.5, color = "black", size = 3.5) + 
  labs(x = "Clase", y = "N�mero de especies", fill = "Reino") +
  ggtitle("N�mero de especies por clase en Animalia y Plantae") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom")



# ENDEMICAS Y RIESGO ----

# Obtener informaci�n sobre la categor�a de amenaza y distribuci�n de las especies
iucn_data <- df_clean %>%
  select(scientificName, iucnRedListCategory, stateProvince) %>%
  na.omit() %>%
  distinct()

# Contar el n�mero de especies en cada categor�a de amenaza
iucn_counts <- iucn_data %>%
  group_by(iucnRedListCategory) %>%
  summarize(num_species = n_distinct(scientificName)) %>%
  slice(-1)

# Mostrar el n�mero de especies en cada categor�a de amenaza
iucn_counts

# Gr�fico de barras para el n�mero de especies en cada categor�a de amenaza
ggplot(iucn_counts, aes(x = iucnRedListCategory, y = num_species, fill = iucnRedListCategory)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  labs(title = "N�mero de especies en cada categor�a de amenaza", x = "Categor�a de amenaza", y = "N�mero de especies") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = c("#FF7F0E", "#1F77B4", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"))

# ENDEMICAS AVES ----

# Ordenar los datos de forma �nica por la variable en orden alfab�tico
df_unique <- df_clean[order(df_clean$scientificName), ]

unique(df_unique$scientificName)

endemic <- Aves_Endemicas %>%
  filter(Cat. == 'E')

nombre_cientificos <- unique(df_clean$scientificName)
nombre_endemica <- unique(endemic$`Nombre cient�fico`)

coincidencia <- intersect(nombre_cientificos, nombre_endemica)

# Filtrar las especies end�micas presentes en las aves observadas
especies_coincidentes <- df_clean %>%
  filter(scientificName %in% nombres_coincidentes)

latitudes <- especies_coincidentes$decimalLatitude
longitudes <- especies_coincidentes$decimalLongitude

# Crear un dataframe con las ubicaciones
ubicaciones <- data.frame(especie = especies_coincidentes$scientificName, lat = latitudes, lon = longitudes)

  # Crear el mapa utilizando Plotly
mapa <- plot_geo(ubicaciones) %>%
  add_markers(
    x = ~lon,
    y = ~lat,
    text = ~especie,
    marker = list(
      size = 10,
      color = "rgb(0, 116, 217)",
      opacity = 0.7,
      line = list(color = "rgb(231, 99, 250)", width = 1)
    )
  ) %>%
  layout(
    title = "Especies End�micas en Colombia",
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(245, 245, 245)",
      countrycolor = "rgb(0, 0, 0)",  # Cambio del color a negro (rgb(0, 0, 0))
      coastlinewidth = 1,
      lataxis = list(range = c(0, 13)),  # Rango de latitud visible (ajustado para Colombia)
      lonaxis = list(range = c(-82, -66)),  # Rango de longitud visible (ajustado para Colombia)
      countrywidth = 1.5,
      countryhoverlabel = list(
        font = list(size = 12)
      ),
      showcountries = TRUE  # Mostrar l�mites de pa�ses
    ),
    margin = list(l = 50, r = 50, b = 50, t = 80)
  )

# Imprimir el mapa
ggplotly(mapa)




# PLANTAS ENDEMICAS ----

nombre_planta_endemica <- unique(Plantas_Endemicas$Nombre)

coincidencia_planta <- intersect(nombre_cientificos, nombre_planta_endemica)

# Filtrar las especies end�micas presentes en las aves observadas
especies_coincidentes <- df_clean %>%
  filter(scientificName %in% nombres_coincidentes)

unique(especies_coincidentes$scientificName)

latitudes <- especies_coincidentes$decimalLatitude
longitudes <- especies_coincidentes$decimalLongitude

# Crear un dataframe con las ubicaciones
ubicaciones <- data.frame(especie = especies_coincidentes$scientificName, lat = latitudes, lon = longitudes)

# Crear el mapa utilizando Plotly
mapa <- plot_geo(ubicaciones) %>%
  add_markers(
    x = ~lon,
    y = ~lat,
    text = ~especie,
    marker = list(
      size = 10,
      color = "rgb(0, 116, 217)",
      opacity = 0.7,
      line = list(color = "rgb(231, 99, 250)", width = 1)
    )
  ) %>%
  layout(
    title = "Especies End�micas en Colombia",
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(245, 245, 245)",
      countrycolor = "rgb(0, 0, 0)",  # Cambio del color a negro (rgb(0, 0, 0))
      coastlinewidth = 1,
      lataxis = list(range = c(0, 13)),  # Rango de latitud visible (ajustado para Colombia)
      lonaxis = list(range = c(-82, -66)),  # Rango de longitud visible (ajustado para Colombia)
      countrywidth = 1.5,
      countryhoverlabel = list(
        font = list(size = 12)
      ),
      showcountries = TRUE  # Mostrar l�mites de pa�ses
    ),
    margin = list(l = 50, r = 50, b = 50, t = 80)
  )

# Imprimir el mapa
ggplotly(mapa)


grouped_data <- df_clean %>%
  group_by(decimalLatitude, decimalLongitude, eventDate) %>%
  summarize(num_species = n_distinct(scientificName))

change_data <- grouped_data %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  mutate(num_species_diff = num_species - lag(num_species, default = num_species[1]))

plot_ly(change_data, x = ~eventDate, y = ~num_species_diff, type = 'scatter', mode = 'lines+markers',
             color = ~decimalLatitude, colors = 'Blues', marker = list(size = 5), line = list(width = 1)) %>%
  layout(
    xaxis = list(title = 'Fecha', range = c(min(change_data$eventDate), max(change_data$eventDate))),
    yaxis = list(title = 'Cambio en el n�mero de especies encontradas'),
    title = 'N�mero de especies encontradas por ubicaci�n y tiempo',
    showlegend = FALSE
  )



riqueza_especies <- length(unique(df_clean$species))

# Calcular �ndice de diversidad de Shannon
proporciones <- table(df_clean$species) / length(df_clean$species)
shannon <- -sum(proporciones * log(proporciones, base = exp(1)))

# Calcular matriz de presencia-ausencia
data_presencia <- table(df_clean$species)
presence_matrix <- matrix(as.numeric(data_presencia > 0), nrow = 1)

# Calcular matriz de distancia de Jaccard
jaccard_matrix <- vegdist(presence_matrix, method = "jaccard")

# Calcular �ndice de diversidad de Jaccard
jaccard_diversity <- mean(jaccard_matrix)

# Imprimir los resultados
cat("Riqueza de especies:", riqueza_especies, "\n")
cat("�ndice de diversidad de Shannon:", shannon, "\n")
cat("�ndice de diversidad de Jaccard:", jaccard_diversity, "\n")

df_clean$eventDate <- as.Date(df_clean$eventDate)
df_clean$genus <- as.factor(df_clean$genus)  # Convertir a factor para an�lisis taxon�mico

# Crear una serie temporal a partir de los datos
serie_temporal <- ts(df_clean$genus, start = min(df_clean$eventDate), frequency = 12)

# Ajustar el modelo ARIMA
modelo <- auto.arima(serie_temporal)


forecast_values <- forecast(modelo, h = 12)  # Pronosticar 12 per�odos hacia adelante
plot(forecast_values, main = "Pron�stico de variable taxon�mica - Genus")