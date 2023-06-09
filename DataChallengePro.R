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
library(nnet)
library(pROC)
library(treemap)
library(d3treeR)
library(plotly)
library(packcircles)
library(viridis)
library(ggiraph)
library(mlbench)
library(ggcorrplot)
library(ggpubr)
library(car)
library(broom)
library(gridExtra)
library(lmtest)


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

# ##########################################
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


filtered_classes <- species_counts %>%
  filter(num_species > 500)


filtered_classes$class <- ifelse(nchar(filtered_classes$class) > 20, substr(filtered_classes$class, 1, 20), filtered_classes$class)


ggplot(filtered_classes, aes(x = reorder(class, num_species), y = num_species, fill = kingdom)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = num_species), vjust = -0.5, color = "black", size = 3.5) + 
  scale_fill_manual(values = paleta) + 
  labs(x = "Clase", y = "N�mero de especies", fill = "Reino") +
  ggtitle("N�mero de especies por clase en Animalia y Plantae") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom")



# ENDEMICAS Y RIESGO ----


iucn_data <- df_clean %>%
  select(scientificName, iucnRedListCategory, stateProvince) %>%
  na.omit() %>%
  distinct()

iucn_counts <- iucn_data %>%
  group_by(iucnRedListCategory) %>%
  summarize(num_species = n_distinct(scientificName)) %>%
  slice(-1)

# Mostrar el n�mero de especies en cada categor�a de amenaza
iucn_counts

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


df_unique <- df_clean[order(df_clean$scientificName), ]

unique(df_unique$scientificName)

endemic <- Aves_Endemicas %>%
  filter(Cat. == 'E')

nombre_cientificos <- unique(df_unique$scientificName)
nombre_endemica <- unique(endemic$`Nombre cient�fico`)

coincidencia <- intersect(nombre_cientificos, nombre_endemica)

especies_coincidentes <- df_clean %>%
  filter(scientificName %in% coincidencia)

latitudes <- especies_coincidentes$decimalLatitude
longitudes <- especies_coincidentes$decimalLongitude


ubicaciones <- data.frame(especie = especies_coincidentes$scientificName, lat = latitudes, lon = longitudes)

colombia_bounds <- list(
  lon = c(-79, -66),
  lat = c(-5, 13)
)

mapa <- plot_ly(ubicaciones, x = ~lon, y = ~lat, type = "scattermapbox",
                mode = "markers", marker = list(color = "green", opacity = 0.7)) %>%
  layout(mapbox = list(style = "carto-positron",
                       zoom = 5,
                       center = list(lon = -73, lat = 4),
                       bearing = 0,
                       pitch = 0,
                       bounds = colombia_bounds),
         title = "Especies End�micas en Colombia")

mapa







# PLANTAS ENDEMICAS ----

nombre_planta_endemica <- unique(Plantas_Endemicas$Nombre)

coincidencia_planta <- intersect(nombre_cientificos, nombre_planta_endemica)


especies_coincidentes <- df_clean %>%
  filter(scientificName %in% nombres_coincidentes)

unique(especies_coincidentes$scientificName)

latitudes <- especies_coincidentes$decimalLatitude
longitudes <- especies_coincidentes$decimalLongitude


ubicaciones <- data.frame(especie = especies_coincidentes$scientificName, lat = latitudes, lon = longitudes)


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
      countrycolor = "rgb(0, 0, 0)",  
      coastlinewidth = 1,
      lataxis = list(range = c(0, 13)),  # Rango de latitud visible (ajustado para Colombia)
      lonaxis = list(range = c(-82, -66)),  # Rango de longitud visible (ajustado para Colombia)
      countrywidth = 1.5,
      countryhoverlabel = list(
        font = list(size = 12)
      ),
      showcountries = TRUE  
    ),
    margin = list(l = 50, r = 50, b = 50, t = 80)
  )


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


# COMEPNSACION PRUEBAS DE HIPOTESIS ----
avesendemicas <- read_excel("C:/Users/pc/Downloads/avesendemicas.xlsx")
plantas_endemicas <- read_excel("C:/Users/pc/Downloads/Plantas_Endemicas (1).xlsx")


reinos <- unique(com_mamonal_paiva$kingdom)
reinos2 <- unique(bio_mamonal_paiva$kingdom)
print(reinos)
print(reinos2)

Animalia <- com_mamonal_paiva[com_mamonal_paiva$kingdom == "Animalia",]
Plantae <- com_mamonal_paiva[com_mamonal_paiva$kingdom == "Plantae" , ]
chromista <- com_mamonal_paiva[com_mamonal_paiva$kingdom == "Chromista" , ]
Protozoa <- com_mamonal_paiva[com_mamonal_paiva$kingdom == "Protozoa" , ]
Bacteria <- com_mamonal_paiva[com_mamonal_paiva$kingdom == "Bacteria" , ]
Fungi <- com_mamonal_paiva[com_mamonal_paiva$kingdom == "Fungi" , ]

Animalia2 <- bio_mamonal_paiva[bio_mamonal_paiva$kingdom == "Animalia",]
Plantae2 <- bio_mamonal_paiva[bio_mamonal_paiva$kingdom == "Plantae",]
chromista2 <- bio_mamonal_paiva[bio_mamonal_paiva$kingdom == "Chromista" , ]
Protozoa2 <- bio_mamonal_paiva[bio_mamonal_paiva$kingdom == "Protozoa" , ]
Bacteria2 <- bio_mamonal_paiva[bio_mamonal_paiva$kingdom == "Bacteria" , ]
Fungi2 <- bio_mamonal_paiva[bio_mamonal_paiva$kingdom == "Fungi" , ]

#Proporción de cada Reino en cada Dataset
proporciones <- prop.table(table(com_mamonal_paiva$kingdom))
print(proporciones)
proporciones2 <- prop.table(table(bio_mamonal_paiva$kingdom))
print(proporciones2)

#Eliminar Fungi de bio diversidad de mamonal para comprarla con la compensación
nuevo_bio_mamonal_paiva <- subset(bio_mamonal_paiva, kingdom != "Fungi")  
#print(names(bio_mamonal_paiva))


proporciones2 <- prop.table(table(nuevo_bio_mamonal_paiva$kingdom))
print(proporciones2)

#Comprar las proporciones de la biodiversidad y la compensacion de la variable reino
niveles_dataset1 <- levels(factor(com_mamonal_paiva$kingdom))
niveles_dataset2 <- levels(factor(nuevo_bio_mamonal_paiva$kingdom))

#Transforma en levels para agregar 0 al fungi del dataset 1 que no tiene observaciones                                                      
tabla_comparativa <- data.frame(
  Comp = ifelse(niveles_dataset1 %in% niveles_dataset2, proporciones, 0),
  Bio = ifelse(niveles_dataset2 %in% niveles_dataset1, proporciones2, 0)
)
nombres_categorias_dataset1 <- c("Animalia", "Bacteria", "Chromista", "Plantae", "Protozoa")
rownames(tabla_comparativa) <- nombres_categorias_dataset1
print(tabla_comparativa) 


#Hipótesis para comparar las proporciones de los reinos, dataset1: compensasion, dataset2: biodiversidad
#HA: Hay signifucativa diferencia entre la proporción de plantas que se sembraron
#HN: No hay diferencia entre la proporcion de plantas que se compensaron

# Realizar la prueba de proporciones para Plantae
resultadoplantae <- prop.test(
  x = c(45843, 10343), # Número de éxitos en cada dataset
  n = c(48353, 17629), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadoplantae)

#Hipótesis para comparar las proporciones de los reinos, dataset1: compensasion, dataset2: biodiversidad
#HA: Hay diferencia entre la proporción de Animales observados
#HN: No hay diferencia entre la proporcion de Animales que se compensaron

# Realizar la prueba de proporciones para Animalia
resultadoanimalia <- prop.test(
  x = c(2329  , 724), # Número de éxitos en cada dataset
  n = c(48353, 17629), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadoanimalia)

#Hipótesis para comparar las proporciones de los reinos, dataset1: compensasion, dataset2: biodiversidad
#HA: Hay diferencia entre la proporción de bacterias observados
#HN: No hay diferencia entre la proporcion de bascterias que se compensaron

# Realizar la prueba de proporciones para bacteria
resultadobacteria <- prop.test(
  x = c(17  , 17), # Número de éxitos en cada dataset
  n = c(48353, 17629), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadobacteria)


#Hipótesis para comparar las proporciones de los reinos, dataset1: compensasion, dataset2: biodiversidad
#HA: Hay diferencia entre la proporción de protistas observados
#HN: No hay diferencia entre la proporcion de protistas que se compensaron

# Realizar la prueba de proporciones para protozoa
resultadoprotozoa <- prop.test(
  x = c(51  , 51), # Número de éxitos en cada dataset
  n = c(48353, 17629), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadoprotozoa)

#Hipótesis para comparar las proporciones de los reinos, dataset1: compensasion, dataset2: biodiversidad
#HA: Hay diferencia entre la proporción de chromista observados
#HN: No hay diferencia entre la proporcion de chor,ista que se compensaron

# Realizar la prueba de proporciones para chromista
resultadochromista <- prop.test(
  x = c(113  , 113), # Número de éxitos en cada dataset
  n = c(48353, 17629), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadochromista)

#Cruce de variablaes ara escala UICN
column_names_aves <- colnames(avesendemicas)
print(column_names_aves)
column_names_plantas <- colnames(plantas_endemicas)
print(column_names_plantas)

#Numero de registros no faltasntes en cada columna
num_registros_aves <- colSums(!is.na(avesendemicas))
print(num_registros_aves)
num_registros_plantas <- colSums(!is.na(plantas_endemicas))
print(num_registros_plantas)

# Obtener la longitud de la columna del primer dataset de comp reino animalia para endemicas
longitud_columna <- nrow(Animalia)

# Crear un vector para almacenar los valores coincidentes
valores_coincidentes <- vector()

# Comparar los valores de la columna del primer dataset con las columnas del segundo dataset
for (i in 1:ncol(avesendemicas)) {
  columna_actual <- avesendemicas[, i]
  
  # Verificar si los valores coinciden
  valores_coincidentes <- c(valores_coincidentes, Animalia$columna_deseada[Animalia$columna_deseada %in% columna_actual])
}

# Mostrar los valores coincidentes
print(valores_coincidentes)
#Logical(0) por lo que no hay pajaros endemicos en el regustro de compensacion

# Obtener la longitud de la columna del primer dataset de comp reino animalia para endemicas
longitud_columna2 <- nrow(Animalia2)

# Crear un vector para almacenar los valores coincidentes
valores_coincidentes2 <- vector()

# Comparar los valores de la columna del primer dataset con las columnas del segundo dataset
for (i in 1:ncol(avesendemicas)) {
  columna_actual2 <- avesendemicas[, i]
  
  # Verificar si los valores coinciden
  valores_coincidentes2 <- c(valores_coincidentes2, Animalia2$columna_deseada[Animalia2$columna_deseada %in% columna_actual2])
}

# Mostrar los valores coincidentes
print(valores_coincidentes2)
#Logical(0) por loq eu tampoco hay aves endemicas en el dataset de biodiversidad

# Obtener la longitud de la columna del primer dataset de comp reino animalia para endemicas
longitud_columna3 <- nrow(Plantae)

# Crear un vector para almacenar los valores coincidentes
valores_coincidentes3 <- vector()

# Comparar los valores de la columna del primer dataset con las columnas del segundo dataset
for (i in 1:ncol(plantas_endemicas)) {
  columna_actual3 <- plantas_endemicas[, i]
  
  # Verificar si los valores coinciden
  valores_coincidentes3 <- c(valores_coincidentes3, Plantae$columna_deseada[Plantae$columna_deseada %in% columna_actual3])
}

# Mostrar los valores coincidentes
print(valores_coincidentes3)
#No Hay conincidencia de plantas endemicas


# Obtener la longitud de la columna del primer dataset de comp reino animalia para endemicas
longitud_columna4 <- nrow(Plantae2)

# Crear un vector para almacenar los valores coincidentes
valores_coincidentes4 <- vector()

# Comparar los valores de la columna del primer dataset con las columnas del segundo dataset
for (i in 1:ncol(plantas_endemicas)) {
  columna_actual4 <- plantas_endemicas[, i]
  
  # Verificar si los valores coinciden
  valores_coincidentes4 <- c(valores_coincidentes4, Plantae2$columna_deseada[Plantae2$columna_deseada %in% columna_actual4])
}

# Mostrar los valores coincidentes
print(valores_coincidentes4)
#No hay plantas endemicas 

# COMPENSACION 2 ----

#---------------------------------------
#Otros dataset
#1.Monitoreo de compensacion de perdida de biodiversidad en el gasoducto loop san-mateo
#2. Biodiversidad loop san mateo-mamonal

reinos3 <- unique(monitoreo_sanmateo$kingdom)
print(reinos3)
reinos4 <- unique(bio_sanmateo_mamonal$kingdom)
print(reinos4)

Animalia3 <- monitoreo_sanmateo[monitoreo_sanmateo$kingdom == "Animalia",]
Plantae3 <- monitoreo_sanmateo[monitoreo_sanmateo$kingdom == "Plantae" , ]

Animalia4 <- bio_sanmateo_mamonal[bio_sanmateo_mamonal$kingdom == "Animalia",]
Plantae4 <- bio_sanmateo_mamonal[bio_sanmateo_mamonal$kingdom == "Plantae" , ]
Fungi <- bio_sanmateo_mamonal[bio_sanmateo_mamonal$kingdom=="Fungi", ]
Chromista4 <- bio_sanmateo_mamonal[bio_sanmateo_mamonal$kingdom=="Chromista", ]
Bacteria4 <- bio_sanmateo_mamonal[bio_sanmateo_mamonal$kingdom=="Bacteria", ]
Protozoa4 <- bio_sanmateo_mamonal[bio_sanmateo_mamonal$kingdom=="Protozoa", ]

#Proporción de cada Reino en cada Dataset
proporciones3 <- prop.table(table(monitoreo_sanmateo$kingdom))
print(proporciones3)

proporciones4 <- prop.table(table(bio_sanmateo_mamonal$kingdom))
print(proporciones4)

#Eliminar Fungi, bacteria, chromista y protozoa de biodiversidad de sanmateo-mamonal para comprarla con la compensación
nuevo_bio_sanmateomamonal <- subset(bio_sanmateo_mamonal, kingdom != "Fungi" 
                                    & kingdom != "Bacteria" & kingdom != "Chromista" 
                                    & kingdom != "Protozoa")



proporciones4 <- prop.table(table(nuevo_bio_sanmateomamonal$kingdom))
print(proporciones4)

#Comprar las proporciones de la biodiversidad y la compensacion de la variable reino
niveles_dataset3 <- levels(factor(monitoreo_sanmateo$kingdom))
niveles_dataset4 <- levels(factor(nuevo_bio_sanmateomamonal$kingdom))

#Transforma en levels para agregar 0 al fungi del dataset 1 que no tiene observaciones                                                      
tabla_comparativa2 <- data.frame(
  Comp3 = ifelse(niveles_dataset3 %in% niveles_dataset4, proporciones3, 0),
  Bio4 = ifelse(niveles_dataset4 %in% niveles_dataset3, proporciones4, 0)
)
nombres_categorias_dataset3 <- c("Animalia", "Plantae")
rownames(tabla_comparativa2) <- nombres_categorias_dataset3
print(tabla_comparativa2) 

#Prueba de hipotesis para proporcion de reinos
#Hay diferencia significativa entre la proporcion de plantas de la biodiversidad versus la compensacion
#No hay diferencia significativa entre la proporcion de plantas de la biodiversidad versus la compensacion
resultadoplantae2 <- prop.test(
  x = c(58104  , 14714), # Número de éxitos en cada dataset
  n = c(64933, 17745), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadoplantae2)

#Hay diferencia significativa entre la proporcion de animalia de la biodiversidad versus la compensacion
#No hay diferencia significativa entre la proporcion de animalia de la biodiversidad versus la compensacion
resultadoanimalia2 <- prop.test(
  x = c(6829  , 3031), # Número de éxitos en cada dataset
  n = c(64933, 17745), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadoanimalia2)

# MODELO ----

Tabla <- df_clean %>%
  dplyr::group_by(iucnRedListCategory) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::filter(iucnRedListCategory != "") %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 2))
Tabla

p <- ggplot(Tabla, aes(x = factor(iucnRedListCategory), y = Total, fill = factor(iucnRedListCategory))) +
  geom_bar(width = 0.7, stat = "identity", position = position_dodge()) +
  geom_text(aes(label = paste0(Total, " ", "", "\n(", Porcentaje, "%)"))) +
  ylim(c(0, max(Tabla$Total))) +
  labs(x = "Nivel de riesgo", y = "Frecuencias") +
  labs(fill = "") +
  facet_wrap(~"Distribuci�n del tipo de programa") +
  theme_bw(base_size = 13) +
  scale_fill_manual(values = paleta)

ggplotly(p)

Tabla2 <- df_clean %>%
  dplyr::group_by(iucnRedListCategory, habitat, elevation, kingdom, phylum, class, order, taxonRank, species, locality) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 2)) %>%
  dplyr::arrange(iucnRedListCategory)
Tabla2

###################  Preprocesamiento ############################## 


posible_vars <- df_clean[, c('elevation','kingdom', 'phylum', 'class', 'order', 'taxonRank','locality')]

df_clean$iucnRedListCategory <- as.factor(df_clean$iucnRedListCategory)
df_clean$iucnRedListCategory <- droplevels(df_clean$iucnRedListCategory, exclude = "")

posible_vars_complete <- na.omit(posible_vars)


posible_vars_complete$kingdom <- as.factor(posible_vars_complete$kingdom)
posible_vars_complete$phylum <- as.factor(posible_vars_complete$phylum)
posible_vars_complete$class <- as.factor(posible_vars_complete$class)
posible_vars_complete$order <- as.factor(posible_vars_complete$order)
posible_vars_complete$taxonRank <- as.factor(posible_vars_complete$taxonRank)
posible_vars_complete$locality <- as.factor(posible_vars_complete$locality)

datos_filtrados <- df_clean[rownames(df_clean) %in% rownames(posible_vars_complete), ]
var_obj <- as.factor(datos_filtrados$iucnRedListCategory)

lillie.test(posible_vars_complete$elevation)

table(posible_vars_complete$kingdom)
barplot(table(posible_vars_complete$kingdom), col = paleta)

table(posible_vars_complete$phylum)
barplot(table(posible_vars_complete$phylum), col= paleta)

table(posible_vars_complete$class)
barplot(table(posible_vars_complete$class), col = paleta)

table(posible_vars_complete$order)
barplot(table(posible_vars_complete$order), col= paleta)

table(posible_vars_complete$taxonRank)
barplot(table(posible_vars_complete$taxonRank), col = paleta)

table(posible_vars_complete$locality)
barplot(table(posible_vars_complete$locality), col = paleta)

chisq.test(posible_vars_complete$kingdom, var_obj)
chisq.test(posible_vars_complete$phylum, var_obj)
chisq.test(posible_vars_complete$class, var_obj)
chisq.test(posible_vars_complete$order, var_obj)
chisq.test(posible_vars_complete$taxonRank, var_obj)
chisq.test(posible_vars_complete$locality, var_obj)

# Todas las variables sugieren que hay una asociacion estad�sticamente significativa con la variable Iucn



plot_ly(data = posible_vars_complete, x = ~kingdom, color = ~var_obj, colors = paleta) %>%
  layout(
    title = "Distribuci�n de la variable 'kingdom'",
    xaxis = list(title = "Kingdom"),
    yaxis = list(title = "Count")
  )

plot_ly(data = posible_vars_complete, x = ~phylum, color = ~var_obj, colors = paleta) %>%
  layout(
    title = "Distribuci�n de la variable 'phylum'",
    xaxis = list(title = "Phylum"),
    yaxis = list(title = "Count")
  )

plot_ly(data = posible_vars_complete, x = ~class, color = ~var_obj, colors = paleta) %>%
  layout(
    title = "Distribuci�n de la variable 'class'",
    xaxis = list(title = "Class"),
    yaxis = list(title = "Count")
  )

plot_ly(data = posible_vars_complete, x = ~order, color = ~var_obj, colors = paleta) %>%
  layout(
    title = "Distribuci�n de la variable 'order'",
    xaxis = list(title = "Order"),
    yaxis = list(title = "Count")
  )

plot_ly(data = posible_vars_complete, x = ~taxonRank, color = ~var_obj, colors = paleta) %>%
  layout(
    title = "Distribuci�n de la variable 'taxonRank'",
    xaxis = list(title = "Taxon Rank"),
    yaxis = list(title = "Count")
  )

plot_ly(data = posible_vars_complete, x = ~locality, color = ~var_obj, colors = paleta) %>%
  layout(
    title = "Distribuci�n de la variable 'locality'",
    xaxis = list(title = "Locality"),
    yaxis = list(title = "Count")
  )


############# modelos ################# 
modelo_completo <- multinom(var_obj ~ ., data = posible_vars_complete)

modelo_seleccionado <- step(modelo_completo, direction = "both")
modelo_nulo <- multinom(var_obj ~ 1, data = posible_vars_complete)

anova(modelo_nulo, modelo_completo, test = "Chisq")
lrtest(modelo_completo, modelo_nulo)

exp(coef(modelo_seleccionado))

qqnorm(residuals(modelo_seleccionado))
qqline(residuals(modelo_seleccionado))
lillie.test(residuals(modelo_seleccionado))

# Obtener la importancia relativa de las variables
importance <- abs(coef(modelo_seleccionado))
importance_sorted <- importance[order(importance, decreasing = TRUE)]
barplot(importance_sorted, horiz = TRUE, las = 1)

# Realizar predicciones en los datos de prueba
predictions <- predict(modelo_seleccionado, newdata = posible_vars_complete, type = "class")
# Matriz de confusi�n
confusion_matrix <- table(predictions, var_obj)

# Exactitud (Accuracy)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

coeficientes <- coef(modelo_seleccionado)
barplot(coeficientes, beside = TRUE, col = paleta)











