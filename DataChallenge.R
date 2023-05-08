# Instalar paquetes y cargarlos
library(rgbif)
library(vegan)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(nortest)
library(rredlist)
library(leaflet)
library(ggmap)


# Buscar un conjuntos de datos
# En este caso vamos a buscar los datos subidos por promigas
orgs<-organizations(country="CO",limit=60)
promigas<-orgs$data$key[grep("Promigas",orgs$data$title)]

# Dentro de los datos subidos por promigas hay varios conjuntos de datos
promigas_dt<-dataset_search(publishingOrg = promigas)
df_pro <- promigas_dt$data
# Definir función para obtener los datos de un conjunto de datos
get_occurrence_data <- function(dataset_key) {
  # Obtener la información del conjunto de datos
  dtset_info <- datasets(data = "all", type = "occurrence", uuid = dataset_key)
  # Devolver los datos como un data.frame
  return(dtset_info)
}

# Identificadores de los conjuntos de datos disponibles
dataset_keys <- c(df_pro$datasetKey[1],df_pro$datasetKey[2],df_pro$datasetKey[3],df_pro$datasetKey[4]
                  ,df_pro$datasetKey[5], df_pro$datasetKey[6], df_pro$datasetKey[7], df_pro$datasetKey[8]
                  ,df_pro$datasetKey[9], df_pro$datasetKey[10], df_pro$datasetKey[11])



dtset<-occ_download(pred("datasetKey",dataset_keys[4]),user="juanes200",pwd = "juanalbis200",email="albisj@uninorte.edu.co")

occ_download_wait('0215987-230224095556074')

d_MP <- occ_download_get('0215987-230224095556074') %>%
  occ_download_import()

eliminar_var_nula <- function(datos) {
  for (col in colnames(datos)) {
    if (all(is.na(datos[[col]]))) {
      datos <- datos[, -which(names(datos) == col)]
    }
  }
  return(datos)
}

no_null_df <- eliminar_var_nula(d_MP)

names(no_null_df)

df_clean <- subset(no_null_df, select = -c(datasetID, institutionID, datasetName
                                           ,occurrenceID, countryCode,publishingCountry
                                           ,level0Gid, level0Name, type))

unique(df_clean$kingdom)

grouped_data <- df_clean %>%
  group_by(decimalLatitude, decimalLongitude, eventDate) %>%
  summarize(num_species = n_distinct(scientificName))

# Calcule el cambio en el número de especies encontradas por cada ubicación a lo largo del tiempo
change_data <- grouped_data %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  mutate(num_species_diff = num_species - lag(num_species, default = num_species[1]))

ggplot(change_data, aes(x = eventDate, y = num_species_diff)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Date") +
  ylab("Change in number of species found")

# Group by location and date, and count number of occurrences of each species in each group
grouped_data <- df_clean %>%
  group_by(decimalLatitude, decimalLongitude, eventDate, scientificName) %>%
  summarize(num_occurrences = n())

# Calculate the change in occurrence of each species over time for each location
change_data<- grouped_data %>%
  group_by(decimalLatitude, decimalLongitude, scientificName) %>%
  mutate(num_occurrences_diff = num_occurrences - lag(num_occurrences, default = num_occurrences[1]))

# Plot the change in occurrence of a particular species over time for each location
ggplot(change_data, aes(x = eventDate, y = num_occurrences_diff)) +
  geom_point() +
  geom_line() +
  theme_bw()

# ENDEMICAS Y RIESGO ----

# Obtener información sobre la categoría de amenaza y distribución de las especies
iucn_data <- df_clean %>%
  select(scientificName, iucnRedListCategory, stateProvince) %>%
  na.omit() %>%
  distinct()

# Contar el número de especies en cada categoría de amenaza
iucn_counts <- iucn_data %>%
  group_by(iucnRedListCategory) %>%
  summarize(num_species = n_distinct(scientificName)) %>%
  slice(-1)

# Mostrar el número de especies en cada categoría de amenaza
iucn_counts

# Gráfico de barras para el número de especies en cada categoría de amenaza
ggplot(iucn_counts, aes(x = iucnRedListCategory, y = num_species, fill = iucnRedListCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de especies en cada categoría de amenaza", x = "Categoría de amenaza", y = "Número de especies")

endemic <- Aves_Endemicas_Colombia_Tablas_Anexos_1_ %>%
  filter(Cat. == 'E')



