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
resultadoplantae2 <- prop.test(
  x = c(6829  , 3031), # Número de éxitos en cada dataset
  n = c(64933, 17745), # Número total de observaciones en cada dataset
  alternative = "two.sided" # Alternativa de dos colas
)

# Mostrar el resultado de la prueba
print(resultadoplantae2)
