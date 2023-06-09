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



#______________________________________
library(shiny)

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Prueba de hipótesis"),
  mainPanel(
    verbatimTextOutput("resultado_plantae"),
    textOutput("texto_plantae"),
    verbatimTextOutput("resultado_animalia"),
    textOutput("texto_animalia"),
    verbatimTextOutput("resultado_bacteria"),
    textOutput("texto_bacteria"),
    verbatimTextOutput("resultado_protozoa"),
    textOutput("texto_protozoa"),
    verbatimTextOutput("resultado_chromista"),
    textOutput("texto_chromista")
  )
)

# Definir el servidor (Server)
server <- function(input, output) {
  # Realizar la prueba de proporciones para Plantae
  resultadoplantae <- prop.test(
    x = c(45843, 10343), # Número de éxitos en cada dataset
    n = c(48353, 17629), # Número total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para Plantae
  output$resultado_plantae <- renderPrint({
    resultadoplantae
  })
  
  # Mostrar el texto para Plantae
  output$texto_plantae <- renderText({
    "Sí hay diferencia significativa entre la proporción Plantae entre la biodiversidad del territorio
    y la compensación, por lo tanto la compensación de plantas no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para Animalia
  resultadoanimalia <- prop.test(
    x = c(2329, 724), # Número de éxitos en cada dataset
    n = c(48353, 17629), # Número total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para Animalia
  output$resultado_animalia <- renderPrint({
    resultadoanimalia
  })
  
  # Mostrar el texto para Animalia
  output$texto_animalia <- renderText({
    "Sí hay diferencia significativa entre la proporción Animalia entre la biodiversidad del territorio
    y la compensación, por lo tanto la compensación subsecuente de animales no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para bacteria
  resultadobacteria <- prop.test(
    x = c(17, 17), # Número de éxitos en cada dataset
    n = c(48353, 17629), # Número total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para bacteria
  output$resultado_bacteria <- renderPrint({
    resultadobacteria
  })
  
  # Mostrar el texto para bacteria
  output$texto_bacteria <- renderText({
    "Sí hay diferencia significativa entre la proporción de bacterias entre la biodiversidad del territorio
    y la compensación, por lo tanto la presencia de bacterias es menor a la debida en el territorio compensado."
  })
  
  # Realizar la prueba de proporciones para protozoa
  resultadoprotozoa <- prop.test(
    x = c(51, 51), # Número de éxitos en cada dataset
    n = c(48353, 17629), # Número total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para protozoa
  output$resultado_protozoa <- renderPrint({
    resultadoprotozoa
  })
  
  # Mostrar el texto para protozoa
  output$texto_protozoa <- renderText({
    "Sí hay diferencia significativa entre la proporción Protozoa entre la biodiversidad del territorio
    y la compensación, por lo tanto la compensación protozoos no fue suficiente."
  })
  
  # Realizar la prueba de proporciones para chromista
  resultadochromista <- prop.test(
    x = c(113, 113), # Número de éxitos en cada dataset
    n = c(48353, 17629), # Número total de observaciones en cada dataset
    alternative = "two.sided" # Alternativa de dos colas
  )
  
  # Mostrar el resultado de la prueba para chromista
  output$resultado_chromista <- renderPrint({
    resultadochromista
  })
  
  # Mostrar el texto para chromista
  output$texto_chromista <- renderText({
    "Sí hay diferencia significativa entre la proporción Chromista entre la biodiversidad del territorio
    y la compensación, por lo tanto la compensación de chromista no fue suficiente."
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
