#Gráficos finales para el tablero de visualización 

#librerias
library(ggplot2)
library(treemap)
library(d3treeR)
library(plotly)
library(packcircles)
library(viridis)
library(ggiraph)

#solo correr esta línea de codigo si tiene problema con la instalacion del paquete
#d3treeR

#devtools::install_github("d3treeR/d3treeR")

#La base de datos usada, es el conjunto de datos publicados por promigas en GBIF
#Biodiversidad asociada al gasoducto Paiva-Caracolí

##### Grafica #1 ####

paleta<-c("#85B84A","#408c4c","#29907D","#4e8855","#b8c290","#B3C93A")

#Conteo de registros por mes
plot_ly(X04_PaivaCaracolí, x = ~month) %>%
  add_histogram(marker = list(color = "#408c4c", opacity = 0.7), 
                hovertemplate = "Mes: %{x}<br>Registros: %{y}") %>%
  layout(xaxis = list(title = "Mes"), yaxis = list(title = "Conteo de registros"), 
         title = "Conteo de Registros por Mes")

##### Grafica #2 ####

#Distribucion de especie por reinos

reino_04<-X04_PaivaCaracolí$kingdom
clase_04<-X04_PaivaCaracolí$class

valor_04<-c(rep(1,length(X04_PaivaCaracolí$kingdom)))
data_04<-data.frame(reino_04,clase_04,valor_04)

prueba_04 <- treemap(data_04,
                     index=c("reino_04","clase_04"),
                     vSize="valor_04",
                     type="index",
                     palette = paleta,
                     bg.labels=c("white"),
                     align.labels=list(
                       c("center", "center"), 
                       c("right", "bottom")
                     )
) 

inter_04 <- d3tree2( prueba_04 ,  rootname = "Reinos" )
inter_04

##### Grafica #3 ####

#Numero de especies encontradas a lo largo del tiempo

plot_ly(X04_PaivaCaracolí, x = X04_PaivaCaracolí$eventDate, type = "histogram",marker = list(color = "#408c4c", opacity = 0.7)) %>%
  layout(xaxis = list(title = "Fecha"), yaxis = list(title = "N?mero de especies"), 
         title = "N?mero de especies encontradas a lo largo del tiempo")

##### Gráfica #4 ####

#Gráfico 10 especies mas comunes

species_counts <- table(X04_PaivaCaracolí$scientificName)
top_species <- head(sort(species_counts, decreasing = TRUE), 10)
df_top_species <- X04_PaivaCaracolí[X04_PaivaCaracolí$scientificName %in% names(top_species), ]

data_p<-data.frame(top_species)
colnames(data_p)<-c("Nombre","valor")

# Generate the layout
packing3 <- circleProgressiveLayout(data_p$valor, sizetype='area')
data_p <- cbind(data_p, packing3)
dat.gg3 <- circleLayoutVertices(packing3, npoints=50)

# Make the plot with a few differences compared to the static version:
pp <- ggplot() + 
  geom_polygon_interactive(data = dat.gg3, aes(x, y, group = id, fill=id, tooltip = data_p$Nombre[id], 
  data_id = id),color = "black", alpha = 0.6) + scale_fill_viridis() +
  geom_text(data = data_p, aes(x, y, label = Nombre), size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()

# Turn it interactive
widg3 <- girafe(ggobj = pp, width_svg = 7, height_svg = 7)
widg3
