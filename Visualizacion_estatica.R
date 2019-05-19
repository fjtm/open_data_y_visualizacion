rm(list=ls())

library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(rgdal)
library(viridis) 
library(cowplot)
library(png)
library(grid)

# Carga de datos ----------------------------------------------------------

# Cargamos los datos correspondientes a la población 
population_male <- read.csv("Archivos_necesarios/table_prov_male_complete.csv", colClasses=c(rep("character",4),rep("numeric",22)))
population_female <- read.csv("Archivos_necesarios/table_prov_female_complete.csv", colClasses=c(rep("character",4),rep("numeric",22)))
population_male <- population_male[,-c(1,ncol(population_male))] 
population_female <- population_female[,-c(1,ncol(population_female))]

# Arreglamos los nombres

names(population_male)[-1] <- gsub("X","", names(population_male)[-1])
names(population_female)[-1] <- gsub("X","", names(population_female)[-1])

population_total <- cbind(population_male[,c(1,2,3)], population_male[,-c(1,2,3)] + population_female[,-c(1,2,3)])


# Primera visualización estática ------------------------------------------

# Función que normaliza min-max entre (0-1)

normalize <- function (col) {
  normalized = (col - min(col)) / (max(col) - min(col))
  return (normalized)
}

# Función que agrupa los municipios en función de la categoría (número de habitantes)
# que tenían en 1998 y los normaliza

standar_population <- function(df, max, min){
  population <- df %>%  
    filter(`1998` >= min, `1998` < max) %>% 
    summarise_all("sum") 
  population = normalize(population)
  return (population)
} 

# Normalizamos y agrupamos por habitantes

size = c(0,300,1000,50000,250000, 500000, 1500000, 10000000)
label = c("year","aldea",'villa','ciudad.pequeña','ciudad.medianas','metropolis.subregionales','metropolis.regionales','metropolis.nacionales')
population_by_size <- data.frame(names(population_total[,-c(1,2,3)]))
for (i in 1:length(label)){
  population <- standar_population(population_total[,-c(1,2,3)], size[i+1], size[i])
  population_by_size <- cbind(population_by_size, t(population)[,1])
}
rownames(population_by_size) <- NULL
population_by_size <- population_by_size[,-c(length(population_by_size))]
names(population_by_size) <- label

# Como la tendencia para las ciudad pequeña, ciudad medianas, metropolis subregionales',
# metropolis regionales y metropolis nacionales es muy similar se graficará solamente 
# una banda con un nivel de confianza. 

population_by_size_long <- melt(population_by_size, id.vars="year")

population_by_size_quan <- population_by_size_long %>% 
  filter(variable %in% c('ciudad.pequeña','ciudad.medianas','metropolis.subregionales','metropolis.regionales','metropolis.nacionales')) %>% 
  group_by(year) %>% 
  summarise(q_inf = mean(value) - qt(0.999, 5) * sd(value) / sqrt(n()),
            q_sup = mean(value) + qt(0.999, 5) * sd(value) / sqrt(n())) %>% 
  as.data.frame()

# Creamos variables para la leyenda del eje y del gráfico
y.labels <- c('Máxima','Mínima')
y.values <- c(1,0)
text <- c(' Ciudades Pequeñas : 1.000-50.000 \n Ciudades Medias : 50.000-250.000 \n Metrópolis Subregionales : 250.000-500.000  \n Metrópolis Regionales :  500.000-1.500.000 \n Metrópolis Nacionales : > 1.500.000')

# Añadimos la franja para las ciudades con más de 1.000 habitantes
gplot <- ggplot() + 
  geom_ribbon(data = population_by_size_quan, aes(x = year,  ymin = q_inf-0.01, ymax = q_sup+0.01, group = 1, fill = text), alpha = 0.5)

# Añadimos las lineas para las villas y aldeas

gplot <- gplot +
  geom_line(data = population_by_size, aes(x = year, y = aldea, group = 1),linetype="dashed", color = "black", size = 1, alpha = 1) +
  geom_point(data = population_by_size, aes(x = year, y = aldea, group = 1, col="Aldea : <300"), size = 3) +
  geom_line(data = population_by_size, aes(x = year, y = villa, group = 1), linetype="dashed", color = "black", size = 1, alpha = 1) +
  geom_point(data = population_by_size, aes(x = year, y = villa, group = 1, colour="Villa : 300-1.000"),size = 3) 

# Arreglamos los ejes (Falta rotar los valores del eje x)

gplot <- gplot +
  scale_y_continuous(breaks = y.values, labels = y.labels, name = "Población normalizada", expand = c(0.01, 0)) +
  scale_x_discrete(expand = c(0.01,0), name = "Año")

# Quitamos el fondo y giramos el eje x

gplot <- gplot +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))

# Añadimos el título
gplot <- gplot +
  ggtitle(label = "Evolución de la población por tamaño de municipio \n (1998-2018)") +
  theme(plot.title = element_text(size = 18, face = "bold", colour = "#3c3c3c"))

# Arreglamos la leyenda (posición, color, margenes...)
gplot <- gplot +
  theme(legend.position="bottom", legend.justification="center", legend.box = "horizont", legend.text=element_text(size=10)) +
  theme(legend.spacing.y = unit(-0.3, "cm"), legend.box.margin=margin(-17,0,-30,-200)) +
  scale_fill_manual(name="",values="#4ABDAC") +
  scale_colour_manual(name="",values=c("#FC4A1A","#F7B733"))  

# Añadimos una aclaración al gráfico
gplot <- gplot + 
  labs(caption="*Unidades de la leyenda en habitantes") + 
  theme(plot.caption = element_text(size = 10))
gplot

# Segunda visualización estática ---------------------------------------------------

# Filtramos los datos correspondientes a villas y aldeas (<1000 habitantes), para los años
# 1998 y 2012

# Para hombres
population_male_filter <- population_male %>% 
  select(provincia, `1998`,`2018`) %>% 
  filter(`1998`<= 1000) %>% 
  group_by(provincia) %>% 
  summarise(sum_1998_male = sum(`1998`),
            sum_2018_male = sum(`2018`))

# Para mujeres ( Se pone -sum para que las barras de las mujeres se pinten en sentido
# opuesto a las de los hombres)
population_female_filter <- population_female %>% 
  select(provincia, `1998`,`2018`) %>% 
  filter(`1998`<= 1000) %>% 
  group_by(provincia) %>% 
  summarise(sum_1998_female = -sum(`1998`),
            sum_2018_female = -sum(`2018`))

# Unimos los dos géneros

population_gender <- left_join(population_male_filter, population_female_filter, by = "provincia")

# Creamos una columna de colores que corresponden a si se han perdido o ganado habitantes entre
# 1998 y 2018

population_gender_complete <- population_gender %>% 
  mutate(color_male = ifelse(sum_1998_male > sum_2018_male, "red", "green"),
         color_female = ifelse(sum_1998_female < sum_2018_female, "red", "green"))

# Reestructuramos el dataframe para obtener una columna con las étiquetas para distinguir
# los hombres y las mujeres

population_1998_long <- gather(population_gender_complete, "gender_1998", "population_1998", c("sum_1998_male", "sum_1998_female") ) %>% select(provincia, color_male, color_female, gender_1998, population_1998)
population_2018_long <- gather(population_gender_complete, "gender_2018", "population_2018", c("sum_2018_male", "sum_2018_female") ) %>%  select(gender_2018, population_2018)

# Unimos los dataframes

population_pyramid <- cbind(population_1998_long, population_2018_long) 

# Creamos listas para reestructurar las barras según las regiones consideradas

Madrid_y_alrededores <- c("Madrid", "Segovia", "Ávila", "Toledo", "Guadalajara") 
Barcelona_y_alrededores <- c("Barcelona", "Tarragona", "Girona", "Lleida")
Costa_mediterranea <- c("Castellón/Castello", "Valencia/València",
                        "Alicante/Alacant", "Murcia","Almería", "Granada", "Málaga", "Cádiz", "Huelva") 
Costa_cantabrica <- c("Pontevedra","Coruña, A", "Lugo", "Asturias", "Cantabria","Bizkaia" ,"Gipuzkoa") 
Islas <- c("Santa Cruz de Tenerife", "Palmas, Las", "Balears, Illes") 
Interior <- c("Ourense","León","Palencia","Burgos","Zamora","Valladolid","Salamanca","Soria","Araba/Álava", "Rioja, La", "Navarra", "Huesca", "Zaragoza","Teruel","Cáceres",
              "Badajoz", "Ciudad Real", "Cuenca", "Albacete", "Sevilla", "Córdoba", "Jaén") 

# Ordenamos el dataframe en función de las agrupaciones anteriores

order_prov <- c(Madrid_y_alrededores, Barcelona_y_alrededores, Costa_mediterranea, Costa_cantabrica, Islas, Interior)
order_prov_fix <- factor( as.character(population_pyramid$provincia[1:50]), levels=order_prov )
population_pyramid[1:50,] <- population_pyramid[1:50,][order(order_prov_fix),]
population_pyramid[51:100,] <- population_pyramid[51:100,][order(order_prov_fix),]

population_pyramid$color <- c(population_pyramid$color_male[1:50], population_pyramid$color_female[51:100])
population_pyramid <- population_pyramid %>% select(provincia, gender_1998, gender_2018, color, population_1998, population_2018)
population_pyramid$provincia <- factor(population_pyramid$provincia, levels = order_prov )

# Realizamos el gráfico con ggplot2

# Graficamos las barras con diferente alpha para los años 1998 y 2018. Así en función de si
# la población aumenta o disminuye, se verá de colores diferentes

pyramid <- ggplot(population_pyramid) +
  geom_bar(aes(x = reorder(provincia, desc(provincia)), y = population_1998, group = gender_1998, fill = gender_1998),stat = "identity", width = 0.8) +
  geom_bar(aes(x = reorder(provincia, desc(provincia)), y = population_2018, group = gender_2018, fill = gender_2018),stat = "identity", width = 0.8, alpha = 0.75) +
  theme_classic() 

# Añadimos lineas negras para separar hombres de mujeres y los grupos hechos

pyramid <- pyramid +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=(50 -c(0, 5, 9, 18, 25, 28, 50))+0.525, linetype = "dotted") 

# Giramos el gráfico y añadimos los números correspondientes a la diferencia de población
# entre 1998 y 2018 para hombres y mujeres al lado de cada barra

pyramid <- pyramid +
  coord_flip() +
  geom_text(data = subset(population_pyramid, gender_1998 == "sum_1998_male"), 
            aes(provincia, apply(cbind(population_pyramid$population_1998[1:50],
                                       population_pyramid$population_2018[1:50]), 1, FUN=max), group=gender_1998, label=population_2018-population_1998), size = 3,
            hjust = -0.35, vjust = 0.25) +
  geom_text(data = subset(population_pyramid, gender_1998 == "sum_1998_female"), 
            aes(provincia, apply(cbind(population_pyramid$population_1998[51:100],
                                       population_pyramid$population_2018[51:100]), 1, FUN=min), group=gender_1998, label=population_1998-population_2018), size = 3,
            hjust = 1.2, vjust = 0.25) 

# Arreglamos los ejes (posición, límite...) y colores de las barras

pyramid <- pyramid + 
  scale_x_discrete(name = "", position = "top") +
  scale_y_continuous(name = "Población (Habitantes)", breaks = seq(-80000,80000, 10000), labels = abs(seq(-80000,80000, 10000)), 
                     limits = c(-130000,90000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
  theme(legend.position="left", legend.justification="center", legend.box = "horizont", legend.text=element_text(size=10)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), axis.title.x = element_text(hjust = 0.65, size = 14)) +
  scale_fill_manual(values=c("darkgreen","darkblue","#18FF00","#14EBEC"),
                    name="",
                    labels=c("M1998", "H1998","M2018", "H2018")) 

# Añadimos las anotaciones correspondientes a los grupos

pyramid <- pyramid +
  annotate("text", x = 48.2, y = -119000, label = "   Madrid y \n  alrededores", size = 5) +
  annotate("text", x = 43.7, y = -117000, label = "  Barcelona y \n alrededores", size = 5) +
  annotate("text", x = 37.2, y = -115000, label = "  Costa \n mediterránea", size = 5) +
  annotate("text", x = 29.2, y = -117000, label = "  Costa \n cantábrica", size = 5) +
  annotate("text", x = 24.2, y = -117000, label = "Islas", size = 5) +
  annotate("text", x = 12.2, y = -115000, label = "Zona interior", size = 5)

# Finalmente añadimos el título u subtítulo

pyramid <- pyramid +
  ggtitle("Variación de la población de villas y aldeas",subtitle = ("(1998-2018)              \n Mujeres                             Hombres")) +
  theme(plot.title = element_text(hjust = 0.8, size = 14),
        plot.subtitle = element_text(hjust = 0.75, size = 12))

# Guardamos los resultados en png para posteriormente añadirle la leyenda
# Es importante destacar que las escalas están fijadas para sacar el resultado al guardar
# la imagen. Por tanto las escalas difieren y el resultado no se verá "bien" del todo en 
# R

ggsave("pyramid.png", plot = pyramid,width = 8, height = 10)

# La leyenda la realizamos a parte ya que conforme se han graficado los datos es trementamente 
# difícil hacer una buena leyenda en el mismo gráfico

# Hacemos la leyenda

# Texto y coordenadas para dibujar los cuadrados de la leyenda. 
label = c("Aumento 2018" ,"Original 1998","Descenso 2018", "Aumento 2018","Original 1998","Descenso 2018")
breaks = c(7.5,6.5,5.5,3.5,2.5,1.5)
square <- data.frame(x1=c(1,1,1,1), x2=c(2,2,2,2), y1=c(1,2,5,6), y2=c(2,3,6,7), t=c('a','b','c','d'))
square_ligth <- data.frame(x1=c(1,1), x2=c(2,2), y1=c(3,7), y2=c(4,8), s=c('h','r'))

# Graficamos la leyenda

legend_pyramid <- ggplot() +
  xlim(c(-0.5,6.4)) +
  ylim(c(0, 11)) +
  geom_rect(data=square, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), alpha=1) +
  geom_rect(data=square_ligth, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=s), alpha=0.5) +
  guides(fill=FALSE) +
  theme_void() +
  scale_fill_manual(values = c("darkgreen","#1EE049","darkblue","#27AECE", "#00FF39","cyan")) +
  annotate("text", x = 4.32, y = breaks, label = label, size = 9) +
  annotate("text", x = 1.65, y = 9, label = "Variación de la\n población", size = 10) 

# Guardamos la leyenda
# Nuevamente las escalas están puestas para obtener buenos resultados al guardar la imagen

ggsave("legend.png", plot = legend_pyramid, width = 4, height = 10)

# Solamente falta unir las dos imagenes con cualquier programa (PowerPoint por ejemplo)   
# Cargamos la imagen final con la ayuda del siguiente enlace : https://stackoverflow.com/questions/9917049/inserting-an-image-to-ggplot2

img <- readPNG("Archivos_necesarios/Imagen pyramide final.png")
g <- rasterGrob(img, interpolate=TRUE)

ggplot() +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_void()


# Tercera visualización estática ---------------------------------------------------

# Cargamos el shapefile que utilizaremos para graficar los municipios con ggplot2. Este ha sido obtenido del siguiente enlace
# https://team.carto.com/u/furilo/tables/municipios_etrs89_30n/public

municipalities_spain <- readOGR(dsn = "Archivos_necesarios/municipios_etrs89_30n/municipios_etrs89_30n.shp", layer = "municipios_etrs89_30n", encoding = "UTF-8")

# Tratamos este shapefile para poder trabajar con el id de los municipios y provincias. Ayudados
# en este caso por : https://stackoverflow.com/questions/34543468/successor-to-ggplot2fortify

map_data_fortified_spain <- ggplot2:::fortify.SpatialPolygonsDataFrame(municipalities_spain) 
map_data_fortified_spain$id <- as.character(map_data_fortified_spain$id)
code_mun <- as.data.frame(municipalities_spain$codigo)
names(code_mun) <- c("code")
code_mun <- code_mun %>% mutate(id = 0:(nrow(municipalities_spain)-1), code_prov = as.character(municipalities_spain$cod_prov)) 
code_mun$id <- as.character(code_mun$id)
spain_shp_df <- left_join(map_data_fortified_spain, code_mun, by = "id")

# Calculamos la variación de población entre distintos años que utilizaremos para graficar

population_total <- population_total %>% 
  mutate( diff_1998_2011 = ((`2011`/`1998`)-1)*100, diff_2011_2018 = ((`2018`/`2011`)-1)*100,
          diff_1998_2018 = ((`2018`/`1998`)-1)*100)

# Arreglamos los id's de las provincias

code_add <- gsub("0.","0", spain_shp_df$code_prov)
code_add <- gsub("[1-9]{1}[0-9]{1}", "", code_add)
spain_shp_df$code <- paste0(code_add, spain_shp_df$code)
population_total_coord <- left_join(spain_shp_df, population_total, by = c("code" = "code_prov_mun"))
population_total_coord[is.na(population_total_coord)] <- 0

# Realizamos el mapa, la idea original del mapa la he sacado de esta página 
#https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/ 

plot_map <- function(population_total_coord, subtitle, pal) {
  
  
  # Datos para graficar la península quitando Ceuta y Melilla (de los cuales no tenemos datos
  # de población ) y las Islas Canarias que por su situación geográfica no podrán graficarse a la vez.
  population_peninsular <- population_total_coord %>%
    filter(code_prov != 35, code_prov != 38, code_prov != 51, code_prov != 52 )
  
  # Esta función se ha obtenido también de la página https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/ y sirver
  # para cambiar el estilo del mapa
  
  theme_map <- function(...) {  
    theme_minimal() +
      theme(
        text = element_text( color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_blank(),
        ...
      )
  }
  
  # Primero graficamos la Península y las Islas Baleares
  peninsular_plot <- ggplot() +  
    geom_polygon(data = population_peninsular, aes(fill = brks,x = long, y = lat, group = group)) + # Representamos los polígonos correpondientes a los municipios
    coord_equal() + 
    theme_map() + # Añadimos el estilo del mapa
    theme(
      legend.position = c(0.8, 0.05), # Arreglamos la leyenda, formato del título
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.2)),
      legend.text = element_text(size = 10, hjust = 0, color = "#4e4d47"),
      legend.title = element_text(size = 10),
      plot.title = element_text(size = 28, hjust = 0.8, color = "#4e4d47"),
      plot.subtitle = element_text(size = 20, hjust = 0.8, face = "italic", color = "#4e4d47"),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.border = element_blank()
    ) + # Movemos los márgenes para poder graficar las Islas Canárias
    theme(plot.margin = margin(t = 0.1, r = 0, b = 1.1, l = 1.3, unit = "cm")) +
    labs(x = NULL, # Añadimos el título y el subtítulo
         y = NULL, 
         title = "Variación de la población", 
         subtitle = subtitle, 
         caption = "") +
    scale_fill_manual( # Cambiamos la escala de colores
      values = color,
      name = "",
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        title.position = 'top',
        reverse = F)
    ) 
  
  # Se repite el mismo proceso para las Islas Canarias que añadiremos al mapa.
  # Notese que en este caso solo se representa el mapa, ya que la leyenda, título.. están
  # ya en el mapa de la Península
  population_island <- population_total_coord %>%
    filter(code_prov %in% c(35,38))
  
  islands_plot <- ggplot() +  
    geom_polygon(data = population_island, aes(fill = brks,x = long, y = lat, group = group)) +
    coord_equal() + 
    theme_map() +
    scale_fill_manual(
      values = color,
      name = ""
    ) +
    theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  # Finalmente unimos ambos mapas
  final_spain_plot <-
    ggdraw() +
    draw_plot(peninsular_plot) +
    draw_plot(islands_plot, x = 0.04, y = 0, width = .25, height = .25)
  final_spain_plot
}

# Creamos el texto de la leyenda
labels = c( "Descenso mayor del 40%",
            "Descendo del 20 al 40%",
            "Descenso de hasta el 30%",
            "Aumento hasta el 15%",
            "Aumento del 15 al 30%",
            "Aumento mayor del 30%")

# Fijamos los valores mínimos y máximos de la escala de color
minVal <- min(population_total_coord$diff_1998_2018, na.rm = T)
maxVal <- max(population_total_coord$diff_1998_2018, na.rm = T)

# Ponemos los cortes para la escala de color
brks <- c(minVal,-40,-20,0,15,30, maxVal)

# Añadimos una columna al dataframe que contiene una lista con los cortes,
# esto sirve para poner colorear cada municipio
population_total_coord$brks <- cut(population_total_coord$diff_1998_2018, 
                                   breaks = brks, 
                                   labels = labels, 
                                   include.lowest = T)

# Elegimos los colores de la escala quitando el amarillo chillón
color = inferno(7)[1:6]

# Graficamos el mapa final
map <- plot_map(population_total_coord, c("1998-2018"), color)
map







