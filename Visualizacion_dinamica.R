library(shiny)
library(geojsonio)
library(leaflet)
library(ggplot2)
library(rgdal)
library(dplyr)
library(shinyjs)
library(lubridate)

# Carga de datos y preparación--------------------------------------------------------

# Cargamos los datos del paro

table_paro <- read.csv("Archivos_necesarios/table_paro.csv", sep = ",",
                       encoding = "utf-8",
                       colClasses=c("codigo_municipio"="character",
                                    "codigo_CA"="character",
                                    "codigo_provincia"="character"))

table_paro <- table_paro[,-1]

# Cargamos el shapefile para los municipios

municipalities_spain <- readOGR(dsn = "Archivos_necesarios/municipios_etrs89_30n/municipios_etrs89_30n.shp",
                                layer = "municipios_etrs89_30n", encoding = "utf-8", use_iconv = T)

# Realizamos la proyección 
municipalities_spain_leaflet <- spTransform(municipalities_spain, CRS("+init=epsg:4326"))

# Arreglamos el problema de los id's 
add_cero <- function(x, l = 10) {
  if ((as.numeric(x)/l) < 1) {return(paste0('0',x))}
  else {return(as.character(x))}
}

municipalities_spain_leaflet$codigo <- sapply(municipalities_spain_leaflet$codigo, add_cero, l=10000)

# Cargamos un geojson como shapefile pero en este caso para las provincias

url = "https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/spain-provinces.geojson"

provinces_spain_leaflet <- geojson_read(url, what = "sp", encoding = "utf-8")

# Quitamos Ceuta y melilla al no tener datos de ellos

provinces_spain_leaflet <- subset(provinces_spain_leaflet, cod_prov != "51")
provinces_spain_leaflet <- subset(provinces_spain_leaflet, cod_prov != "52")

# Primero necesitamos separar los datos de paro de las provincias

names_rate <- names(table_paro)[!grepl('rate', names(table_paro))]
provinces_paro <- table_paro[,c(names_rate)] %>%
  select(-municipio,-codigo_municipio) %>% 
  group_by(date,codigo_CA,CA,codigo_provincia,provincia) %>% 
  summarise_all("sum")
provinces_paro <- as.data.frame(provinces_paro)
names_new <- names(provinces_paro)
names_new[c(6,8:16)] <- paste0(names_new[c(6,8:16)],"_rate")
provinces_paro_rate <- as.data.frame(sapply(provinces_paro[,c(6,10:16)],function(x) {(x/provinces_paro$population)*100}))
provinces_paro_sex_male <- provinces_paro$paro_hombres/provinces_paro$population_male*100
provinces_paro_sex_female <- provinces_paro$paro_mujeres/provinces_paro$population_female*100
provinces_paro <- cbind(provinces_paro[,c(1:5)], provinces_paro_rate$total_paro, provinces_paro$population,
                        provinces_paro_sex_male, provinces_paro_sex_female,
                        provinces_paro_rate[,c(2:length(provinces_paro_rate))],provinces_paro$population_male,
                        provinces_paro$population_female)
names(provinces_paro) <- names_new

# Separamos los datos de provincias y municipios para la representación de género

provinces_paro_sexo <- provinces_paro[,c(1:5,8:9)] %>% 
  mutate(sex_rate = ifelse(paro_hombres_rate>paro_mujeres_rate,"Hombres","Mujeres"))

table_paro <- table_paro %>% 
  mutate(sex_rate = ifelse(paro_hombres_rate>paro_mujeres_rate,"Hombres","Mujeres"))

# Hcemos lo mismo para el sector

replace_max_4 <- function(x,y,z,t, labels){ # Esta función crea factores en función de que
  v_max <- pmax(x, y, z, t)                 # sector predomine
  vector_return <- seq(0,0,length.out=length(x))
  vector_return[which(x == v_max)] <- labels[1]
  vector_return[which(y == v_max)] <- labels[2]
  vector_return[which(z == v_max)] <- labels[3]
  vector_return[which(t == v_max)] <- labels[4]
  return(vector_return)
}


provinces_paro_sector <- provinces_paro[,c(1:5,13:16)] %>% 
  mutate(sector_rate = replace_max_4(paro_agricultura_rate, paro_industria_rate,
                                     paro_construccion_rate, paro_servicio_rate,
                                     c("Agricultura","Industria","Construcción","Servicio")))

table_paro <- table_paro %>% 
  mutate(sector_rate = replace_max_4(paro_agricultura_rate, paro_industria_rate,
                                     paro_construccion_rate, paro_servicio_rate,
                                     c("Agricultura","Industria","Construcción","Servicio")))

# Y para la edad

replace_max_3 <- function(x,y,z, labels){ # De la misma manera crea factores para la edad
  v_max <- pmax(x, y, z)
  vector_return <- seq(0,0,length.out=length(x))
  vector_return[which(x == v_max)] <- labels[1]
  vector_return[which(y == v_max)] <- labels[2]
  vector_return[which(z == v_max)] <- labels[3]
  return(vector_return)
}


provinces_paro_edad <- provinces_paro[,c(1:5,10:12)] %>% 
  mutate(edad_rate = replace_max_3(paro_menores_25_rate, paro_entre_25_45_rate,
                                   paro_mayores_45_rate, c("Menores de 25 años",
                                                           "Entre 25 y 45 años",
                                                           "Mayores de 45 años")))

table_paro <- table_paro %>% 
  mutate(edad_rate = replace_max_3(paro_menores_25_rate, paro_entre_25_45_rate,
                                   paro_mayores_45_rate, c("Menores de 25 años",
                                                           "Entre 25 y 45 años",
                                                           "Mayores de 45 años")))

# Funciones generales y coordenadas -------------------------------------------------

# Para poder ampliar cada uno de las provincias al hacer click se necesita definir las 
# coordenas y el zoom (lat, long, zoom)

coord <- list(c(-2.8, 42.87, 9),c(-1.95, 38.73, 8),c(-0.6, 38.4, 8),c(-2.4, 37.3, 8),
              c(-4.9, 40.65, 8),c(-6.0, 38.65, 8),c(2.9, 39.3, 8),c(2, 41.75, 8),
              c(-3.4, 42.4, 8),c(-6.2, 39.7, 8),c(-5.8, 36.5, 8),c(-0.1, 40.2, 8),
              c(-3.8, 39.0, 8),c(-4.9, 37.95, 8),c(-8.5, 43.15, 8),c(-2.2, 39.95, 8),
              c(2.5, 42.1, 8),c(-3.3, 37.4, 8),c(-2.55, 40.75, 8),c(-2.15, 43.15, 9),
              c(-6.75, 37.50, 8),c(-0.15, 42.2, 8),c(-3.45, 38.0, 8),c(-5.9, 42.65, 8),
              c(1, 42.05, 8),c(-2.5, 42.25, 8),c(-7.4, 43.05, 8),c(-3.75, 40.55, 8),
              c(-4.65, 36.8, 8),c(-1.45, 38.1, 8),c(-1.60, 42.6, 8),c(-7.5, 42.15, 8),
              c(-5.85, 43.3, 8),c(-4.6, 42.4, 8),c(-14.3, 28.7, 7),c(-8.5, 42.4, 8),
              c(-6.1, 40.75, 8),c(-17.1, 28.2, 8),c(-3.9, 43.27, 8),c(-4, 41.15, 8),
              c(-5.6, 37.5, 8),c(-2.6, 41.6, 8),c(1, 41.1, 8),c(-0.7, 40.6, 8),
              c(-4.15, 39.8, 8),c(-0.7, 39.45, 8),c(-4.8, 41.7, 8),c(-2.9, 43.25, 9),
              c(-6.1, 41.65, 8),c(-0.9, 41.8, 8))

# Estas dos funciones sirven para filtrar los datos por id de provincia y de municipio

province_choose <- function(df, id) {
  province <- subset(df, cod_prov %in% id)
  return(province)
}

municipaliti_choose <- function(df, id){
  municipalities <- subset(df, codigo %in% id)
  return(municipalities)
}


# Explicación funciones para el server ------------------------------------------------

# El resto de funciones es cada uno de los segmentos del server de shiny. Se necesitan 
# 5 funciones para cada selección (Paro total, por sexo, por sector y por edad)

# La primera función (****_initial), carga y filtra los datos de la provincia para una 
# selección (paro_total, género...) y un año dado. Los datos de paro correspondientes 
# se unen con un merge a las datos geográficos para poder representarlos.
# Una vez unificados los datos, se genera una paleta por cuantiles para el caso del paro
# total y por factores para los otros tres casos (sector, edad y género)
# Finalmente se representan los datos con leaflet(con características como bloquear el scroll,
# fijar el zoom, añadir la leyenda...). Es importante notar que para poder añadir los labels
# con un salto de línea se necesita utilizar una función con lapply para utilizar características
# de HTML. 

# La segunda función (****_zoom_prov) realiza un zoom en la provincia, para ello se selecciona esta mediante el 
# id extraido del evento de click. Posteriormente tras filtrar los datos (nótese que estos datos
# al ser generales para todas las selecciones se dejan en la propia estructura del server). 
# Con estos datos, modificamos el mapa creado inicialmente mediante leafletProxy. Se eliminan
# las formas, restricciones... y se grafica la provincia elegida. 

# La tercera función (****_zoom_muni) realiza un proceso muy similar a la función anterior.
# En este caso se cargan solamente los datos del municipio, manteniendo el zoom de la provincia. 

# La cuarta función (****_ggplot2), es básicamente un gráfico de ggplot2 customizado para cada
# una de las elecciones disponibles. Muestra datos históricos desde 2006 hasta 2018 del municipio.

# La quinta y última función, reinicia el mapa al hacer click fuera de los municipios (en todos
# los casos, o en cualquier lugar tras mostrar el gráfico de ggplot2)

# Se comentará solamente de manera breve las funciones correspondientes a la selección del
# paro total, ya que el resto son muy similares

# Funciones server paro ---------------------------------------------------

total_paro_initial <- function(provinces_paro, provinces_spain_leaflet, date_complete,
                               input_select_option, input_select_year){
  
  # Filtramos por año
  province_year <- subset(provinces_paro, date %in% as.character(date_complete))
  
  # Unimos los datos del paro, con los datos geográficos filtrados por año
  provinces_year_merge <<- merge(provinces_spain_leaflet, province_year, by.x = "cod_prov",by.y = "codigo_provincia")
  
  # Creamos la paleta
  pal <- colorQuantile(
    palette = "Reds",
    domain = provinces_year_merge$total_paro_rate, n = 5)
  
  # Realizamos el gráfico de leaflet
  # inconv(...) se utiliza para definir la codificación expecífica de utf-8, 
  # ya que si no se hace esto, al subir la aplicación a shinyio no se reconoce la codificación.
  plot <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_spain_leaflet$cod_prov, # Esta linea es importante para añadir el id para filtrar por click posteriormente
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$total_paro_rate),
                label = ~paste0('Paro total en ',iconv(provincia,'iso-8859-1', 'UTF-8'),' : ', 
                                as.character(sprintf('%.2f',round(provinces_year_merge$total_paro_rate,2))),"%"),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = provinces_year_merge$total_paro_rate,
              title = paste0(input_select_option," ", input_select_year), opacity = 1,
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                p = paste0(round(p * 100), '%')
                cuts = paste0(sprintf('%.2f',round(cuts[-n],2)), "% - ", sprintf('%.2f',round(cuts[-1],2)), "%")
              })
  return(plot)
}
total_paro_zoom_prov <- function(province_select_merge, input_select_option, input_select_year){
  # Creamos la paleta
  pal <- colorQuantile(
    palette = "Reds",
    domain = province_select_merge$total_paro_rate, n = 5)
  
  # Modificamos el gráfico creado con la función anterior correspondiente a la provincia seleccionada
  leafletProxy("final_map") %>%
    clearShapes() %>%  # Eliminamos todos los controles, formas... del mapa anterior
    clearControls() %>% 
    clearTiles() %>%
    addTiles(options=tileOptions(minZoom=index_coord[3], maxZoom=index_coord[3])) %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = province_select_merge,
                layerId = province_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(province_select_merge$total_paro_rate),
                label = ~paste0('Paro total en ',texto,' : ',
                                as.character(sprintf('%.2f',round(province_select_merge$total_paro_rate,2))),"%"),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = province_select_merge$total_paro_rate,
              title = paste0(input_select_option," ",input_select_year), opacity = 1,
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                p = paste0(round(p * 100), '%')
                cuts = paste0(sprintf('%.2f',round(cuts[-n],2)), "% - ", sprintf('%.2f',round(cuts[-1],2)), "%")
              })
}
total_paro_zoom_muni <- function(municipaliti_select_merge){
  # Modificamos el mapa para mostrar solamente el municipio elegido
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = municipaliti_select_merge,
                layerId = municipaliti_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  "#ff4d4d",
                label = ~paste0('Paro total en ',iconv(texto,'iso-8859-1', 'UTF-8'),' : ',
                                as.character(sprintf('%.2f',round(municipaliti_select_merge$total_paro_rate,2))),"%"),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE)) 
}
total_paro_ggplot2 <- function(municipaliti_select_data, index_prov){
  # Creamos el gráfico de ggplot2, en este caso se necesitan además datos de España
  # y la provincia para graficar las medias de paro 
  total_paro_spain <- table_paro[,c("date","total_paro","population")] %>%
    group_by(date) %>% 
    summarise(paro_final_esp = sum(total_paro)/sum(population)*100) 
  
  total_paro_province <- table_paro[, c("date","codigo_provincia","total_paro","population")] %>%  
    filter (codigo_provincia == index_prov) %>% 
    group_by(date) %>% 
    summarise(paro_final_prov = sum(total_paro)/sum(population)*100)
  
  
  total_data <- cbind(municipaliti_select_data$total_paro_rate,
                      total_paro_province$paro_final_prov,
                      total_paro_spain$paro_final_esp)
  
  gplot <- ggplot() +
    geom_line(data = total_paro_spain, (aes(x = date, y = paro_final_esp, color = "Media de paro en España", group = 1)), size = 2) +
    geom_line(data = total_paro_province,(aes(x = date, y = paro_final_prov, color = "Media de paro en la provincia", group = 1)), size = 2) +
    geom_line(data = municipaliti_select_data, aes(x = date, y = total_paro_rate, group = 2), linetype = "dotted", color = "black", size = 1)+
    geom_point(data = municipaliti_select_data, aes(x = date, y = total_paro_rate, group = 2,
                                                    fill = paste0("Paro total en ",unique(iconv(municipaliti_select_data$municipio,'iso-8859-1', 'UTF-8')))), size = 4, show.legend = T, shape = 21,
               color = "black", stroke = 1.5) +
    scale_color_manual(labels =c('Media de paro en España',paste0("Media de paro en ",unique(iconv(municipaliti_select_data$provincia,'iso-8859-1', 'UTF-8')))),
                       values=c("Media de paro en España" = "#ff1a1a", "Media de paro en la provincia" = "#00e673")) +
    guides(color=guide_legend(override.aes=list(shape=c(NA,NA), linetype=c("Media de paro en España" = 8,"Media de paro en la provincia" = 8)))) +
    scale_x_discrete(name = "Año", labels = (year(municipaliti_select_data$date))) +
    scale_y_continuous(name = "Paro total (%)",
                       limits = c(floor(min(total_data)),ceiling(max(total_data))),
                       breaks = c(floor(min(total_data)):ceiling(max(total_data))),
                       labels = paste0(c(floor(min(total_data)):ceiling(max(total_data))),"%")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.35, size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"),
          axis.title = element_text(size =12))  +
    theme(legend.position="bottom", legend.justification="center",
          legend.box = "vertical", legend.text=element_text(size=12),
          legend.title=element_blank(), legend.spacing.y = unit(-0.25, "cm"))+
    scale_fill_manual(name="",values=c("#005ce6")) +
    labs(title=paste0("Paro en ",iconv(unique(municipaliti_select_data$municipio),'iso-8859-1', 'UTF-8'), " (",
                      iconv(unique(municipaliti_select_data$provincia),'iso-8859-1', 'UTF-8'),")")) +
    theme(plot.title = element_text(size=22))
  
  return(gplot)
}
total_paro_reset <- function(provinces_year_merge, input_select_option, input_select_year){
  # Se vuelve al mapa de España inicial al hacer click fuera de las provincias o tras graficar con ggplot2
  pal <- colorQuantile(
    palette = "Reds",
    domain = provinces_year_merge$total_paro_rate, n = 5)
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>% 
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% 
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_year_merge$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$total_paro_rate),
                label = ~paste0('Paro total en ',iconv(unique(provincia),'iso-8859-1', 'UTF-8'),' : ',
                                as.character(sprintf('%.2f',round(provinces_year_merge$total_paro_rate,2))),"%"),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = provinces_year_merge$total_paro_rate,
              title = paste0(input_select_option," ", input_select_year), opacity = 1,
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                p = paste0(round(p * 100), '%')
                cuts = paste0(sprintf('%.2f',round(cuts[-n],2)), "% - ", sprintf('%.2f',round(cuts[-1],2)), "%")
              })
}

# Funciones server género -------------------------------------------------

sexo_initial <- function(provinces_paro_sexo, provinces_spain_leaflet, date_complete,
                         input_select_option, input_select_year){
  
  province_year <- subset(provinces_paro_sexo, date %in% as.character(date_complete))
  
  provinces_year_merge <<- merge(provinces_spain_leaflet, province_year, by.x = "cod_prov",by.y = "codigo_provincia")
  
  pal <- colorFactor(
    palette = c("Blue","Green"),
    domain = c("Hombres","Mujeres"),
    ordered=T)
  
  labs <- lapply(seq(50), function(i) {
    paste0('Paro mujeres en ',iconv(unique(provinces_year_merge$provincia[i]),'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_mujeres_rate[i],2))),"%", '<p></p>',
           'Paro hombres en ',iconv(unique(provinces_year_merge$provincia[i]),'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_hombres_rate[i],2))),"%")
  })
  
  plot <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_spain_leaflet$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$sex_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Hombres","Mujeres"),
              title = paste0(input_select_option," ", input_select_year), opacity = 1)
  return(plot)
}
sexo_zoom_prov <- function(province_select_merge, input_select_option, input_select_year){  
  pal <- colorFactor(
    palette = c("Blue","Green"),
    domain = c("Hombres","Mujeres"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(province_select_merge$texto))), function(i) {
    paste0('Paro mujeres en ',iconv(unique(province_select_merge$texto[i]),'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_mujeres_rate[i],2))),"%", '<p></p>',
           'Paro hombres en ',iconv(unique(province_select_merge$texto[i]),'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_hombres_rate[i],2))),"%")
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>%
    addTiles(options=tileOptions(minZoom=index_coord[3], maxZoom=index_coord[3])) %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = province_select_merge,
                layerId = province_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(province_select_merge$sex_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Hombres","Mujeres"),
              title = paste0(input_select_option," ",input_select_year), opacity = 1)
}
sexo_zoom_muni <- function(municipaliti_select_merge){ 
  pal <- colorFactor(
    palette = c("Blue","Green"),
    domain = c("Hombres","Mujeres"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(municipaliti_select_merge$municipio))), function(i) {
    paste0('Paro mujeres en ',iconv(unique(municipaliti_select_merge$municipio[i]),'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_mujeres_rate[i],2))),"%", '<p></p>',
           'Paro hombres en ',iconv(unique(municipaliti_select_merge$municipio[i]),'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_hombres_rate[i],2))),"%")
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = municipaliti_select_merge,
                layerId = municipaliti_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(municipaliti_select_merge$sex_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))
}
sexo_ggplot2 <- function(municipaliti_select_data){
  
  total_data <- cbind(municipaliti_select_data$paro_hombres_rate, municipaliti_select_data$paro_mujeres_rate)
  
  gplot <- ggplot(data = municipaliti_select_data) + 
    geom_line(aes(x = date, y = paro_hombres_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_hombres_rate,fill = "Hombres"), size = 4, show.legend = T, shape = 21,
               color = "black", stroke = 1.5) +
    geom_line(aes(x = date, y = paro_mujeres_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_mujeres_rate,fill = "Mujeres"), size = 4, show.legend = T, shape = 21,
               color = "black", stroke = 1.5) +
    scale_x_discrete(name = "Año", labels = (year(municipaliti_select_data$date))) +
    scale_y_continuous(name = paste0("Paro (%)"),
                       limits = c(floor(min(total_data)),ceiling(max(total_data))),
                       breaks = c(floor(min(total_data)):ceiling(max(total_data))),
                       labels = paste0(c(floor(min(total_data)):ceiling(max(total_data))),"%")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.35, size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"),
          axis.title = element_text(size =12))  +
    theme(legend.position="bottom", legend.justification="center",
          legend.box = "vertical", legend.text=element_text(size=12),
          legend.title=element_blank(),
          legend.margin=margin(0,0,0,0),legend.box.margin=margin(0,0,-5,0))+
    scale_fill_manual(name="",values=c("Hombres" = "blue","Mujeres" = "green"),
                      breaks = c("Hombres","Mujeres")) +
    labs(title=paste0("Paro en ",iconv(unique(municipaliti_select_data$municipio),'iso-8859-1', 'UTF-8'), " (",
                      iconv(unique(municipaliti_select_data$provincia),'iso-8859-1', 'UTF-8'),")")) +
    theme(plot.title = element_text(size=22))
  return(gplot)
}
sexo_reset <- function(provinces_year_merge, input_select_option, input_select_year){
  pal <- colorFactor(
    palette = c("Blue","Green"),
    domain = c("Hombres","Mujeres"),
    ordered=T)
  
  labs <- lapply(seq(50), function(i) {
    paste0('Paro mujeres en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_mujeres_rate[i],2))),"%", '<p></p>',
           'Paro hombres en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_hombres_rate[i],2))),"%")
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>% 
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_year_merge$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$sex_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Hombres","Mujeres"),
              title = paste0(input_select_option," ", input_select_year), opacity = 1)
}

# Funciones server sector -------------------------------------------------

sector_initial <- function(provinces_paro_sector, provinces_spain_leaflet, date_complete,
                           input_select_option, input_select_year){
  province_year <- subset(provinces_paro_sector, date %in% as.character(date_complete))
  
  provinces_year_merge <<- merge(provinces_spain_leaflet, province_year, by.x = "cod_prov",by.y = "codigo_provincia")
  
  pal <- colorFactor(
    palette = c("Green","#5B6477","Red","Blue"),
    domain = c("Agricultura","Industria","Construcción","Servicio"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(provinces_year_merge$provincia))), function(i) {
    paste0('Paro en agricultura en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_agricultura_rate[i],2))),"%", '<p></p>',
           'Paro en industria en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_industria_rate[i],2))),"%", '<p></p>',
           'Paro en construcción en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_construccion_rate[i],2))),"%", '<p></p>',
           'Paro en servicio en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_servicio_rate[i],2))),"%")
  })
  
  plot <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_spain_leaflet$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$sector_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Agricultura","Industria","Construcción","Servicio"),
              title = paste0(input_select_option," ", input_select_year), opacity = 1)
  return(plot)
}
sector_zoom_prov <- function(province_select_merge, input_select_option, input_select_year){
  
  pal <- colorFactor(
    palette = c("Green","#5B6477","Red","Blue"),
    domain = c("Agricultura","Industria","Construcción","Servicio"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(province_select_merge$texto))), function(i) {
    paste0('Paro en agricultura en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_agricultura_rate[i],2))),"%", '<p></p>',
           'Paro en industria en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_industria_rate[i],2))),"%", '<p></p>',
           'Paro en construcción en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_construccion_rate[i],2))),"%", '<p></p>',
           'Paro en servicio en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_servicio_rate[i],2))),"%")
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>%
    addTiles(options=tileOptions(minZoom=index_coord[3], maxZoom=index_coord[3])) %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = province_select_merge,
                layerId = province_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(province_select_merge$sector_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Agricultura","Industria","Construcción","Servicio"),
              title = paste0(input_select_option," ",input_select_year), opacity = 1)
}
sector_zoom_muni <- function(municipaliti_select_merge) { 
  pal <- colorFactor(
    palette = c("Green","#5B6477","Red","Blue"),
    domain = c("Agricultura","Industria","Construcción","Servicio"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(municipaliti_select_merge$municipio))), function(i) {
    paste0('Paro en agricultura en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_agricultura_rate[i],2))),"%", '<p></p>',
           'Paro en industria en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_industria_rate[i],2))),"%", '<p></p>',
           'Paro en construcción en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_construccion_rate[i],2))),"%", '<p></p>',
           'Paro en servicio en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_servicio_rate[i],2))),"%")
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = municipaliti_select_merge,
                layerId = municipaliti_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(municipaliti_select_merge$sector_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))
}
sector_ggplot2 <- function(municipaliti_select_data){
  
  total_data <- cbind(municipaliti_select_data$paro_agricultura_rate, municipaliti_select_data$paro_industria_rate,
                      municipaliti_select_data$paro_construccion_rate, municipaliti_select_data$paro_servicio_rate)
  
  gplot <- ggplot(data = municipaliti_select_data) + 
    geom_line(aes(x = date, y = paro_agricultura_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_agricultura_rate,fill = "Agricultura"), size = 4, show.legend = T, shape = 21,
               color = "black", stroke = 1.5, alpha = 1) +
    geom_line(aes(x = date, y = paro_industria_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_industria_rate,fill = "Industria"), size = 4, show.legend = T, shape = 22,
               color = "black", stroke = 1.5, alpha = 1) +
    geom_line(aes(x = date, y = paro_construccion_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_construccion_rate,fill = "Construcción"), size = 4, show.legend = T, shape = 24,
               color = "black", stroke = 1.5, alpha = 1) +
    geom_line(aes(x = date, y = paro_servicio_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_servicio_rate,fill = "Servicio"), size = 4, show.legend = T, shape = 25,
               color = "black", stroke = 1.5, alpha = 1) +
    scale_x_discrete(name = "Año", labels = (year(municipaliti_select_data$date))) +
    scale_y_continuous(name = paste0("Paro (%)"),
                       limits = c(floor(min(total_data)),ceiling(max(total_data))),
                       breaks = c(floor(min(total_data)):ceiling(max(total_data))),
                       labels = paste0(c(floor(min(total_data)):ceiling(max(total_data))),"%")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.35, size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title = element_text(size =14))  +
    theme(legend.position="bottom", legend.justification="center",
          legend.box = "vertical", legend.text=element_text(size=12),
          legend.margin=margin(0,0,0,0),legend.box.margin=margin(0,0,-5,0),
          legend.spacing.x = unit(0.2, "cm"))+
    scale_fill_manual(name="",values=c("Agricultura" = "Green","Industria" = "#5B6477",
                                       "Construcción" = "Red","Servicio" = "Blue"),
                      breaks = c("Agricultura","Industria","Construcción","Servicio")) +
    labs(title=paste0("Paro en ",iconv(unique(municipaliti_select_data$municipio),'iso-8859-1', 'UTF-8'), " (",
                      iconv(unique(municipaliti_select_data$provincia),'iso-8859-1', 'UTF-8'),")")) +
    theme(plot.title = element_text(size=22)) +
    guides(fill=guide_legend(override.aes=list(shape=c(21,22,24,25))))
  return(gplot)
}
sector_reset <- function(provinces_year_merge, input_select_option, input_select_year){  
  pal <- colorFactor(
    palette = c("Green","#5B6477","Red","Blue"),
    domain = c("Agricultura","Industria","Construcción","Servicio"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(provinces_year_merge$provincia))), function(i) {
    paste0('Paro en agricultura en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_agricultura_rate[i],2))),"%", '<p></p>',
           'Paro en industria en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_industria_rate[i],2))),"%", '<p></p>',
           'Paro en construcción en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_construccion_rate[i],2))),"%", '<p></p>',
           'Paro en servicio en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_servicio_rate[i],2))),"%")
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>% 
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_year_merge$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$sector_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Agricultura","Industria","Construcción","Servicio"),
              title = paste0(input_select_option," ", input_select_year), opacity = 1)
}

# Funciones edad server ---------------------------------------------------

edad_initial <- function(provinces_paro_edad, provinces_spain_leaflet, date_complete,
                         input_select_option, input_select_year){
  
  province_year <- subset(provinces_paro_edad, date %in% as.character(date_complete))
  
  provinces_year_merge <<- merge(provinces_spain_leaflet, province_year, by.x = "cod_prov",by.y = "codigo_provincia")
  
  pal <- colorFactor(
    palette = c("Green","#ff3333","Blue"),
    domain = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(provinces_year_merge$provincia))), function(i) {
    paste0('Paro menores de 25 en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_menores_25_rate[i],2))),"%", '<p></p>',
           'Paro entre 25 y 45 en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_entre_25_45_rate[i],2))),"%", '<p></p>',
           'Paro mayores de 45 en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_mayores_45_rate[i],2))),"%", '<p></p>')
  })
  
  plot <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_spain_leaflet$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$edad_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
              title = paste0(input_select_option," ", input_select_year), opacity = 1)
  return(plot)
}
edad_zoom_prov <- function(province_select_merge, input_select_option, input_select_year){ 
  
  pal <- colorFactor(
    palette = c("Green","#ff3333","Blue"),
    domain = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(province_select_merge$texto))), function(i) {
    paste0('Paro menores de 25 en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_menores_25_rate[i],2))),"%", '<p></p>',
           'Paro entre 25 y 45 en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_entre_25_45_rate[i],2))),"%", '<p></p>',
           'Paro mayores de 45 en ',iconv(province_select_merge$texto[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(province_select_merge$paro_mayores_45_rate[i],2))),"%", '<p></p>')
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>%
    addTiles(options=tileOptions(minZoom=index_coord[3], maxZoom=index_coord[3])) %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = province_select_merge,
                layerId = province_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(province_select_merge$edad_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
              title = paste0(input_select_option," ",input_select_year), opacity = 1)
}
edad_zoom_muni <- function(municipaliti_select_merge){
  
  pal <- colorFactor(
    palette = c("Green","#ff3333","Blue"),
    domain = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(municipaliti_select_merge$municipio))), function(i) {
    paste0('Paro menores de 25 en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_menores_25_rate[i],2))),"%", '<p></p>',
           'Paro entre 25 y 45 en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_entre_25_45_rate[i],2))),"%", '<p></p>',
           'Paro mayores de 45 en ',iconv(municipaliti_select_merge$municipio[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(municipaliti_select_merge$paro_mayores_45_rate[i],2))),"%", '<p></p>')
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    setView(lng = index_coord[1], lat = index_coord[2], zoom = index_coord[3]) %>%
    addPolygons(data = municipaliti_select_merge,
                layerId = municipaliti_select_merge$codigo,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(municipaliti_select_merge$edad_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE)) 
}
edad_ggplot2 <- function(municipaliti_select_data){
  
  total_data <- cbind(municipaliti_select_data$paro_menores_25_rate, municipaliti_select_data$paro_entre_25_45_rate,
                      municipaliti_select_data$paro_mayores_45_rate)
  
  gplot <- ggplot(data = municipaliti_select_data) + 
    geom_line(aes(x = date, y = paro_menores_25_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_menores_25_rate,fill = "Menores de 25 años"), size = 4, show.legend = T, shape = 21,
               color = "black", stroke = 1.5, alpha = 1) +
    geom_line(aes(x = date, y = paro_entre_25_45_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_entre_25_45_rate,fill = "Entre 25 y 45 años"), size = 4, show.legend = T, shape = 22,
               color = "black", stroke = 1.5, alpha = 1) +
    geom_line(aes(x = date, y = paro_mayores_45_rate, group = 1),linetype = "dotted", color = "black", size = 1) +
    geom_point(aes(x = date, y = paro_mayores_45_rate,fill = "Mayores de 45 años"), size = 4, show.legend = T, shape = 24,
               color = "black", stroke = 1.5, alpha = 1) +
    scale_x_discrete(name = "Año", labels = (year(municipaliti_select_data$date))) +
    scale_y_continuous(name = paste0("Paro (%)"),
                       limits = c(floor(min(total_data)),ceiling(max(total_data))),
                       breaks = c(floor(min(total_data)):ceiling(max(total_data))),
                       labels = paste0(c(floor(min(total_data)):ceiling(max(total_data))),"%")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.35, size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title = element_text(size =14))  +
    theme(legend.position="bottom", legend.justification="center",
          legend.box = "vertical", legend.text=element_text(size=12),
          legend.margin=margin(0,0,0,0),legend.box.margin=margin(0,0,-5,0),
          legend.spacing.x = unit(0.2, "cm"))+
    scale_fill_manual(name="", values=c("Menores de 25 años" = "Green",
                                        "Entre 25 y 45 años" = "#ff3333",
                                        "Mayores de 45 años" = "Blue"),
                      breaks = c("Menores de 25 años",
                                 "Entre 25 y 45 años",
                                 "Mayores de 45 años")) +
    labs(title=paste0("Paro en ",iconv(unique(municipaliti_select_data$municipio),'iso-8859-1', 'UTF-8'), " (",
                      iconv(unique(municipaliti_select_data$provincia),'iso-8859-1', 'UTF-8'),")")) +
    theme(plot.title = element_text(size=22)) +
    guides(fill=guide_legend(override.aes=list(shape=c(21,22,24))))
  return(gplot)
}
edad_reset <- function(provinces_year_merge, input_select_option, input_select_year){
  
  pal <- colorFactor(
    palette = c("Green","#ff3333","Blue"),
    domain = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
    ordered=T)
  
  labs <- lapply(seq(length(unique(provinces_year_merge$provincia))), function(i) {
    paste0('Paro menores de 25 en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_menores_25_rate[i],2))),"%", '<p></p>',
           'Paro entre 25 y 45 en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_entre_25_45_rate[i],2))),"%", '<p></p>',
           'Paro mayores de 45 en ',iconv(provinces_year_merge$provincia[i],'iso-8859-1', 'UTF-8'),' : ', as.character(sprintf('%.2f',round(provinces_year_merge$paro_mayores_45_rate[i],2))),"%", '<p></p>')
  })
  
  leafletProxy("final_map") %>%
    clearShapes() %>% 
    clearControls() %>% 
    clearTiles() %>% 
    addTiles(options=tileOptions(minZoom=6, maxZoom=6)) %>% # Evita scroll
    setView(lng = -3.68, lat = 40.43, zoom = 6) %>%
    addPolygons(data = provinces_year_merge,
                layerId = provinces_year_merge$cod_prov,
                color = "black", weight = 1,
                fillOpacity = 0.7,
                fillColor =  pal(provinces_year_merge$edad_rate),
                label = lapply(labs,HTML),
                highlight = highlightOptions(
                  weight = 2,
                  color = "white",
                  fillOpacity = 0.9,
                  bringToFront = TRUE))%>% 
    addLegend("bottomright", pal = pal, values = c("Menores de 25 años","Entre 25 y 45 años","Mayores de 45 años"),
              title = paste0(input_select_option," ", input_select_year), opacity = 1)
}
################################

# Shiny ui ---------------------------------------------------

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), # Hace que el mapa ocupe toda la pantalla
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: black}")), #Cambia el color de la barra del slider
  tags$style(HTML(".irs-grid-pol {display: none}")), # Quita las marcas del slider
  tags$style(HTML(".irs-grid-text {font-size: 12px; color: black}")), # Customiza los ticks 
  tags$style(HTML("body {font-size: 16px")), # Aumenta el tamaño de las legendas
  useShinyjs(),
  leafletOutput(outputId = 'final_map', # Mapas de leaflet 
                width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, # Permite seleccionar las cuatro opciones disponibles
                selectInput(inputId = 'select_option', label ="Selecciona la opción", 
                            choices = c("Paro total","Paro por sexo","Paro por sector","Paro por edad"))
  ),
  absolutePanel(top = 10, left = 10, # Permite seleccionar el año 
                sliderInput(inputId = 'select_year',label = 'Selecciona el año', 
                            min = 2006, max = 2018, value = 2018, round = T,sep ="")
  ),
  absolutePanel(bottom = 30, left = 60, # Salida para las representaciones con ggplot2
                plotOutput("plot_data", width = "100%", height = "100%" )
   )
  )

# Shiny server ------------------------------------------------------------

# Este es el server de la función de shiny, es bastante complejo y lleva varias variables de
# control (Explicación detallada en el documento adjunto)

# Notese que en ocasiones se utiliza <<- en vez de <-. La doble flecha permite generar variables
# "globales" en el entorno de shiny que se pueden utilizar en diferentes subregiones del código

plot_done <- T  # Se utiliza para esconder inicialmente el mapa de ggplot2. Ya que una vez
                # que se grafíca no se puede elimar y la solución encontrada fue esconderlo y
                # y mostrarlo cuando convenía

server <- function(input, output){

  # Graficamos el mapa inicial de todas las provincias
  output$final_map <- renderLeaflet({
    # Controlamos si se esconde el mapa de ggplot2 antes de seleccionar un municipio
    if(plot_done){shinyjs::hideElement("plot_data", anim = T, animType = "slide")} 
    
    date_complete <<- as.Date(paste0(input$select_year, "-01-01")) # Para poder filtrar por año
    
    # Se añaden las cuatro opciones a las que se accede en función de la opción elegida.
    # Se asignan a una variable ya que sino solo se conseguiría graficar la opción correspondiente
    # al último if
    
    if(input$select_option == "Paro total"){
      plot <- total_paro_initial(provinces_paro, provinces_spain_leaflet, date_complete,
                         input$select_option, input$select_year) 
    }
    if(input$select_option == "Paro por sexo"){
      plot <- sexo_initial(provinces_paro_sexo, provinces_spain_leaflet, date_complete,
                   input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por sector"){
      plot <- sector_initial(provinces_paro_sector, provinces_spain_leaflet, date_complete,
                     input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por edad"){
      plot <- edad_initial(provinces_paro_edad, provinces_spain_leaflet, date_complete,
       input$select_option, input$select_year)
    }
    
    zoomout <<- F # Esta variable controla el reinicio del mapa cuando se ha ampliado la provincia
    zoomoutout <<- F # Esta controla el reinicio cuando se ha seleccionado un municipio específico
    plot_agree <<- F # Esta controla que se permita generar el mapa con ggplot2 cuando es necesario
    
    return(plot) # Se devuleve el mapa de leaflet
  })
  # Cuando hay un evento de click en una de las provincias se seleccionan y cargan los
  # datos de los municipios correspondientes a esa provincia y se hace zoom.
  
  observeEvent(input$final_map_shape_click,{
    
    click<-input$final_map_shape_click # Guarda la información del click sobre una forma 
    if(zoomoutout){return()} # Si se hace click fuera del mapa se fuerza a saltar al último paso
    if(is.null(click$id) | as.numeric(click$id)/100>1){return()} # Controla o bien el reinicio del mapa
                            # o bien que no se ejecute cuando el id correponde a un 
                            # municipio (id prov **, id mun *****)
    
    index_prov <<- click$id # Guarda el id de la provincia
    index_coord <<- coord[[as.numeric(click$id)]] # Guarda el zoom correspondiente a la provincia
    
    # Obtiene los datos de paro de los municípios y se unen con los datos geográficos 
    
    province_select_data <- table_paro %>% 
      filter(codigo_provincia == click$id ) %>%
      group_by(date, codigo_CA, CA,codigo_provincia, 
               provincia, codigo_municipio, municipio) 
    
    province_select_data_year <- subset(province_select_data, date %in% as.character(date_complete))
    
    province_select <- province_choose(municipalities_spain_leaflet, click$id)
    
    province_select_merge <- merge(province_select, province_select_data_year,
                                   by.x = "codigo", by.y = "codigo_municipio")
    
    # Finalmente se representa la opción elegída
    
    if(input$select_option == "Paro total"){
      total_paro_zoom_prov(province_select_merge, input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por sexo"){
      sexo_zoom_prov(province_select_merge, input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por sector"){
      sector_zoom_prov(province_select_merge, input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por edad"){
    edad_zoom_prov(province_select_merge, input$select_option, input$select_year)
    }
    zoomout <<- F
    plot_agree <<- F
    return()
  }) 
  # Se seleccionan, tratan y representan los datos del municipio 
  observeEvent(input$final_map_shape_click,{
    
    # El código es practicamente identico al correspondiente al zoom sobre la provincia.
    
    click<-input$final_map_shape_click
    
    if(zoomoutout){return()}
    if(as.numeric(click$id)/100<1){return()} # nótese que en este caso se permiten solamente id's de municipios
    index_mun <<- click$id
    
    municipaliti_select_data <<- table_paro %>%
      filter(codigo_municipio == click$id ) %>%
      group_by(date, codigo_CA, CA,codigo_provincia, 
               provincia)
    
    municipaliti_select_data_year <- subset(municipaliti_select_data, date %in% as.character(date_complete))
    
    municipaliti_select <- municipaliti_choose(municipalities_spain_leaflet, click$id)
    
    municipaliti_select_merge <- merge(municipaliti_select, municipaliti_select_data_year,
                                       by.x = "codigo", by.y = "codigo_municipio")
    
    if(input$select_option == "Paro total"){
      total_paro_zoom_muni(municipaliti_select_merge)
    }
    if(input$select_option == "Paro por sexo"){
      sexo_zoom_muni(municipaliti_select_merge)
    }
    if(input$select_option == "Paro por sector"){
      sector_zoom_muni(municipaliti_select_merge)
    }
    if(input$select_option == "Paro por edad"){
    edad_zoom_muni(municipaliti_select_merge)
    }
    zoomout <<- F
    zoomoutout <<- T # Hace que el próximo click fuera del gráfico de ggplot2 reinicie el mapa de España automaticamente
    plot_agree <<- T # Hace que se grafiquen los datos con ggplot2 automaticamente en la misma iteración
    return()
  }) 
  
  # Se representan los datos con ggplot2
  observeEvent(input$final_map_click,{
    if(plot_agree){
      # Muestra el gráfico 
      # Se hace con un pequeño retraso (delay) para que la transición no sea muy brusca,
      # sin embargo esto no se consigue en todos los casos. No cubre todas las posibles opciones
      shinyjs::delay(500, shinyjs::showElement("plot_data" , anim = T, animType = "fade")) 
      
      output$plot_data <- renderPlot({
        # Filtra los datos del município específico
        municipaliti_select_data <- table_paro %>%
          filter(codigo_municipio == index_mun ) %>%
          group_by(date, codigo_CA, CA,codigo_provincia, provincia)
        
        if(input$select_option == "Paro total"){
          gplot <- total_paro_ggplot2(municipaliti_select_data, index_prov)
        }
        if(input$select_option == "Paro por sexo"){
          gplot <- sexo_ggplot2(municipaliti_select_data) 
        }
        if(input$select_option == "Paro por sector"){
          gplot <- sector_ggplot2(municipaliti_select_data)
        }
        if(input$select_option == "Paro por edad"){
          gplot <- edad_ggplot2(municipaliti_select_data)
        }
      gplot
      },height = 400,width = 500) # Controla el tamaño con el que se superpone sobre el mapa de leaflet
      plot_agree <<- F
    }
  })
  
  #Cuando se hace un click fuera del mapa se vuelve al mapa total o cuando se llega a nivel de un municipio
  observeEvent(input$final_map_click,{
   
    click<-input$final_map_click
    # Utilizamos zoomout aquí para evitar que se vuelva al mapa inicial al hacer click fuera del mapa de España inicial
    # (solo lo permite una vez, un doble click fuera del mapa inicial también lo reiniciará)
    if(!zoomout) {return(zoomout <<- T)} 
    shinyjs::hideElement("plot_data", anim = T, animType = "slide") # Esconde ggplot2
    
    plot_done <<- T 
    # Reinicia el mapa original correspondiente a la selección
    if(input$select_option == "Paro total"){
      total_paro_reset(provinces_year_merge, input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por sexo"){
      sexo_reset(provinces_year_merge, input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por sector"){
      sector_reset(provinces_year_merge, input$select_option, input$select_year)
    }
    if(input$select_option == "Paro por edad"){
    edad_reset(provinces_year_merge, input$select_option, input$select_year)
    }
    zoomout <<- F
    zoomoutout <<- F
    plot_agree <<- F
    
    return()
    
  })
  
}

# Construimos la app
shinyApp(ui = ui, server = server)
