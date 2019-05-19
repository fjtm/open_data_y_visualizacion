library(RCurl)
library(dplyr)
library(tidyr)
library(zoo)

# Primero descargarmos y cargamos todos los datos

# Descargamos los datos del paro de la página de la SEPE (Servicio Público de Empleo Estatal).
# Para ello descargamos directamente el csv del código
# html de la página con ayuda del siguiente enlace:
#https://www.r-bloggers.com/getting-data-from-an-online-source/


years <- c(2006:2018) # Años para modificar la url

# Creamos la url, descargamos los datos de un año y convertimos los datos en un dataframe
url <-  paste0(c('https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_'),years[1],c('_csv.csv'))
myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE) 
table_paro <- read.csv(textConnection(myfile), header=F, sep = ";", stringsAsFactors = F)[-c(1,2),]

# Descargamos el resto de los datos y los vamos concatenando para crear el dataframe final
for (year in years[-1]) {
  print(paste0(c("Descargando: "), year, c(".../2018")))
  url <-  paste0(c('https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_'),year,c('_csv.csv'))
  myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  table_paro1 <- read.csv(textConnection(myfile), header=F, sep = ";")[-c(1,2),]
  table_paro <- rbind(table_paro, table_paro1)
}
table_paro1 <- NULL

# Cambiamos el nombre del df correspondiente al paro

names(table_paro) <- c('codigo_mes', 'mes', 'codigo_CA', 'CA','codigo_provincia', 
                       'provincia', 'codigo_municipio', 'municipio','total_paro', 
                       'paro_hombre_menor_25','paro_hombre_25_45', 'paro_hombre_mayor_45',
                       'paro_mujer_menor_25', 'paro_mujer_25_45','paro_mujer_mayor_45',
                       'paro_agricultura', 'paro_industria','paro_construccion',
                       'paro_servicio', 'paro_sin_empleo_anterior')

# Cargamos los datos obtenidos mediante Selenium en Python. 

table_prov_male <- read.csv("Archivos_necesarios/table_prov_male.csv", sep = ',', encoding = "iso-8859-1", dec = ".")
table_prov_female <- read.csv("Archivos_necesarios/table_prov_female.csv", sep = ',', encoding = "iso-8859-1", dec = ".")

# Arreglamos los índices

table_prov_male <- table_prov_male[,-1] 
table_prov_female <- table_prov_female[,-1]

# Arreglamos los nombres

names(table_prov_male)[-1] <- gsub("X","", names(table_prov_male)[-1])
names(table_prov_female)[-1] <- gsub("X","", names(table_prov_female)[-1])

# Preprocesado

# Datos de población 


glimpse(table_prov_male) # 1997 No tiene datos, pero los tipos de las variables están bien
glimpse(table_prov_female)

# Lo primero de todo separamos el código de provincia o municipio


table_prov_male <- separate(table_prov_male, Provincia_Municipios, c("code_prov_mun","municipio"), extra = "merge", sep = " ")
table_prov_female <- separate(table_prov_female, Provincia_Municipios, c("code_prov_mun","municipio"), extra = "merge", sep = " ")

# Añadimos una columna con la provincia 
name_prov = c()
code_prov = c()
for (i in 1:length(table_prov_male$code_prov_mun)){
  if (nchar(table_prov_male$code_prov_mun[i]) == 2) {
    name_prov = c(name_prov, table_prov_male$municipio[i])
    code_prov = c(code_prov, table_prov_male$code_prov_mun[i])
  }
}

names(name_prov) <- code_prov
prov <- name_prov[substr(table_prov_male$code_prov_mun, 1, 2)]
table_prov_male$provincia <- prov
table_prov_female$provincia <- prov
table_prov_male <- table_prov_male[,c(1,26,2:25)]
table_prov_female <- table_prov_female[,c(1,26,2:25)]

# Eliminamos 1997 ya que se año no se recogieron datos de población

table_prov_male <- table_prov_male[,-which(names(table_prov_male) == '1997')]
table_prov_female <- table_prov_female[,-which(names(table_prov_female) == '1997')]

# Veamos si tienen más NA's

colSums(is.na(table_prov_male)) # Si hay NA's
colSums(is.na(table_prov_female))

# Puesto que los datos no varian mucho, imputamos a la media de la fila y no perderemos 
# mucha información al tener muchos registros. 

table_prov_male_complete <- table_prov_male
table_prov_female_complete <- table_prov_female 

for (i in 4:ncol(table_prov_male_complete)) {
  index_male <- which(is.na(table_prov_male_complete[,i]), i)
  index_female <- which(is.na(table_prov_female_complete[,i]), i)
  mean_data_male <- apply(table_prov_male_complete[index_male,-c(1,2,3)], 1, function(x) {round(mean(x, na.rm = T))})
  mean_data_female <- apply(table_prov_female_complete[index_female,-c(1,2,3)], 1, function(x) {round(mean(x, na.rm = T))})
  table_prov_male_complete[index_male, i] <- mean_data_male
  table_prov_female_complete[index_female, i] <- mean_data_female
}

# df's para visualización estática
# write.csv(table_prov_male_complete, "table_prov_male_complete.csv")
# write.csv(table_prov_female_complete, "table_prov_female_complete.csv")

# Creamos la tabla correspondiente a la población total como suma de los dos data frames

table_prov_total_complete <- cbind(table_prov_male_complete[,c(1,2,3)], table_prov_male_complete[,-c(1,2,3)] + table_prov_female_complete[,-c(1,2,3)])
sum(is.na(table_prov_total_complete))

# Convertimos los wide datasets en long datasets para poder calcular posteirmento los ratios de paro

# Población total
table_prov_total_complete <- table_prov_total_complete[,1:16]
nombres <- names(table_prov_total_complete)
table_population_long <- gather(table_prov_total_complete, date, population, -c(nombres[c(1,2,3)]))

table_population_long$date <- as.Date(paste0(table_population_long$date, "-01-01"))
table_population_long <- table_population_long %>% 
  select(date,code_prov_mun,provincia,municipio,population)

# Población hombres
table_prov_male_complete <- table_prov_male_complete[,1:16]
nombres_male <- names(table_prov_male_complete)
table_population_male_long <- gather(table_prov_male_complete, date, population_male, -c(nombres_male[c(1,2,3)]))

table_population_male_long$date <- as.Date(paste0(table_population_male_long$date, "-01-01"))
table_population_male_long <- table_population_male_long %>% 
  select(date,code_prov_mun,provincia,municipio,population_male)

# Población mujeres
table_prov_female_complete <- table_prov_female_complete[,1:16]
nombres_female <- names(table_prov_female_complete)
table_population_female_long <- gather(table_prov_female_complete, date, population_female, -c(nombres_female[c(1,2,3)]))

table_population_female_long$date <- as.Date(paste0(table_population_female_long$date, "-01-01"))
table_population_female_long <- table_population_female_long %>% 
  select(date,code_prov_mun,provincia,municipio,population_female)

# Eliminamos codigos 02 ,03... Es decir eliminamos los datos de las provincias. 
table_population_long <- table_population_long[as.numeric(table_population_long$code_prov_mun)/100 >1,]

table_population_male_long <- table_population_male_long[as.numeric(table_population_male_long$code_prov_mun)/100 >1,]

table_population_female_long <- table_population_female_long[as.numeric(table_population_female_long$code_prov_mun)/100 >1,]

# Datos de paro 

glimpse(table_paro) # Hay que arreglar la mayoría de los tipos

table_paro[ ,c(1,2,7,8)] <- sapply(table_paro[ ,c(1,2,7,8)], as.character)
table_paro[ ,c(9:ncol(table_paro))] <- sapply(table_paro[ ,c(9:ncol(table_paro))], as.numeric)

# Veamos si tienen Na's

sum(is.na(table_paro)) # No hay Na's

#Apañamos el código de CA, provincia y municipio. (Ej 2198 --> 02198)

add_cero <- function(x, l = 10) {
  if ((as.numeric(x)/l) < 1) {return(paste0('0',x))}
  else {return(as.character(x))}
}

table_paro$codigo_CA <- sapply(table_paro$codigo_CA, add_cero)
table_paro$codigo_provincia <- sapply(table_paro$codigo_provincia, add_cero)
table_paro$codigo_municipio <- sapply(table_paro$codigo_municipio, add_cero, l=10000)

# Arreglamos la fecha

table_paro$date <- as.Date(as.yearmon(table_paro$codigo_mes, "%Y%m"))

table_paro <- table_paro[,c(ncol(table_paro),3:(ncol(table_paro)-1))]

# Unimos la población de los long datasets con los datos de paro

table_paro_complete <- left_join(table_paro, table_population_long[,c(1,2,5)],
                                 by = c("date" = "date",
                                        "codigo_municipio" = "code_prov_mun"))
table_paro_male_complete <- left_join(table_paro_complete, table_population_male_long[,c(1,2,5)],
                                 by = c("date" = "date",
                                        "codigo_municipio" = "code_prov_mun"))
table_paro_female_complete <- left_join(table_paro_complete, table_population_female_long[,c(1,2,5)],
                                 by = c("date" = "date",
                                        "codigo_municipio" = "code_prov_mun"))
# Los registros con NA's no nos interesan 

table_paro_complete <- table_paro_complete[complete.cases(table_paro_complete), ]
table_paro_male_complete <- table_paro_male_complete[complete.cases(table_paro_male_complete), ]
table_paro_female_complete <- table_paro_female_complete[complete.cases(table_paro_female_complete), ]

# Finalmente calculamos los valores relativos para la visualización dinámica
# (Número de parados(provincia, municipio...)/Población(provincia, municipio...)*100 )

# En todos los casos (menos para el genero) se usa la población total, que si bien no es 
# lo más indicado para el cálculo del ratio de paro por sector y por edad, es una aproximación 
# razonable. 
# Para el género si se utilizan los datos de población por sexo. 

names_numeric <- names(table_paro_complete)[sapply(table_paro_complete, is.numeric)]
population <- table_paro_complete[,names_numeric[length(names_numeric)]]
paro_rate <- sapply(table_paro_complete[,names_numeric[1:(length(names_numeric)-1)]],
                    function(x) {(x/population)*100})
paro_rate <- as.data.frame(paro_rate)
names_new <- paste0(names_numeric[1:(length(names_numeric)-1)],"_rate")
names(paro_rate) <- names_new
table_paro_complete <- cbind(table_paro_complete, paro_rate)

paro_sex_male <- sapply(table_paro_male_complete[,c(9:11)],function(x) {(x/table_paro_male_complete$population_male)*100})
paro_sex_female <- sapply(table_paro_female_complete[,c(12:14)],function(x) {(x/table_paro_female_complete$population_female)*100})
paro_sex <- as.data.frame(cbind(paro_sex_male, paro_sex_female))

# Visualización dinámica paro total

paro_total <- table_paro_complete[,c(8,21,20)]

# Visualización dinámica paro por sexo

paro_sex_rate <- paro_sex %>% 
  mutate( paro_hombres_rate = (paro_hombre_menor_25+paro_hombre_25_45+paro_hombre_mayor_45),
          paro_mujeres_rate = (paro_mujer_menor_25+paro_mujer_25_45+paro_mujer_mayor_45)) %>% 
  dplyr::select(paro_hombres_rate,paro_mujeres_rate)
paro_sex_no_rate <- table_paro_complete[,c(9:14)] %>% 
  mutate( paro_hombres = (paro_hombre_menor_25+paro_hombre_25_45+paro_hombre_mayor_45),
          paro_mujeres = (paro_mujer_menor_25+paro_mujer_25_45+paro_mujer_mayor_45)) %>% 
  dplyr::select(paro_hombres,paro_mujeres)

paro_sex <- as.data.frame(cbind(paro_sex_no_rate, paro_sex_rate))

# Visualización dinámica paro por edad

paro_edad <- table_paro_complete[,c(9:14,22:27)] %>% 
  mutate( paro_menores_25 = (paro_hombre_menor_25+paro_mujer_menor_25),
          paro_entre_25_45 = (paro_hombre_25_45+paro_mujer_25_45),
          paro_mayores_45 =(paro_hombre_mayor_45+paro_mujer_mayor_45),
          paro_menores_25_rate = (paro_hombre_menor_25_rate+paro_mujer_menor_25_rate),
          paro_entre_25_45_rate = (paro_hombre_25_45_rate+paro_mujer_25_45_rate),
          paro_mayores_45_rate =(paro_hombre_mayor_45_rate+paro_mujer_mayor_45_rate)) %>% 
  dplyr::select(paro_menores_25,paro_entre_25_45,paro_mayores_45,
                paro_menores_25_rate,paro_entre_25_45_rate,paro_mayores_45_rate)

# Visualización dinámica paro por sector 

paro_sector <- table_paro_complete[,c(15:18,28:31)] 

# Unimos todos estos datos en un dataframe final 

table_paro_final <- cbind(table_paro_complete[,c(1:7)],paro_total, paro_sex, paro_edad, paro_sector,
                          population_male = table_paro_male_complete$population_male,
                          population_female = table_paro_female_complete$population_female)
# Guardamos el dataframe final

# df para visualización dinámica
# write.csv(table_paro_final, "table_paro.csv") 

