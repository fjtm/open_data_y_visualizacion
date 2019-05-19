# -*- coding: utf-8 -*-
"""
Created on Fri Jan  4 13:41:43 2019

@author: ftebar
"""
#%%

# Recomiendo encarecidamente no ejecutar todo el codigo a la vez. La descarga de 
# de todos los datos requiere un tiempo bastante elevado (algo más de media hora). Por tanto se adjunta
# los datos finales obtenidos con este scrip

from selenium import webdriver
import pandas as pd
import time

#%%

def population_prov(code_provincia,gender):
    driver = webdriver.Chrome("Archivos_necesarios/chromedriver") # Abrimos el navegador
    driver.get("https://www.ine.es/dynt3/inebase/index.htm?padre=525") # Cargamos la página
    time.sleep(1)
    button = driver.find_element_by_xpath("//*[text()='Aceptar']") # Aceptamos los términos
    button.click()
    time.sleep(1)
    button = driver.find_element_by_id(code_provincia) # Selecionamos la provincia
    button.click()
    time.sleep(2)
    button = driver.find_element_by_xpath("//*[text()='Aceptar']") # Aceptamos los términas otra vez
    button.click()
    button = driver.find_element_by_xpath("//option[text()='Total']") # Quitamos la selección del total
    button.click()
    button = driver.find_element_by_xpath("//option[text()="+ gender + "]") # Seleccionamos el género 
    button.click()
    button = driver.find_element_by_xpath("//*[@id='caja_periodo']//*[@title='Seleccionar todos']") # Selecionamos todos los años 
    button.click()
    button = driver.find_element_by_id("botonConsulSele") # Cargamos la tabla con los datos seleccionados
    button.click()
    time.sleep(1)
    table = driver.find_element_by_id("tablaDatos") # Buscamos la tabla
    table = table.get_attribute("outerHTML") # Descargamos el html 
    driver.close() # Cerramos el navegador
    # Es importante tener en cuenta el separador de miles como el punto, sino no guarda bien los datos y es muy dificil de procesar
    prov_table = pd.read_html(table, header=0, encoding="utf8", thousands = '.')[0] # Convertimos la tabla html a df
    time.sleep(1)
    return(prov_table[1:])
    
# Códigos de las provincias a descargar ordendos alfabeticamente 

code_provincia = ["t_2855","t_2856","t_2857","t_2854","t_2886","t_2858","t_2859",
                  "t_2860","t_2861","t_2905","t_2862","t_2863","t_2864","t_2893",
                  "t_2865","t_2866","t_2901","t_2868","t_2869","t_2873","t_2870",
                  "t_2871","t_2872","t_2874","t_2875","t_2876","t_2877","t_2878",
                  "t_2880","t_2881","t_2882","t_2883","t_2884","t_2885","t_2888",
                  "t_2889","t_2890","t_2879","t_2891","t_2892","t_2894","t_2895",
                  "t_2896","t_2900","t_2899","t_2902","t_2903","t_2904","t_2906",
                  "t_2907"] 

# Creamos el encabezado del data frame

lista = (range(1996,2019))
lista = [str(i) for i in lista][::-1]
lista = ["Provincia_Municipios"] + lista

# Descargamos todos los datos de población por provincia y municipio para los hombres
# No se descarga todo a la vez porque el INE no permite generar tablas de más de 10.000 valores cada vez

table_prov_male = population_prov("t_2855","'Hombres'") # Descargamos la primera provincia s
table_prov_male.columns = lista # Se introduce el encabezado

for code in code_provincia[1::]: 
    table_prov_male1 = population_prov(code,"'Hombres'") # Descargamos la siguiente provincia 
    table_prov_male1.columns = lista # Se introduce el encabezado
    table_prov_male = pd.concat([table_prov_male, table_prov_male1]) # Se añaden los resultados al data frame total
    
table_prov_male = table_prov_male.reset_index() # Se reinician los indices
del(table_prov_male['index'] ) # Se elimina la columna con los índices anteriores
#%% 

# Se repite exáctamente el mismo proceso para el caso de las mujeres 

table_prov_female = population_prov("t_2855","'Mujeres'")
table_prov_female.columns = lista

for code in code_provincia[1::]:
    table_prov_female1 = population_prov(code,"'Mujeres'")
    table_prov_female1.columns = lista
    table_prov_female = pd.concat([table_prov_female, table_prov_female1])

table_prov_female = table_prov_female.reset_index()
del(table_prov_female['index'] )    
#%%

# Se guardan los resultados en un csv para poder importarlo a R

table_prov_female.to_csv("table_prov_female.csv", encoding = "iso-8859-1")
table_prov_male.to_csv("table_prov_male.csv", encoding = "iso-8859-1")