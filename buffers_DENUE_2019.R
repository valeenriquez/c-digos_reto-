# Fijamos el directorio de trabajo 
setwd("~/Documents/Ciencia de datos/Reto")

#LIBRERÍAS
library(tidyverse)
library(sf)

#DATOS 
# Análisis del acceso a instalaciones de servicios de cuidados en México;
# para adultos mayores, niños de entre 0-4 años de edad y personas con alguna discapacidad.
# La información utilizada es del Directorio Estadístico Nacional de Unidades Económicas (DENUE) del año 2019.
# La finalidad es conocer cuál es la probabilidad de que las personas que viven en las distintas regiones del país; 
# norte, norte-occidente, centro-norte, centro y sur (CEEY, 2022) puedan tener acceso a los servicios de cuidado 
# (estancias para adultos mayores, estancias para discapacitados y guarderías) en la región en la que viven.

#Importamos datos de estancias pertenecientes al DENUE 2019
archivos = list.files(path = 'DENUE_por_estado', full.names = T) 
#Importamos datos de las colonias de la CONAPO (2020)
colonias = read_sf('imc2020_shp')

#Dividimos los datos por regiones
archivos_norte = archivos[c(2, 5, 8, 19, 26, 28)] # Archivos para la región norte 
archivos_norte_occidente = archivos[c(3, 10, 18, 25, 32)] # Archivos para la región norte occidente
archivos_centro_norte = archivos[c(1, 6, 14, 16, 24)] # Archivos para la región centro norte 
archivos_centro = archivos[c(9, 11, 13, 15, 17, 21, 22, 29)] # Archivos para la región centro
archivos_sur = archivos[c(4, 7, 12, 20, 23, 27, 30, 31)] # Archivos para la región sur

# Nacional ----------------------------------------------------------------

#Vector vacío
unidades_nacional = c()

# Unidades de cuidado en México, ubicamos cada estancia en las coordenas que le corresponden
for (archivo in archivos) {
  df = readxl::read_xlsx(archivo) %>% 
    select (nombre_act, longitud, latitud) %>% 
    st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
  
  unidades_nacional = bind_rows(unidades_nacional, df)
  rm(df)
}
# Hacemos la clasificación de estancias de adultos mayores, estancias de discapacitados y niños. Creamos una tabla que contenga las unidades de servicios de cuidados a nivel nacional.
unidades_nacional=unidades_nacional %>% 
  mutate(nombre_act=case_when(
    nombre_act %in% c("Asilos y otras residencias del sector privado para el cuidado de ancianos",
                      "Asilos y otras residencias del sector público para el cuidado de ancianos") ~ "Estancias de adultos mayores", # Clasificamos las estancias de adultos mayores 
    nombre_act %in% c("Centros del sector público dedicados a la atención y cuidado diurno de ancianos y discapacitados",
                      "Centros del sector privado dedicados a la atención y cuidado diurno de ancianos y discapacitados")~"Estancias de discapacitados y adultos mayores", # Clasificamos las estancias de discapacitados y adultos mayores
    nombre_act %in% c("Guarderías del sector privado","Guarderías del sector público")~"Guarderías")) # Clasificamos las estancias para niños de entre 0-4 años de edad

# Generamos un gráfico del acceso a los servicios de cuidados en las regiones del país, se grafican las estancias en las coordenadas que se encuentran para mapear la ubicación en la que se encuentran.   
ggplot(unidades_nacional) +
  geom_sf(aes(col = nombre_act))+
  scale_color_manual(values = c("#CC2936", "#EA7AF4", "#4B296B")) +
  labs(title = "Servicios de cuidados en México 2019",
       caption = "Elaboración propia con datos del DENUE 2019")+
  theme_classic()

# Generamos una clasificación para ver el acceso a los servicios de cuidados para Adultos mayores.
ancianos = unidades_nacional %>% 
  filter(str_detect(nombre_act,'adultos')) # Filtramos la clasificación de unidades para "adultos"
  
# Gráfico para mapear las ubicaciones de las estancias para adultos mayores.
ggplot(ancianos) +
  geom_sf(aes(col = nombre_act))+
  scale_color_manual(values = c("#CC2936","#CC2936"))+
  labs(title = "Estancias de adultos mayores en México 2019",
       caption = "Elaboración propia con datos del DENUE 2019")+
  theme_classic()

# Generamos una clasificación para ver el acceso a los servicios de cuidados para personas con discapacidad.
discapacitados = unidades_nacional %>% 
  filter(str_detect(nombre_act, 'discapacitados')) # Filtramos la clasificación de unidades para "discapacitados"
  
# Gráfico para mapear las ubicaciones de las estancias para discapacitados.
ggplot(discapacitados) +
  geom_sf(aes(col = nombre_act))+
  scale_color_manual(values = c("#EA7AF4"))+
  labs(title = "Estancias de discapacitados y adultos mayores en México 2019",
       caption = "Elaboración propia con datos del DENUE 2019")+
  theme_classic()

# Generamos una clasificación para ver el acceso a los servicios de cuidados para niños de 0-4 años de edad.
guarderias = unidades_nacional %>% 
  filter(str_detect(nombre_act, 'Guarderías')) # Filtramos la clasificación de unidades para "Guarderías"

 # Gráfico para mapear las ubicaciones de las estancias para niños "Guarderías".  
ggplot(guarderias) +
  geom_sf(aes(col = nombre_act))+
  scale_color_manual(values = c("#4B296B"))+ 
  labs(title = "Guarderías en México 2019",
       caption = "Elaboración propia con datos del DENUE 2019")+
  theme_classic()

# Clasificación por Regiones ----------------------------------------------------------------
# Norte -------------------------------------------------------------------
#Vector vacío
unidades_norte = c()

#Unidades de cuidado en la región norte
for (archivo in archivos_norte) {
  df = readxl::read_xlsx(archivo) %>% 
    select (nombre_act, longitud, latitud) %>% 
    st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
  
  unidades_norte = bind_rows(unidades_norte, df)
  rm(df)
}

#Adultos mayores
ancianos_norte = unidades_norte %>% 
  filter(str_detect(nombre_act, 'ancianos'))

#Creamos los buffers de las estancias para ancianos
buffers_ancianos_norte = st_buffer(ancianos_norte, 500)

#Seleccionamos las colonias del norte e igualamos su proyección a la de los buffers porque no era igual
colonias_norte = colonias %>% 
  select(CVE_ENT, geometry) %>% 
  filter(CVE_ENT %in% c('02', '05', '08', '19', '26', '28')) %>% 
  st_transform(crs = st_crs(buffers_ancianos_norte)) %>% 
  st_make_valid()

#Calculamos los centroides de las colonias en esta región
centroides_norte = st_centroid(colonias_norte)

#Intersecciones
int_ancianos_norte = st_intersects(centroides_norte, buffers_ancianos_norte)  %>% 
  sapply(length)

# Así se vería el panel
table(int_ancianos_norte > 0)

#La probabilidad de tener acceso a una estancia para adultos mayores en la región norte es 6.56%
p_ancianos_norte = (1034/nrow(colonias_norte))*100
p_ancianos_norte

#Discapacitados
discapacitados_norte = unidades_norte %>% 
  filter(str_detect(nombre_act, 'discapacitados'))

#Creamos los buffers de las estancias para discapacitados
buffers_discapacitados_norte = st_buffer(discapacitados_norte, 500)

#Las proyecciones ya son iguales
st_crs(buffers_discapacitados_norte) == st_crs(colonias_norte)

#Intersecciones
int_discapacitados_norte = st_intersects(centroides_norte, buffers_discapacitados_norte)  %>% 
  sapply(length)

# Así se vería el panel
table(int_discapacitados_norte > 0)

#La probabilidad de tener acceso a una estancia para discapacitados en la región norte es 3.04%
p_discapacitados_norte = (480/nrow(colonias_norte))*100
p_discapacitados_norte

#Guarderías
guarderias_norte = unidades_norte %>% 
  filter(str_detect(nombre_act, 'Guarderías'))

#Creamos los buffers de las guarderías
buffers_guarderias_norte = st_buffer(guarderias_norte, 500)

#Las proyecciones ya son iguales
st_crs(buffers_guarderias_norte) == st_crs(colonias_norte)

#Intersecciones
int_guarderias_norte = st_intersects(centroides_norte, buffers_guarderias_norte)  %>% 
  sapply(length)

# Así se vería el panel
table(int_guarderias_norte > 0)

#La probabilidad de tener acceso a una estancia para guarderias en la región norte es 32.07%
p_guarderias_norte = (5058/nrow(colonias_norte))*100
p_guarderias_norte


# Norte-occidente ---------------------------------------------------------

#Vector vacío
unidades_norte_occidente = c()

#Unidades de cuidado en la región norte_occidente
for (archivo in archivos_norte_occidente) {
  df = readxl::read_xlsx(archivo) %>% 
    select (nombre_act, longitud, latitud) %>% 
    st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
  
  unidades_norte_occidente = bind_rows(unidades_norte_occidente, df)
  rm(df)
}

#Adultos mayores
ancianos_norte_occidente = unidades_norte_occidente %>% 
  filter(str_detect(nombre_act, 'ancianos'))

#Creamos los buffers de las estancias para ancianos
buffers_ancianos_norte_occidente = st_buffer(ancianos_norte_occidente, 500)

#Seleccionamos las colonias del norte-occidente e igualamos su proyección a la de los buffers porque no era igual
colonias_norte_occidente = colonias %>% 
  select(CVE_ENT, geometry) %>% 
  filter(CVE_ENT %in% c('03', '10', '18', '25', '32')) %>% 
  st_transform(crs = st_crs(buffers_ancianos_norte_occidente)) %>% 
  st_make_valid()

#Calculamos los centroides de las colonias en esta región
centroides_norte_occidente = st_centroid(colonias_norte_occidente)

#Intersecciones
int_ancianos_norte_occidente = st_intersects(centroides_norte_occidente, buffers_ancianos_norte_occidente)  %>% 
  sapply(length)

# Así se vería el panel
table(int_ancianos_norte_occidente > 0)

#La probabilidad de tener acceso a una estancia para adultos mayores en la región norte-occidente es 6.87%
p_ancianos_norte_occidente = (407/nrow(colonias_norte_occidente))*100
p_ancianos_norte_occidente

#Discapacitados
discapacitados_norte_occidente = unidades_norte_occidente %>% 
  filter(str_detect(nombre_act, 'discapacitados'))

#Creamos los buffers de las estancias para discapacitados
buffers_discapacitados_norte_occidente = st_buffer(discapacitados_norte_occidente, 500)

#Las proyecciones ya son iguales
st_crs(buffers_discapacitados_norte_occidente) == st_crs(colonias_norte_occidente)

#Intersecciones
int_discapacitados_norte_occidente = st_intersects(centroides_norte_occidente, buffers_discapacitados_norte_occidente)  %>% 
  sapply(length)

# Así se vería el panel
table(int_discapacitados_norte_occidente > 0)

#La probabilidad de tener acceso a una estancia para discapacitados en la región norte-occidente es 4.42%
p_discapacitados_norte_occidente = (262/nrow(colonias_norte_occidente))*100
p_discapacitados_norte_occidente

#Guarderías
guarderias_norte_occidente = unidades_norte_occidente %>% 
  filter(str_detect(nombre_act, 'Guarderías'))

#Creamos los buffers de las guarderías
buffers_guarderias_norte_occidente = st_buffer(guarderias_norte_occidente, 500)

#Las proyecciones ya son iguales
st_crs(buffers_guarderias_norte_occidente) == st_crs(colonias_norte_occidente)

#Intersecciones
int_guarderias_norte_occidente = st_intersects(centroides_norte_occidente, buffers_guarderias_norte_occidente)  %>% 
  sapply(length)

# Así se vería el panel
table(int_guarderias_norte_occidente > 0)

#La probabilidad de tener acceso a una estancia para guarderias en la región norte-occidente es 43.42%
p_guarderias_norte_occidente = (2572/nrow(colonias_norte_occidente))*100
p_guarderias_norte_occidente


# Centro-norte ------------------------------------------------------------
#Vector vacío
unidades_centro_norte = c()

#Unidades de cuidado en la región centro_norte
for (archivo in archivos_centro_norte) {
  df = readxl::read_xlsx(archivo) %>% 
    select (nombre_act, longitud, latitud) %>% 
    st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
  
  unidades_centro_norte = bind_rows(unidades_centro_norte, df)
  rm(df)
}

#Adultos mayores
ancianos_centro_norte = unidades_centro_norte %>% 
  filter(str_detect(nombre_act, 'ancianos'))

#Creamos los buffers de las estancias para ancianos
buffers_ancianos_centro_norte = st_buffer(ancianos_centro_norte, 500)

#Seleccionamos las colonias del centro-norte e igualamos su proyección a la de los buffers porque no era igual
colonias_centro_norte = colonias %>% 
  select(CVE_ENT, geometry) %>% 
  filter(CVE_ENT %in% c('01', '06', '14', '16', '24')) %>% 
  st_transform(crs = st_crs(buffers_ancianos_centro_norte)) %>% 
  st_make_valid()

#Calculamos los centroides de las colonias en esta región
centroides_centro_norte = st_centroid(colonias_centro_norte)

#Intersecciones
int_ancianos_centro_norte = st_intersects(centroides_centro_norte, buffers_ancianos_centro_norte)  %>% 
  sapply(length)

# Así se vería el panel
table(int_ancianos_centro_norte > 0)

#La probabilidad de tener acceso a una estancia para adultos mayores en la región centro-norte es 7.4%
p_ancianos_centro_norte = (900/nrow(colonias_centro_norte))*100
p_ancianos_centro_norte

#Discapacitados
discapacitados_centro_norte = unidades_centro_norte %>% 
  filter(str_detect(nombre_act, 'discapacitados'))

#Creamos los buffers de las estancias para discapacitados
buffers_discapacitados_centro_norte = st_buffer(discapacitados_centro_norte, 500)

#Las proyecciones ya son iguales
st_crs(buffers_discapacitados_centro_norte) == st_crs(colonias_centro_norte)

#Intersecciones
int_discapacitados_centro_norte = st_intersects(centroides_centro_norte, buffers_discapacitados_centro_norte)  %>% 
  sapply(length)

# Así se vería el panel
table(int_discapacitados_centro_norte > 0)

#La probabilidad de tener acceso a una estancia para discapacitados en la región centro-norte es 2.71%
p_discapacitados_centro_norte = (329/nrow(colonias_centro_norte))*100
p_discapacitados_centro_norte

#Guarderías
guarderias_centro_norte = unidades_centro_norte %>% 
  filter(str_detect(nombre_act, 'Guarderías'))

#Creamos los buffers de las guarderías
buffers_guarderias_centro_norte = st_buffer(guarderias_centro_norte, 500)

#Las proyecciones ya son iguales
st_crs(buffers_guarderias_centro_norte) == st_crs(colonias_centro_norte)

#Intersecciones
int_guarderias_centro_norte = st_intersects(centroides_centro_norte, buffers_guarderias_centro_norte)  %>% 
  sapply(length)

# Así se vería el panel
table(int_guarderias_centro_norte > 0)

#La probabilidad de tener acceso a una estancia para guarderias en la región centro-norte es 33.96%
p_guarderias_centro_norte = (4130/nrow(colonias_centro_norte))*100
p_guarderias_centro_norte

# Centro ------------------------------------------------------------------

#Vector vacío
unidades_centro = c()

#Unidades de cuidado en la región centro
for (archivo in archivos_centro) {
  df = readxl::read_xlsx(archivo) %>% 
    select (nombre_act, longitud, latitud) %>% 
    st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
  
  unidades_centro = bind_rows(unidades_centro, df)
  rm(df)
}

#Adultos mayores
ancianos_centro = unidades_centro %>% 
  filter(str_detect(nombre_act, 'ancianos'))

#Creamos los buffers de las estancias para ancianos
buffers_ancianos_centro = st_buffer(ancianos_centro, 500)

#Seleccionamos las colonias del centro e igualamos su proyección a la de los buffers porque no era igual
colonias_centro = colonias %>% 
  select(CVE_ENT, geometry) %>% 
  filter(CVE_ENT %in% c('09', '11', '13', '15', '17', '21', '22', '29')) %>% 
  st_transform(crs = st_crs(buffers_ancianos_centro)) %>% 
  st_make_valid()

#Calculamos los centroides de las colonias en esta región
centroides_centro = st_centroid(colonias_centro)

#Intersecciones
int_ancianos_centro = st_intersects(centroides_centro, buffers_ancianos_centro)  %>% 
  sapply(length)

# Así se vería el panel
table(int_ancianos_centro > 0)

#La probabilidad de tener acceso a una estancia para adultos mayores en la región centro es 8.61%
p_ancianos_centro = (1849/nrow(colonias_centro))*100
p_ancianos_centro

#Discapacitados
discapacitados_centro = unidades_centro %>% 
  filter(str_detect(nombre_act, 'discapacitados'))

#Creamos los buffers de las estancias para discapacitados
buffers_discapacitados_centro = st_buffer(discapacitados_centro, 500)

#Las proyecciones ya son iguales
st_crs(buffers_discapacitados_centro) == st_crs(colonias_centro)

#Intersecciones
int_discapacitados_centro = st_intersects(centroides_centro, buffers_discapacitados_centro)  %>% 
  sapply(length)

# Así se vería el panel
table(int_discapacitados_centro > 0)

#La probabilidad de tener acceso a una estancia para discapacitados en la región centro es 5.29%
p_discapacitados_centro = (1137/nrow(colonias_centro))*100
p_discapacitados_centro

#Guarderías
guarderias_centro = unidades_centro %>% 
  filter(str_detect(nombre_act, 'Guarderías'))

#Creamos los buffers de las guarderías
buffers_guarderias_centro = st_buffer(guarderias_centro, 500)

#Las proyecciones ya son iguales
st_crs(buffers_guarderias_centro) == st_crs(colonias_centro)

#Intersecciones
int_guarderias_centro = st_intersects(centroides_centro, buffers_guarderias_centro)  %>% 
  sapply(length)

# Así se vería el panel
table(int_guarderias_centro > 0)

#La probabilidad de tener acceso a una estancia para guarderias en la región centro es 36.17%
p_guarderias_centro = (7772/nrow(colonias_centro))*100
p_guarderias_centro


# Sur ---------------------------------------------------------------------
# Vector vacío
unidades_sur = c()

# Unidades de cuidado en la región sur
for (archivo in archivos_sur) {
  df = readxl::read_xlsx(archivo) %>% 
    select (nombre_act, longitud, latitud) %>% 
    st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
  
  unidades_sur = bind_rows(unidades_sur, df)
  rm(df)
}

#Adultos mayores
ancianos_sur = unidades_sur %>% 
  filter(str_detect(nombre_act, 'ancianos'))

#Creamos los buffers de las estancias para ancianos
buffers_ancianos_sur = st_buffer(ancianos_sur, 500)

#Seleccionamos las colonias del sur e igualamos su proyección a la de los buffers porque no era igual
colonias_sur = colonias %>% 
  select(CVE_ENT, geometry) %>% 
  filter(CVE_ENT %in% c('04', '07', '12', '20', '23', '27', '30', '31')) %>% 
  st_transform(crs = st_crs(buffers_ancianos_sur)) %>% 
  st_make_valid()

#Calculamos los centroides de las colonias en esta región
centroides_sur = st_centroid(colonias_sur)

#Intersecciones
int_ancianos_sur = st_intersects(centroides_sur, buffers_ancianos_sur)  %>% 
  sapply(length)

# Así se vería el panel
table(int_ancianos_sur > 0)

#La probabilidad de tener acceso a una estancia para adultos mayores en la región sur es 4.46%
p_ancianos_sur = (843/nrow(colonias_sur))*100
p_ancianos_sur

#Discapacitados
discapacitados_sur = unidades_sur %>% 
  filter(str_detect(nombre_act, 'discapacitados'))

#Creamos los buffers de las estancias para discapacitados
buffers_discapacitados_sur = st_buffer(discapacitados_sur, 500)

#Las proyecciones ya son iguales
st_crs(buffers_discapacitados_sur) == st_crs(colonias_sur)

#Intersecciones
int_discapacitados_sur = st_intersects(centroides_sur, buffers_discapacitados_sur)  %>% 
  sapply(length)

# Así se vería el panel
table(int_discapacitados_sur > 0)

#La probabilidad de tener acceso a una estancia para discapacitados en la región sur es 2.66%
p_discapacitados_sur = (503/nrow(colonias_sur))*100
p_discapacitados_sur

#Guarderías
guarderias_sur = unidades_sur %>% 
  filter(str_detect(nombre_act, 'Guarderías'))

#Creamos los buffers de las guarderías
buffers_guarderias_sur = st_buffer(guarderias_sur, 500)

#Las proyecciones ya son iguales
st_crs(buffers_guarderias_sur) == st_crs(colonias_sur)

#Intersecciones
int_guarderias_sur = st_intersects(centroides_sur, buffers_guarderias_sur)  %>% 
  sapply(length)

# Así se vería el panel
table(int_guarderias_sur > 0)

#La probabilidad de tener acceso a una estancia para guarderias en la región sur es 29.27%
p_guarderias_sur = (5528/nrow(colonias_sur))*100
p_guarderias_sur

# Tabla de probabilidades por región ----

# Creamos un vector de las regiones
regiones = c("centro", "centro-norte", "norte", "norte-occidente","sur") 
regiones
# Creamos vectores con las probabilidades de acceso por región
# Vector de probabilidad de acceso a estancias para adultos mayores por regiones
p_ancianos = c(p_ancianos_centro,p_ancianos_centro_norte, p_ancianos_norte,
               p_ancianos_norte_occidente,p_ancianos_sur)

# Vector de probabilidad de acceso a estancias para discapacitados y adultos mayores por regiones
p_discapacitados = c(p_discapacitados_centro,p_discapacitados_centro_norte, p_discapacitados_norte,
                     p_discapacitados_norte_occidente,p_discapacitados_sur)

# Vector de probabilidad de acceso a estancias infantiles
p_guarderias = c(p_guarderias_centro, p_guarderias_centro_norte, p_guarderias_norte,
                 p_guarderias_norte_occidente, p_guarderias_sur)

# Unimos los vectores en una tabla
probabilidades_acceso = tibble(region = regiones,
                               p_ancianos = p_ancianos,
                               p_discapacitados = p_discapacitados,
                               p_guarderias = p_guarderias  )
probabilidades_acceso

# Guardamos las probabilidades en csv
write.csv(probabilidades_acceso, file = "Probabilidades de acceso a estancias por región.csv", row.names = FALSE)
