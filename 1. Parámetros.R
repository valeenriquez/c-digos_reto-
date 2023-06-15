# Fijar el directorio de trabajo----- 
setwd("~/Documents/Ciencia de Datos/Reto")

# Librerías ----
library(DBI)
library(tidyverse)
library(dplyr)

# Introducción ----
# A tráves de los datos del INEGI publicados en la ENUT (Encuesta 
# del uso del tiempo) y la ENOE (Encuesta Nacional de Ocupación y Empleo) para 2019,
# se seleccionaron una serie de variables que ayuden a ilustrar la situación de 
# México con respecto a los servicios de cuidado. Se seleccionaron variables que 
# permitan calcular el porcentaje de personas que trabajan, personas que no pueden trabajar
# por tener que cuidar a alguien, personas que tuvieron que cuidar a alguien con una discaapacidad,
# número de adultos mayores, porcentaje de personas en el sector informal, hogares con hijos entre 0 y
# 4 años y hogares que contratan servicio de cuidados 
# Cada uno de estos parámetros fue filtrado por región y sexo.
# Bases de datos ENOE ----------------------------------------------------------

# En esta sección del código se cargan las tablas que se necesitan para variables que 
# provienen de la ENOE

# Estas tablas se usan para la variables de : 
# 1) Personas que necesitan trabajar y no pueden por cuidados
# 2) Personas ocupadas
# 3)Personas que trabajan en el sector informal

reto_db = dbConnect(RSQLite::SQLite(), "reto.db")
dbListTables(reto_db)

#Cargamos cada una de las tablas que se necesitan para calcular los parámetros
#Tabla sociodemográfica de la ENOE 2019
dbWriteTable(
  conn = reto_db,
  name = 'enoe_socio',
  value = read_csv('sdemt419.csv')
)
#Cuestionario de ocupación y empleo parte I de la ENOE 2019
dbWriteTable(
  conn = reto_db,
  name = 'enoe1',
  value = read_csv('coe1t419.csv')
)

#Limpiamos los nombres de las variables
enoe_socio = tbl(reto_db, 'enoe_socio') %>% 
  janitor::clean_names()

enoe1 = tbl(reto_db, 'enoe1') %>% 
  janitor::clean_names()

#Dividimos por región la tabla sociodemográfica
enoe_reg = enoe_socio %>% 
  mutate(region = case_when(
    ent %in% c(2, 5, 8, 19, 26, 28) ~ 'norte',
    ent %in% c(3, 10, 18, 25, 32) ~ 'norte-occidente',
    ent %in% c(1, 6, 14, 16, 24) ~ 'centro-norte',
    ent %in% c(9, 11, 13, 15, 17, 21, 22, 29) ~ 'centro',
    ent %in% c(4, 7, 12, 20, 23, 27, 30, 31) ~ 'sur'
  )
  )



# Bases de datos ENUT -----------------------------------------------------

# Estas tablas se usan para la variables de : 
# 1) Personas que cuidan adultas mayores
# 2)Hogares con hijos entre 0 y 4 a años 
# 3) Hogares que contratan servicios de cuidados
# 4) Hogares con una persona con discapacidad 

# ENUT, datos de la tabla "módulo" 
dbWriteTable(
  conn = reto_db,
  name = 'enut_modulo',
  value = read_csv('TMODULO.csv')
)

# ENUT,  datos de la tabla  "demográfico" 
dbWriteTable(
  conn = reto_db,
  name = 'enut_socio',
  value = read_csv('TSDEM.csv')
)

# ENUT, datos de la tabla "vivienda" 
dbWriteTable(
  conn = reto_db,
  name = 'enut_vivienda',
  value = read_csv('TVIVIENDA.csv')
)

# ENUT, datos de la tabla "hogar" 
dbWriteTable(
  conn = reto_db,
  name = 'enut_hogar',
  value = read_csv('THOGAR.csv')
)

#Creamos un objeto para cada una de las tablas que cargamos y limpiamos sus nombres
# Tabla enut socio 
enut_socio = tbl(reto_db, 'enut_socio') %>% 
  janitor::clean_names()

# Tabla enut vivienda
enut_vivienda = tbl(reto_db, 'enut_vivienda') %>% 
  janitor::clean_names() 

#Tabla enut "módulo" 

enut_modulo = tbl(reto_db, 'enut_modulo') %>% 
  janitor::clean_names()

# Tabla enut "hogar"
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names()
# Regiones
# Añadimos variable región para la tabla de los datos modulo
enut_reg = enut_modulo %>% 
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte',
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente',
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte',
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro',
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  )



# Porcentaje de población en edad de trabajar ENOE ------------------------
# Sacamos la variable de personas en edad de trabajar con las tablas de la ENOE
sexos = enoe_socio %>% 
  #Seleccionamos el sexo, factor de expansión y edad
  select (sex, fac, eda) %>% 
  # Agrupamos por sexo
  group_by(sex) %>% 
  # Quitamos a quienes no declararon su sexo/edad y dejamos a las personas de 15 años y más
  filter(sex != 'NA',
         eda >= 15,
         eda != 98, 
         eda != 99) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_nacional = sum(fac)
  ) %>% 
  # Calculamos el pocentaje de hombres/mujeres de 15 años y más
  mutate(
    porcentaje = round(total_nacional/sum(total_nacional)*100, digits = 2),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')))

#recolectamos el resultado
sexos_enoe = collect(sexos)

#Porcentaje de hombres y mujeres de 15 años y más 
sexos_enoe

#Sacamos la personas en edad de trabajar filtrado por región 
sexos_reg = enoe_reg %>% 
  #Seleccionamos el sexo, el factor, la región y la edad
  select (sex, fac, region, eda) %>%
  #Agrupamos por sexo y región
  group_by(sex, region) %>% 
  # quitamos a quienes no declararon su sexo/edad y dejamos a las personas de 15 años y más
  filter(sex != 'NA',
         eda >= 15,
         eda != 98, 
         eda != 99) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_reg = sum(fac)
  ) %>% 
  #creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = round(total_reg / sum(total_reg) * 100, digits = 2 ),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')))


sexos_reg_enoe = collect(sexos_reg)

#Porcentaje de hombres y mujeres por región
sexos_reg_enoe

# Porcentaje de poblacion en edad de trabajar ENUT----
# Sacamos la variable de personas en edad de trabajar con las tablas de la ENUT 
sexos = enut_modulo %>% 
  #Seleccionamos el sexo, el factor y la edad
  select (sexo, fac_per, edad_v) %>% 
  # Agrupamos por sexo
  group_by(sexo) %>% 
  # quitamos a quienes no declararon su sexo/edad y dejamos a las personas de 15 años y más
  filter(sexo != 'NA',
         edad_v >= 15) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_nacional = sum(fac_per)
  ) %>% 
  #creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = round(total_nacional/sum(total_nacional)*100, digits = 2),
    sexo = ifelse(sexo == 1,c("Hombres"),c('Mujeres')))

sexos_ENUT = collect(sexos)

#Porcentaje de hombres y mujeres de 15 años y más 
sexos_ENUT

#Calculamos el porcentaje a nivel regional
sexos_reg = enut_reg %>% 
  # Seleccionamos sexo, factor, region y edad
  select (sexo, fac_per, region, edad_v) %>%
  # Agrupamos por región
  group_by(sexo, region) %>% 
  # quitamos a quienes no declararon su sexo/edad y dejamos a las personas de 15 años y más
  filter(sexo != 'NA',
         edad_v >= 15) %>% 
  # Calculamos el total de personas representadas por el factor
    summarise(
    total_reg = sum(fac_per)
  ) %>% 
  #creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = round(total_reg / sum(total_reg) * 100, digits = 2 ),
    sexo = ifelse(sexo == 1,c("Hombres"),c('Mujeres')))

sexos_reg_enut = collect(sexos_reg) %>% 
  #renombramos la variable sexo para poder unir las tablas posteriormente
  rename(sex = sexo)


#Porcentaje de hombres y mujeres de 15 años y más por región
sexos_reg_enut

# Informalidad ------------------------------------------------------------
#Informalidad Nacional
# Modificamos la tabla sociodemográfica de la ENOE
# Se calcula el porcentaje nacional de personas en el sector informal 
informalidad_nacional = enoe_socio %>% 
  # Seleccionamos las variables, entidad, sexo, clasificación de empleo y factor
  select(ent,sex,emp_ppal,fac) %>% 
  #Agrupamos por sexo
  group_by(sex) %>% 
  # quitamos a las personas que tienen una clasificación de empleo diferente al informal
  filter(emp_ppal == 1
  ) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_nacional_inf = sum(fac)
  ) %>% 
  #Creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = total_nacional_inf/sum(total_nacional_inf),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')),
    porcentaje = round(porcentaje*100, digits = 2))

Informalidad_nacional = collect(informalidad_nacional) 

#Porcentaje nacional de personas en el sector informal 
Informalidad_nacional

#Porcentaje sobre el total de mujeres y hombres en informalidad respecto a
#las personas en edad de trabajr
Porcentaje_informal_nacional = left_join(sexos_enoe, Informalidad_nacional, 
                                         by = 'sex') %>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          porcentaje = round((total_nacional_inf / total_nacional)*100, digits = 2))
Porcentaje_informal_nacional

#Sacamos el porcentaje de personas en el sector informal por region y por sexo
# Utilizando la tabla enoe_reg que se definió en las tablas ENOE
Informalidad = enoe_reg %>% 
  # Seleccionamos la clasificación de empleo, sexo, factor y región
  select (emp_ppal, sex, fac, region) %>% 
  # Agrupamos por sexo y región
  group_by(sex, region) %>% 
  # quitamos a las personas que tienen una clasificación de empleo diferente al informal
  filter(emp_ppal == 1 
  ) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total = sum(fac)
  ) %>% 
  #Creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = total/sum(total),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')),
    porcentaje = round(porcentaje*100, digits = 2))

#Informalidad regional 
Informalidad = collect(Informalidad) 
Informalidad

# Obtenemos el porcentaje regional de mujeres y hombres en informalidad
# respecto al total nacional de personas a la población en edad de trabajar
informal_reg = left_join(sexos_reg_enoe, Informalidad,
                         by = c('sex','region')
) %>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          sexo.x = NULL, 
          sexo.y = NULL,
          porcentaje = round((total / total_reg)*100, digits = 2))

informal_reg

# Personas que necesitan trabajar pero no pueden por cuidar a alguien ----

#Personas que necesitan trabajar pero no pueden por cuidar a alguien (nacional)
#Cargamos el cuestionario 1 de la ENOE

enoe1T = enoe1 %>% 
  # Seleccionamos las variables: Ciudad autorrepresentada, control, hogar mudado,
  # entidad, vivienda seleccionada, número de hogar , número de renglón,
  # actualmente tiene necesidad de trabajar, ¿Hay alguna otra razón, además de ser por la que 
  # no esté buscando trabajo?, Opciones de respuesta cuando P2G1=1 y factor
  select(cd_a, con, h_mud, ent, v_sel, n_hog, n_ren, p2f, p2g1, p2g2, fac)

# Cargamos la tabla sociodemográfica de la ENOE
enoe_socioT = enoe_socio%>% 
  # Seleccionamos las variables: Ciudad autorrepresentada, control, hogar mudado,
  # entidad, sexo, vivienda seleccionada, número de hogar , número de renglón
  select(cd_a, con, h_mud, ent, sex, v_sel, n_hog, n_ren)


# Unimos las tablas
enoe1_socio = left_join(enoe1T, enoe_socioT,
                        by = c('cd_a', 'con', 'h_mud', 'ent', 
                               'v_sel','n_hog', 'n_ren'))


# Calculamos el porcentaje de personas que quieren trabajar pero no
# pueden porque realizan cuidados por sexo
no_empleo_cuidados = enoe1_socio %>% 
  # Seleccionamos las variables: actualmente tiene necesidad de trabajar, ¿Hay alguna otra razón, además de ser por la que 
  # no esté buscando trabajo?, Opciones de respuesta cuando P2G1=1, sexo y factor
  select (p2f, p2g1, p2g2, sex, fac) %>% 
  # Agrupamos por sexo
  group_by(sex) %>% 
  # nos quedamos únicamente con las personas quieren trabajar pero no pueden por 
  # realizar servicios de cuidados
    filter(p2f != 3, p2f != 9,
         p2g1 == 1, 
         p2g2 == 9
  ) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_no_empleo_cuidados = sum(fac)
  ) %>% 
  #Creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = round( total_no_empleo_cuidados/sum(total_no_empleo_cuidados) *100 , digits = 2 ),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')))



no_empleo_cuidados = collect(no_empleo_cuidados)

#Porcentaje de hombres/mujeres que quieren/necesitan trabajar, 
#pero no pueden porque realizan servicios de cuidados
no_empleo_cuidados

#Porcentaje sobre el total de mujeres y hombres en edad de trabajar
no_empleo_cuidados_nacional = left_join(sexos_enoe, no_empleo_cuidados, 
                                        by = 'sex') %>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          porcentaje = round((total_no_empleo_cuidados / total_nacional)*100, digits = 2))
no_empleo_cuidados_nacional

#Añadimos regiones
enoe1_socio_reg = enoe1_socio %>% 
  mutate(region = case_when(
    ent %in% c(2, 5, 8, 19, 26, 28) ~ 'norte',
    ent %in% c(3, 10, 18, 25, 32) ~ 'norte-occidente',
    ent %in% c(1, 6, 14, 16, 24) ~ 'centro-norte',
    ent %in% c(9, 11, 13, 15, 17, 21, 22, 29) ~ 'centro',
    ent %in% c(4, 7, 12, 20, 23, 27, 30, 31) ~ 'sur'
  )
  )

#Calculamos el porcentaje de que una persona quiera trabajar pero no pueda
#porque realiza servicios de cuidado por sexo y región
no_empleo_cuidados_reg= enoe1_socio_reg%>% 
  # Seleccionamos las variables: actualmente tiene necesidad de trabajar, ¿Hay alguna otra razón, además de ser por la que 
  # no esté buscando trabajo?, Opciones de respuesta cuando P2G1=1, sexo, factor y región
  select (p2f, p2g1, p2g2, sex, fac, region) %>% 
  # Agrupamos por sexo y región
  group_by(sex, region) %>% 
  # nos quedamos únicamente con las personas quieren trabajar pero no pueden por 
  # realizar servicios de cuidados
  filter(p2f != 3, p2f != 9,
         p2g1 == 1,
         p2g2 == 9
  ) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_no_empleo_cuidados = sum(fac)
  ) %>% 
  #Creamos una variable para el porcentaje y sexo de las personas
   mutate(
    porcentaje = round(total_no_empleo_cuidados/sum(total_no_empleo_cuidados) * 100, digits = 2),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')))


no_empleo_cuidados_reg = collect(no_empleo_cuidados_reg)

#Porcentaje de hombres/mujeres que quieren/necesitan trabajar, 
#pero no pueden porque realizan servicios de cuidados por región
no_empleo_cuidados_reg

#Porcentaje sobre el total de mujeres y hombres por región
no_empleo_cuidados_total_reg = left_join(sexos_reg_enoe, no_empleo_cuidados_reg, 
                                         by = c('sex', 'region')) %>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          porcentaje = round((total_no_empleo_cuidados / total_reg)*100, digits = 4))
no_empleo_cuidados_total_reg


# Hijos -----

# Calculamos el número de hogares en México con la ENUT
# De la ENUT vivienda Seleccionamos las variables: entidad, vivienda seleccionada y la UPM
enut_vivienda = tbl(reto_db, 'enut_vivienda') %>% 
  janitor::clean_names() %>% 
  select(ent,viv_sel, upm)

# De la ENUT sociodemográfica Seleccionamos las variables: entidad, vivienda seleccionada y la UPM
enut_socio = tbl(reto_db, 'enut_socio') %>% 
  janitor::clean_names() %>% 
  select(viv_sel, hogar, upm) %>% 
  distinct()

#Creamos la variable de region
# llamamos la ENUT hogar
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  #Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  #Creamos la varaible región
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte',
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente',
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte',
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro',
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  ) %>% 
  # Agrupamos por región
  group_by(region) %>% 
  # Calculamos el número de hogares representados por el factor
  summarise(total= sum(fac_hog))

# Obtenemos el total de hogares por región 

total_hog = collect(enut_hogar)
total_hog

# Se agrupa por edad los hogares, para que nos de el 
# número de hogares con hijos en el rango de edad entre 0 y 4

enut_socio = tbl(reto_db, 'enut_socio') %>% 
  janitor::clean_names() %>% 
  # Filtramos solo a los que su parentezco es hijo (a).
  filter( paren == 3.0) %>% 
  # Quitamos NA
  filter(
    edad == '00'|edad == '01'| edad == '02'|edad == '03'|edad == '04' 
  ) %>% 
  # Agrupamos por edad
  group_by(edad) %>% 
  # Seleccionamos la vivienda selccionada, el número de hogar, la UPM y el factor hogar
  select(viv_sel, hogar, upm, fac_hog) %>% 
  distinct()

# Sacamos el porcentaje nacional de hogares con hijos de 0 a 4 años
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  # Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  # Agrupamos por edad
  group_by(edad) %>% 
  # Calculamos el número de hogares representados por el factor
  summarise (total = sum(fac_hog))%>% 
    mutate(
    porcentaje = total/sum(total),
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 

hijos = collect(enut_hogar)

# Porcentaje nacional de hogares con hijos de 0 a 4 años 
hijos


# Añadimos la variable de región para esta variable de hijos 
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  # Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  #Creamos la varaible región
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte',
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente',
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte',
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro',
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  ) %>% 
  # Agrupamos por región
  group_by(region) %>% 
  # Calculamos los hogares representados por el factor
  summarise(total = sum(fac_hog))%>% 
  #Creamos una variable para el porcentaje
  mutate(
    porcentaje = total/sum(total),
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 

hijos_region = collect(enut_hogar)

# Porcentaje de hogares con hijos de 0 a 4 años 
hijos_region

# Probabilidad 
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  # Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  # Agrupamos por la edad
  group_by(edad) %>% 
  # Obtenemos el total de hogares representadas por el factor
  summarise (total = sum(fac_hog))%>% 
  #Creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = total/36622284,
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 


hijos = collect(enut_hogar)

# Porcentaje nacional de hogares con hijos de personas entre 0 a 4 años 
hijos

# Realizamos el porcentaje para cada una de las regiones
# Region Norte
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>%
  # Unimos las tablas sociodemográfica y de vivienda a la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  # Generamos la variable región
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte'
  )
  ) %>% 
  # Quitamos los hogares sin región
  filter (region != "NA") %>% 
  # Agrupamos por región y edad
  group_by(region, edad) %>% 
  # Calculamos el número de hogares representados por el factor
  summarise(total = sum(fac_hog))%>% 
  #Creamos una variable para el porcentaje
  mutate(
    porcentaje = total/6835417,
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 


hijos_region_norte = collect(enut_hogar)

# Porcentaje de hogares en el norte con hijos de 0 a 4 años 
hijos_region_norte


# Región Norte Occidente 
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  #Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  #Creamos la varaible región
  mutate(region = case_when(
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente'
  )
  ) %>% 
  # Quitamos los hogares sin región
  filter (region != "NA") %>% 
  # Agrupamos por región y edad
  group_by(region, edad) %>% 
  # Obtenemos el número de hogares representados por el factor
  summarise(total = sum(fac_hog))%>% 
  # v una variable para el porcentaje
  mutate(
    porcentaje = total/2595337,
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 


hijos_region_norte_occidente = collect(enut_hogar)

#Porcentaje de hogares en el norte-occidente con hijos de 0 a 4 años 
hijos_region_norte_occidente


# Region Centro Norte
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  #Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>%
  #Creamos la variable región
  mutate(region = case_when(
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte'
    )
  ) %>% 
  # quitamos los hogares sin región
  filter (region != "NA") %>% 
  # Agrupamos por región y edad
  group_by(region, edad) %>%
  # Obtenemos el número de hogares representados por el factor
  summarise(total = sum(fac_hog))%>% 
  #Creamos una variable para el porcentaje
  mutate(
    porcentaje = total/5294417,
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 


hijos_region_centro_norte = collect(enut_hogar)
#Porcentaje de hogares en el centro norte con hijos de 0 a 4 años 
hijos_region_centro_norte


# Region centro
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  #Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  #Creamos la variable región
  mutate(region = case_when(
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro'
  )
  ) %>% 
  # quitamos los hogares sin región
  filter (region != "NA") %>% 
  # Agrupamos por región y edad
  group_by(region, edad) %>% 
  # Obtenemos el número de hogares representados por el factor
  summarise(total = sum(fac_hog))%>% 
  #Creamos una variable para el porcentaje
  mutate(
    porcentaje = total/13639347,
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 


hijos_region_centro = collect(enut_hogar)
# Porcentaje de hogares en el centro con hijos de 0 a 4 años 
hijos_region_centro



# Region Sur
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names() %>% 
  #Unimos las tablas sociodemográfica y de vivienda la tabla hogar
  inner_join(enut_socio) %>% 
  inner_join(enut_vivienda) %>% 
  mutate(region = case_when(
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  ) %>% 
  # quitamos los hogares sin región
  filter (region != "NA") %>% 
  # Agrupamos por región y edad
  group_by(region, edad) %>% 
  # Obtenemos el número de hogares representados por el factor
  summarise(total = sum(fac_hog))%>% 
  #Creamos una variable para el porcentaje
  mutate(
    porcentaje = total/13639347,
    porcentaje=round(porcentaje*100, digits = 2), 
  ) 


hijos_region_sur = collect(enut_hogar)
#Porcentaje de hogares en el sur con hijos de 0 a 4 años 
hijos_region_sur


# Adultos Mayores SQL -----------------------------------------------------

# Personas que tuvieron que cuidar a un adulto mayor en la última semana nacional
# Seleccionamos las variables: entidad, sexo, factor persona y si la persona tuvo que cuidar 
# a un adulto mayor en la última semana 
Adultos_M_nacional = enut_reg %>% 
  select(ent, sexo, fac_per, p6_16_6) %>% 
  # Agrupamos por sexo
  group_by(sexo) %>% 
  # Seleccionamos solo a las personas que tuvieron que cuidar a un adulto mayor 
  # en la última semana
  filter(
    p6_16_6 == 1
  ) %>% 
  # Obtenemos el número que de hogares representados por el factor
    summarise(
    total_nacional_A = sum(fac_per)
  ) %>% 
  #Creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = total_nacional_A/sum(total_nacional_A),
    sexo = ifelse(sexo == 1,c("Hombres"),c('Mujeres')),
    porcentaje = round(porcentaje*100, digits = 2),
  )

Adultos_M_nacional = collect(Adultos_M_nacional)

# Porcentaje de persoans que cuidan a un adulto mayor
Adultos_M_nacional  

porcenaje_adultos_nacional = left_join(sexos_ENUT, Adultos_M_nacional,
                                       by = c('sexo'))%>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          sexo.x = NULL, 
          sexo.y = NULL,
          porcentaje = round((total_nacional_A / total_nacional)*100, digits = 2))
porcenaje_adultos_nacional


#Adultos por región
Adultos_M_regional = enut_reg %>% 
  # Seleccionamos las variables: sexo, factor persona, si la persona tuvo que cuidar 
  # a un adulto mayor en la última semana y región
  select(sexo, fac_per, p6_16_6, region) %>% 
  # Agrupamos por sexo y región
  group_by(sexo, region) %>% 
  # Seleccionamos solo a las personas que tuvieron que cuidar a un adulto mayor 
  # en la última semana
  filter(
    p6_16_6 == 1
  ) %>% 
  # Obtenemos el número que de hogares representados por el factor
  summarise(
    total = sum(fac_per)
  ) %>% 
  # Creamos una variable para el porcentaje y sexo de las personas
  mutate(
    porcentaje = total/sum(total),
    sexo = ifelse(sexo == 1,c("Hombres"),c('Mujeres')),
    porcentaje = round(porcentaje*100, digits = 2),
  )

Adultos_M_regional = collect(Adultos_M_regional) %>% 
  rename(sex = sexo)

# Porcentaje de adultos mayores con region
Adultos_M_regional  

# Con respecto al total nacional 
adultos_regional = left_join(sexos_reg_enut, Adultos_M_regional,
                                       by = c('sex','region'))%>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          sexo.x = NULL, 
          sexo.y = NULL,
          porcentaje = round((total / total_reg)*100, digits = 2))  
  

adultos_regional

# Discapacidad SQL------------------------------------------------------------
# Hogares con una persona con discapacidad en México

# Cargamos la ENUT vivienda
enut_vivienda = tbl(reto_db, 'enut_vivienda') %>% 
  janitor::clean_names()

# Cargamos la ENUT hogar
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  janitor::clean_names()

# Cargamos la ENUT sociodemográfica
enut_socio = tbl(reto_db, 'enut_socio') %>% 
  janitor::clean_names()

# Hacemos la variable de total de hogares nacional
# Unimos tablas
enutH = left_join(enut_vivienda, enut_hogar, 
                  by = c('upm', 'viv_sel'))

# Agrupamos por Región y sumamos el Factor Hogar
hogares_nacional = enutH %>% 
  summarise(
    total_hogares_nacional = sum(fac_hog)
  )
hogares_nacional = collect(hogares_nacional)
# Número total de hogares en México 
hogares_nacional

# Hacemos la variable de hogares regionales
# dividimos por región
enut_regH = enutH %>%
  #Creamos la variable región
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte',
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente',
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte',
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro',
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  )

#Agrupamos por Región y sumamos el Factor Hogar
hogares_reg = enut_regH %>% 
  # Agrupamos por región
  group_by(region)%>% 
  # Obtenemos los hogares representados por el factor
  summarise(
    hogares_regionales = sum(fac_hog)
  )
hogares_reg = collect(hogares_reg)
# Número de hogares por region
hogares_reg

# Hogares con una persona con discapacidad nacional
enut_socioD = enut_socio %>% 
  #Filtramos la variable p3_11b (personas con discapacidad por hogar)
  # 1) == 1 Enfermedad Crónica
  # 2)  == 2 Limitación Física
  # 3) == 3 Limitación Mental
  filter(p3_11b == 1 | p3_11b == 2 |  p3_11b == 3) %>% 
  # Seleccionamos: vivienda seleccionada, número de hogar y UPM
  select(viv_sel, hogar, upm) %>% 
  distinct()

# Seleccionamos de la ENUT vivienda la variable entidad, vivienda seleccionada y UPM
enut_viviendaD = enut_vivienda %>% 
  select(ent,viv_sel, upm)

# Hogares nacionales con una persona con discapacidad
Hogares_discapacidad_nacional = enut_hogar %>%  
  # Unimos las tablas filtradas de ENUT socio y ENUT vivienda
  inner_join(enut_socioD) %>% 
  inner_join(enut_viviendaD) %>% 
  # Calculamos el número de hogares que representados por el factor
  summarise(hogares_con_discapacidad = sum(fac_hog))

Hogares_discapacidad_nacional = collect(Hogares_discapacidad_nacional)
# Porcentaje de hogares con al menos una persona con discapacidad
Hogares_discapacidad_nacional

# Sacamos el porcentaje con respecto al toatl de hogares nacional
Probabilidad_discapacidad = cross_join(hogares_nacional, Hogares_discapacidad_nacional,
) %>% 
  mutate(
    porcentaje = (hogares_con_discapacidad / total_hogares_nacional)*100)
Probabilidad_discapacidad

# Hogares con discapacidad por región

enut_hogarD = enut_hogar %>%  
  # Unimos las tablas
  inner_join(enut_socioD) %>% 
  inner_join(enut_viviendaD) %>% 
  # Agregamos variable de región
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte',
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente',
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte',
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro',
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  ) %>% 
  # Agrupamos por región
  group_by(region) %>% 
  # Obtenemos el número de hogares representados por el factor
  summarise(hogares_con_discapacidad = sum(fac_hog))

# Número de hogares por región con una persona discapacitada
enut = collect(enut_hogarD)
enut

# Porcentaje de hogares con una persona con discapacidad respecto al total nacional
probabilidad_discapacidad_reg = left_join(hogares_reg, enut,
                                      by = c('region')) %>% 
  #Creamos la variable porcentaje
  mutate(
    porcentaje = (hogares_con_discapacidad / hogares_regionales)*100)

# Porcentaje de hogares con al menos una persona con discapacidad con 
# reespecto a numero de hogares por región
probabilidad_discapacidad_reg

# Trabajo SQL ----------------
#Personas que trabajan a nivel nacional
#Utilizamos la tabla ENOE sociodemográfica

personas_trabajan = enoe_socio %>% 
  # Seleccionamos las variables:
  #       clase2== 1, personas que son parte de población ocupada
  #       r_def == 00 , entrevistas completas
  #       c_res = 1, residente habitual 
  #       c_res = 3, nuevo residente 
  #       edad mayor a 15, que esta en edad de trabajar, 
  #       eda != 98, 99 para personas de 97 años o mas 
  select(ent, sex, clase2, fac, r_def,c_res, eda) %>% 
  #Agrupamos por sexo
  group_by(sex) %>% 
  # Nos quedamos únicamente con las personas que trabajan 
  filter(
    clase2 == 1,
    r_def == 00,
    c_res == 1 | 3, 
    eda >= 15,
    eda != 99,
    eda !=98
  ) %>% 
  # Obtenemos el número de personas represetadas por el factor
  summarise(
    total_personas_trabajan = sum(fac)
  ) %>% 
  # Creamos la variable porcentaje
  mutate(
    porcentaje = round( total_personas_trabajan/sum(total_personas_trabajan) *100 , digits = 2 ),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')))

porcentaje_trabajo= collect(personas_trabajan)
# Porcentaje de personas que trabajan 
porcentaje_trabajo

# Porcentaje sobre el total de mujeres y hombres en edad de trabajar
trabajo_nacional = left_join(sexos_enoe, porcentaje_trabajo, 
                             by = 'sex') %>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          porcentaje = round((total_personas_trabajan / total_nacional)*100, digits = 2))

trabajo_nacional

# Porcentaje de personas ocupadas filtrado por region 
# Utilizamos la tabla sociodemográfica de la ENOE dividida por regiones
trabajo_regional= enoe_reg %>% 
  # Agrupamos por sexo y región
  group_by(sex, region) %>% 
  # únicamente nos quedamos con las personas ocupadas
  filter(
    clase2==1,
    r_def == 00,
    c_res == 1 | 3, 
    eda >= 15, 
    eda !=99,
    eda != 98
  ) %>% 
  # Calculamos el total de personas representadas por el factor
  summarise(
    total_trabajos= sum(fac)
  ) %>% 
  # Generamos la variable porcetaje
  mutate(
    porcentaje = round(total_trabajos/sum(total_trabajos) * 100, digits = 2),
    sex = ifelse(sex == 1,c("Hombres"),c('Mujeres')))



trabajo_reg = collect(trabajo_regional)
# Personas ocupadas a nivel regional
trabajo_reg

#Porcentaje  de personas ocupadas sobre el total de mujeres y hombres por región
personas_trabajan_reg= left_join(sexos_reg_enoe, trabajo_reg, 
                                 by = c('sex', 'region')) %>% 
  mutate (porcentaje.x = NULL,
          porcentaje.y = NULL,
          porcentaje = round((total_trabajos / total_reg)*100, digits = 4))
personas_trabajan_reg


# Servicio de cuidadoras--------
#Llamamos la ENUT hogar
enut_hogar = tbl(reto_db, 'enut_hogar') %>% 
  # Limpiamos la base de datos
  janitor::clean_names() %>% 
  # Seleccionamos las variables UPM, vivienda seleccionada, la cantidad de hogares que si contratan servicios de cuidados, la cantidad de trabajadoras, el promedio de horas trabajadas, factor hogar y los hogares  
  select(upm, viv_sel,p2_6_3, p2_7_3, p2_8_3,fac_hog, hogar)  

#Llamamos la ENUT vivienda
enut_vivienda = tbl(reto_db, 'enut_vivienda') %>% 
  #Limpiamos la base de datos
  janitor::clean_names() %>% 
  # Seleccionamos las variables UPM, Vivienda selecionada y entidad
  select(upm, viv_sel, ent)

#Unimos las tablas
enutHV = left_join(enut_hogar, enut_vivienda,
                       by = c('upm', 'viv_sel'))




# Porcentajes a nivel nacional
r_query = enutHV %>%
  # Seleccionamos las variables de hogares que contratan, cantidad de trabajadoras que contratan, horas que contratan a la semana, el factor hogar, la etidad y UPM
  select (p2_6_3, p2_7_3, p2_8_3 ,fac_hog, ent, upm) %>% 
  # Agrupamos por la variable de hogares que contratan 
  group_by(p2_6_3)%>% 
  # Filtramos por las respuestas de si contratan (1) y no contratan (2)
  filter(p2_6_3 != 'NA') %>% 
  # Sacamos el total de hogares, el total de trabajadoras y el total de horas trabajadas a la semana
  summarise(
    total = sum(fac_hog),
    trabajadoras =sum(p2_7_3*fac_hog),
    horas= sum(p2_8_3*fac_hog)
  ) %>% 
  # Calculamos el porcentaje de hogares, el promedio de trabajadoras por hogar y el promedio de horas a la semana contratadas 
  mutate(
    porcentaje= (total/sum(total))*100,
    prom_tra=trabajadoras/total,
    prom_hor=horas/total
  )


servicio_cuidados=collect(r_query)
# Visualizamos la tabla
servicio_cuidados

# Dividimos por regiones
# Variable de regiones 
enut_hogar_re = enutHV %>% 
  # Clasificamos las regiones 
  mutate(region = case_when(
    ent %in% c('02', '05', '08', '19', '26', '28') ~ 'norte',
    ent %in% c('03', '10', '18', '25', '32') ~ 'norte-occidente',
    ent %in% c('01', '06', '14', '16', '24') ~ 'centro-norte',
    ent %in% c('09', '11', '13', '15', '17', '21','22', '29') ~ 'centro',
    ent %in% c('04', '07', '12', '20', '23', '27', '30', '31') ~ 'sur'
  )
  )
# Calculamos la cantidad de hogares por region 
r_query = enut_hogar_re %>%
  # Seleccionamos las variables de contratar servicios, el factor hogar y la region 
  select (fac_hog,region) %>% 
  # Agrupamos por región
  group_by(region)%>% 
  # Agregamos la columna de hogares por región 
  summarise(
    hogares_por_region = sum(fac_hog),
  ) 

hogar=collect(r_query)

# Vizualizamos la tabla hogar 
hogar


# Porcentaje por regiones de contratar servicios de cuidado
# Calculamos la cantidad de hogares que contratan enfermeras(os) o cuidadoras(es) de niñas, niños o de personas mayores o con alguna discapacidad o enfermedad
r_query = enut_hogar_re %>% 
  #Seleccionamos las variables de contratar servicios, el factor hogar y la region 
  select (p2_6_3,fac_hog,region) %>% 
  #filtramos para los hogares que si contratan un servicio de cuidados 
  filter(p2_6_3 == 1) %>% 
  #Agrupamos por región
  group_by(region) %>%
  # Calculamos la cantidad de hogares
  summarise(
    hogares_que_contratan = sum(fac_hog)
  ) 
servicio_cuidados_regiones=collect(r_query)
#Vizualizamos la tabla unida de los hograes totales por region y los que contratan 
servicio_cuidados_regiones

# Calculamos el porcentaje de hogares que contratan servicios de cuidado por hogar 
porcentaje_regiones_servicios_cuidados = left_join(hogar, servicio_cuidados_regiones, by='region') %>%
  mutate(porcentaje = hogares_que_contratan/hogares_por_region * 100)

#Vizualizamos la tabla 
porcentaje_regiones_servicios_cuidados


# Construccón de Base de Datos --------

# Base de datos que incluya los parametros que se calcularon. 

# 1) Cargamos cada uno de los parametros que vamos a unir 
# 2) Clasificamos los parametros en hogar y personas 

# Esto para crear una base para las variables de hogar y las variables de personas 

informal_reg # personas
personas_trabajan_reg # personas
porcentaje_regiones_servicios_cuidados # hogar
no_empleo_cuidados_total_reg# personas
hijos_region # hogar 
adultos_regional # personas
probabilidad_discapacidad_reg # hogar
sexos_reg_enoe # personas
sexos_reg_enut# personas
adultos_regional #personas

# Agregamos llave a las tablas 
informal_reg$llave <-paste0(informal_reg$region,"_", informal_reg$sex
)
informal_reg

colnames(informal_reg) <- c("sex", "region", "total_reg_informal", "total_informal", 
                            "porcentaje_informal", "llave")
informal_reg

# Variable personas que trabajan 
personas_trabajan_reg$llave <-paste0(personas_trabajan_reg$region,"_", 
                                     personas_trabajan_reg$sex
)
personas_trabajan_reg

colnames(personas_trabajan_reg) <- c("sex", "region", "total_reg_trabajo", "total_trabajos", 
                                     "porcentaje_trabajo", "llave")

personas_trabajan_reg

# Dataframe de personas que trabajan y personas en informalidad
inf_trab <- merge(informal_reg, personas_trabajan_reg[,c("llave", "total_reg_trabajo", "total_trabajos",
                                                         "porcentaje_trabajo")], 
                  by = "llave")


# Agregamos llave a las tablas 
no_empleo_cuidados_total_reg$llave <-paste0(no_empleo_cuidados_total_reg$region,
                                            "_", no_empleo_cuidados_total_reg$sex
)
no_empleo_cuidados_total_reg

colnames(no_empleo_cuidados_total_reg) <- c("sex", "region", "total_reg_no_empleo", 
                                            "total_no_empleo_cuidados", 
                                            "porcentaje_no_empleo", "llave")
no_empleo_cuidados_total_reg
View(no_empleo_cuidados_total_reg)


inf_trab_no_e <-merge(inf_trab, no_empleo_cuidados_total_reg [, c("llave", "total_reg_no_empleo",
                                                                  "total_no_empleo_cuidados","porcentaje_no_empleo")],
                      by="llave")
inf_trab_no_e
# Visualisar la nueva base
View(inf_trab_no_e)

# Variable de adultos mayores
adultos_regional$llave <-paste0(adultos_regional$region,
                                  "_", adultos_regional$sex
)
adultos_regional

colnames(adultos_regional) <- c("sex", "region", "total_adultos_mayores_reg", 
                                  "total_adultos_mayores", 
                                  "porcentaje_adultos_mayores", "llave")
adultos_regional

trab_cuidados <- merge(inf_trab_no_e, adultos_regional [, c("llave", "total_adultos_mayores_reg",
                                                              "total_adultos_mayores", "porcentaje_adultos_mayores")],
                       by = "llave")

trab_cuidados

# Unimos los dataframe de enoe y enut para personas en edad de trabajar 
sexos_reg_encuestas <- cbind.data.frame(sexos_reg_enut, sexos_reg_enoe [, c("total_reg", "porcentaje")])

colnames(sexos_reg_encuestas) <- c("sexo", "region", "total_reg_enut",
                                   "porcentaje_enut", "total_reg_enoe",
                                   "porcentaje_enoe")
sexos_reg_encuestas

# LLAVE de las tablas para unirlas 
sexos_reg_encuestas$llave <-paste0(sexos_reg_encuestas$region,"_", sexos_reg_encuestas$sex
)
sexos_reg_encuestas

# Hacemos el merge de la tabla de variable 

# Parámetros que faltan cálculo nacional  inf_trab_no_e con sexos_reg_encuestas

df_personas <-merge(trab_cuidados, sexos_reg_encuestas[, c("llave","total_reg_enut","porcentaje_enut",
                                                           "total_reg_enoe", "porcentaje_enoe")], by = "llave" )
view(df_personas)

# AhoraCreamos el dataframe para las variables de hogar

colnames(porcentaje_regiones_servicios_cuidados) <- c("region", "hogares_por_region",
                                                      "hogares_que_contratan", "porcentaje_hogares_contratan") 

colnames(hijos_region) <- c("region", "total_hijos", "porcentaje_hijos")

colnames(probabilidad_discapacidad_reg) <- c("region", "total_discapacidad_regional", 
                                         "hogares_con_discapacidad", "porcentaje_discapacidad")


df_hijos_servicios_cuidados <- merge(porcentaje_regiones_servicios_cuidados, hijos_region, 
                                     by = "region")
df_hijos_servicios_cuidados

df_hogares <- merge(df_hijos_servicios_cuidados, probabilidad_discapacidad_reg, 
                    by = "region")
df_hogares

View(df_hogares)

# Descargar los data frame 
write.csv(df_personas, "/Users/mariananazar/Desktop/Ciencia de Datos/df_personas.csv", row.names = FALSE)
write.csv(df_hogares, "/Users/mariananazar/Desktop/Ciencia de Datos/df_hogares.csv", row.names = FALSE)


#Bases de datos nacionales
# Seleccionamos el nombre de las variables que nos interesan 
# Clasificamos entre variables de personas y hogar

Porcentaje_informal_nacional # Personas
porcenaje_adultos_nacional # Personas
Hogares_discapacidad_nacional # Hogar
sexos_enoe # Personas
sexos_ENUT # Personas
no_empleo_cuidados_nacional # Personas
hijos # Hogar
trabajo_nacional # Personas
servicio_cuidados  # Hogares


# Variable de informalidad nacional 
# Renombramos las columnas 

colnames(Porcentaje_informal_nacional) <- c("sexo", "total_personas",
                                            "total_nacional_inf", 
                                            "porcentaje_informal") 
Porcentaje_informal_nacional

# Variable de adultos mayores
# Renombramos las columnas 

colnames(porcenaje_adultos_nacional) <- c("sexo", "total_nacional_adultos", "total_adultos_mayores",
                                          "porcentaje_adultos_mayores")
porcenaje_adultos_nacional


# Unimos las variables de informalidad y adultos mayores
adultos_informalidad<- merge(Porcentaje_informal_nacional, porcenaje_adultos_nacional, 
                                     by = "sexo")

# Data frame de informalidad y adultos mayores
adultos_informalidad

# Repetimos este proceso para las variables de personas en edad de trabajar
edad_trabajar <- cbind.data.frame(sexos_enoe, sexos_ENUT [, c("total_nacional", "porcentaje")])
edad_trabajar

colnames(edad_trabajar) <- c("sexo", "edad_de_trabajat_enoe", "porcentaje_edad_de_trabajar_enoe",
                                   "total_nacional_edad_trabajar_enut", "porcentaje_edad_trabajar_enut")
edad_trabajar

adultos_edad<- merge(adultos_informalidad, edad_trabajar, 
                             by = "sexo")
adultos_edad


# Ahora para la variable de personas que no trabajn por cuidados y personas que trabajan
colnames(no_empleo_cuidados_nacional) <- c("sexo", "total_nacional_personas_no_trabajan",
                                            "total_no_empleo_cuidados", 
                                            "porcentaje_no_empleo_cuidados") 
no_empleo_cuidados_nacional

colnames(trabajo_nacional) <- c("sexo", "total_nacional_personas", "total_personas_trabajan",
                                          "porcentaje_trabajan")
trabajo_nacional


#Creamos el primer data frame de datos de personas 
no_trabajo_pea<- merge(no_empleo_cuidados_nacional, trabajo_nacional, 
                             by = "sexo")
no_trabajo_pea

# Unimos ambas tablas en un solo data frame

df_personas_nacional <-merge(adultos_edad, no_trabajo_pea, 
                             by = "sexo")
df_personas_nacional

# Descargar data frame 
write.csv(df_personas_nacional, "/Users/mariananazar/Desktop/Ciencia de Datos/df_personas_nacional.csv", row.names = FALSE)

# Nos desconectamos
dbDisconnect(reto_db)




