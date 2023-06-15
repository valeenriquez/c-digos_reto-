#Fijamos el directorio de trabajo
setwd("~/Documents/Ciencia de datos/Reto")
# Librerías ---------------------------------------------------------------
# tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Condiciones iniciales (código para csv) ---------------------------------------------------
#Todos los cálculos fueron realizados con base en las condiciones mexicanas de 2019. Las bases de datos
#utilizadas para calcular los porcentajes fueron la Encuesta Nacional de Uso de Tiempo (ENUT), el 
#Directorio Estadístico Nacional de Unidades Económicas (DENUE) y la Encuestra Nacional de Ocupación
#y Empleo (ENOE) 2019. 

#Usamos la población económicamente activa (PEA) (55.9 millones) dividido entre 10,000 (INEGI, 2020)
num_agentes = 5590

#Creamos a los agentes 
agentes = tibble(
  id = 1:num_agentes
)


# SEXO
#El 53% de la PEA es mujer
#Seleccionamos el ID de los agentes que tendrán ese sexo
set.seed(123)
num_mujeres = sample(agentes$id, 
                     size = num_agentes * .53, 
                     replace = F)

mujeres = agentes[num_mujeres, ] %>%  
  mutate(
    sexo = 'mujer'
  )

#El otro 47% es hombre
hombres = agentes[-num_mujeres, ] %>% 
  mutate(
    sexo = 'hombre'
  )

#Asignamos el sexo a cada agente
agentes = full_join(mujeres, hombres) %>% 
  left_join(agentes, by = 'id') %>% 
  arrange(id)


# REGIONES 
#Cada agente pertenece a una región
#Regiones MUJERES
#El 39% de las mujeres pertenece a la región centro
#Seleccionamos el ID de las mujeres que pertenecerán a esa región
set.seed(123)
num_mujeres_centro = sample(mujeres$id, 
                            size = num_agentes * .53 * .39, 
                            replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
mujeres_centro = agentes[num_mujeres_centro, ] %>%  
  mutate(
    region = 'centro', 
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = 0.0343,
    #probabilidad de trabajar
    p_trabajo = 0.444,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0861,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0529,
    #probabilidad de acceso a guarderías
    p_guarderias = .3617,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.268,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0317,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1413,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0617,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0012
  )

#El 14.2% pertenece a la región centro-norte
#Eliminamos el ID de las mujeres en la región centro
mujeres_nuevo = agentes[-num_mujeres_centro, ] %>% 
  filter(sexo == 'mujer')
#Seleccionamos el ID de las mujeres que pertenecerán al centro-norte
set.seed(123)
num_mujeres_centro_norte = sample(mujeres_nuevo$id, 
                                  size = num_agentes * .53 * .142, 
                                  replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
mujeres_centro_norte = agentes[num_mujeres_centro_norte, ] %>%  
  mutate(
    region = 'centro-norte',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = 0.0248,
    #probabilidad de trabajar
    p_trabajo = 0.444,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0740,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0271,
    #probabilidad de acceso a guarderías
    p_guarderias = .3396,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.252,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0379,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1642,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0572,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0045
  )

#El 17.9% pertenece a la región norte
#Eliminamos el ID de las mujeres en la región centro y centro-norte
mujeres_nuevo = agentes[
  -c(num_mujeres_centro_norte, num_mujeres_centro), ]%>% 
  filter(sexo == 'mujer')
#Seleccionamos el ID de las mujeres que pertenecerán al norte
set.seed(123)
num_mujeres_norte = sample(mujeres_nuevo$id, 
                           size = num_agentes * .53 * .179, 
                           replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
mujeres_norte = agentes[num_mujeres_norte, ] %>%  
  mutate(
    region = 'norte',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = 0.0281,
    #probabilidad de trabajar
    p_trabajo = 0.46,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0656,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0304,
    #probabilidad de acceso a guarderías
    p_guarderias = .3207,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.19,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0348,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1695,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0562,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0023
  )

#El 6.5% pertenece a la región norte-occiddente
#Eliminamos el ID de las mujeres en la región centro, centro-norte y norte
mujeres_nuevo =agentes[
  -c(num_mujeres_centro_norte, num_mujeres_centro, num_mujeres_norte ), ]%>% 
  filter(sexo == 'mujer')
#Seleccionamos el ID de las mujeres que pertenecerán al norte-occidente
set.seed(123)
num_mujeres_norte_occidente = sample(mujeres_nuevo$id, 
                                     size = num_agentes * .53 * .065, 
                                     replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
mujeres_norte_occidente = agentes[num_mujeres_norte_occidente, ] %>%  
  mutate(
    region = 'norte-occidente',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .0336,
    #probabilidad de trabajar
    p_trabajo = 0.456,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0687,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0442,
    #probabilidad de acceso a guarderías
    p_guarderias = .4342,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.248,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0391,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1602,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0740,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0014
  )

#El 22.4% restante pertenece al sur 
#Eliminamos el ID de las mujeres en las demás regiones
#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
mujeres_sur = agentes[
  -c(num_mujeres_norte_occidente, num_mujeres_centro_norte,
     num_mujeres_centro, num_mujeres_norte ), ]%>% 
  filter(sexo == 'mujer') %>% 
  mutate(
    region = 'sur',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .0374,
    #probabilidad de trabajar
    p_trabajo = 0.405,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0446,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0266,
    #probabilidad de acceso a guarderías
    p_guarderias = .2927,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.291,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0305,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1087,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0758,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0022
  )

#Juntamos todos los tibbles de mujeres
mujeres = full_join(mujeres_sur, mujeres_norte) %>% 
  full_join(mujeres_norte_occidente) %>% 
  full_join(mujeres_centro) %>% 
  full_join(mujeres_centro_norte)


# Regiones HOMBRES 
#El 38.2% de los hombres pertenece a la región centro
#Seleccionamos el ID de los hombres que pertenecerán a esa región
set.seed(123)
num_hombres_centro = sample(hombres$id, 
                            size = num_agentes * .47 * .382, 
                            replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
hombres_centro = agentes[num_hombres_centro, ] %>%  
  mutate(
    region = 'centro',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .00055,
    #probabilidad de trabajar
    p_trabajo = 0.731,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0861,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0529,
    #probabilidad de acceso a guarderías
    p_guarderias = .3617,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.426,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0215,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1413,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0617,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0012
  )

#El 14.4% pertenece a la región centro-norte
#Eliminamos el ID de los hombres en la región centro
hombres_nuevo = agentes[-num_hombres_centro, ] %>% 
  filter(sexo == 'hombre')

#Seleccionamos el ID de los hombres que pertenecerán al centro-norte
set.seed(123)
num_hombres_centro_norte = sample(hombres_nuevo$id, 
                                  size = num_agentes * .47 * .144, 
                                  replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
hombres_centro_norte = agentes[num_hombres_centro_norte, ] %>%  
  mutate(
    region = 'centro-norte',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .00053,
    #probabilidad de trabajar
    p_trabajo = 0.757,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0740,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0271,
    #probabilidad de acceso a guarderías
    p_guarderias = .3396,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.416,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0296,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1642,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0572,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0045
  )

#El 18.8% pertenece a la región norte
#Eliminamos el ID de los hombres en la región centro y centro-norte
hombres_nuevo =agentes[
  -c(num_hombres_centro_norte, num_hombres_centro), ]%>% 
  filter(sexo == 'hombre')
#Seleccionamos el ID de los hombres que pertenecerán al norte
set.seed(123)
num_hombres_norte = sample(hombres_nuevo$id, 
                           size = num_agentes * .47 * .188, 
                           replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
hombres_norte = agentes[num_hombres_norte, ] %>%  
  mutate(
    region = 'norte',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .00094,
    #probabilidad de trabajar
    p_trabajo = 0.746,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0656,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0304,
    #probabilidad de acceso a guarderías
    p_guarderias = .3207,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.28,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.023,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1695,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0562,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0023
  )

#El 6.8% pertenece a la región norte-occiddente
#Eliminamos el ID de los hombres en la región centro, centro-norte y norte
hombres_nuevo =agentes[
  -c(num_hombres_centro_norte, num_hombres_centro, num_hombres_norte ), ]%>% 
  filter(sexo == 'hombre')
#Seleccionamos el ID de los hombres que pertenecerán al norte-occidente
set.seed(123)
num_hombres_norte_occidente = sample(hombres_nuevo$id,
                                     size = num_agentes * .47 * .068,
                                     replace = F)

#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
hombres_norte_occidente = agentes[num_hombres_norte_occidente, ] %>%  
  mutate(
    region = 'norte-occidente',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .00058,
    #probabilidad de trabajar
    p_trabajo = 0.751,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0687,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0442,
    #probabilidad de acceso a guarderías
    p_guarderias = .4342,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.404,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0241,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1602,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0740,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0014
  )

#El 21.8% restante pertenece al sur 
#Eliminamos el ID de los hombres en las demás regiones
#Creamos un tibble con sus probabilidades dependiendo de su sexo y región 
hombres_sur = agentes[
  -c(num_hombres_norte_occidente, num_hombres_centro_norte, 
     num_hombres_centro, num_hombres_norte ), ]%>% 
  filter(sexo == 'hombre') %>% 
  mutate(
    region = 'sur',
    #probabilidad de no trabajar (a pesar de quererlo/necesitarlo) porque cuida a alguien
    p_no_trabajo_x_cuidados = .00047,
    #probabilidad de trabajar
    p_trabajo = 0.762,
    #probabilidad de acceso a estancia para adultos mayores
    p_estancias_am = .0446,
    #probabilidad de acceso a estancia para discapacitados
    p_estancias_disc = .0266,
    #probabilidad de acceso a guarderías
    p_guarderias = .2927,
    #probabilidad de pertenecer al sector informal
    p_informalidad = 0.533,
    #probabilidad de cuidar a un adulto mayor
    p_anciano = 0.0287,
    #probabilidad de tener un hijo/a entre 0 y 4 años 
    p_hijos = 0.1087,
    #probabilidad de tener una persona con alguna discapacidad en el hogar
    p_discapacitado = 0.0758,
    #probabilidad de que en su hogar contraten servicios de cuidados
    p_cuidados = 0.0022
  )

#Juntamos todos los tibbles de hombres
hombres = full_join(hombres_sur, hombres_norte) %>% 
  full_join(hombres_norte_occidente) %>% 
  full_join(hombres_centro) %>% 
  full_join(hombres_centro_norte)


#Creamos el tibble con todos los agentes y sus características iniciales
agentes = full_join(mujeres, hombres) %>% 
  arrange(id) %>% 
  mutate ( #añadimos la riqueza
    # Riqueza inicial (todos comienzan con la misma riqueza)
    riqueza_0 = 3,
    # Riqueza en el periodo T
    riqueza_T = 3)

#Guardamos nuestras condiciones iniciales como csv
write_csv(agentes, 'Condiciones iniciales.csv')
