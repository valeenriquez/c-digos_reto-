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
# progress                                                       
if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress)                                                            
}
# patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}
# geomtextpath                                                       
if(require(geomtextpath) == FALSE){                                                
  install.packages('geomtextpath')                                                 
  library(geomtextpath)                                                            
}else{                                                                          
  library(geomtextpath)    
}

# openxlsx                                                      
if(require(openxlsx) == FALSE){                                                
  install.packages('openxlsx')                                                 
  library(openxlsx)                                                            
}else{                                                                          
  library(openxlsx)    
}

# Modelo de servicios de cuidado 2019 -----------------------------------------------
#Partiendo de los modelos base e incorporando los efectos de la informalidad, necesidad de servicios de 
#cuidados y política de estancias, introducimos las condiciones de México en 2019 para modelar 
#el impacto de tener estancias para adultos mayores, niños y personas discapacitadas en la 
#distribución del ingreso y la movilidad social. Este acceso va a impactar en la probabilidad de trabajar
#y, a su vez, en la probabilidad de ser formal dado que trabaja.

#Número de agentes (PEA en México en 2019 dividida entre 10,000)
num_agentes = 5590

#Importamos el csv de condiciones inciales
agentes = read_csv('Condiciones iniciales.csv') %>%
  mutate(
    #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
    discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
    #Seleccionamos a los agentes que tendrán que acceso a una estancia para discapacitados 
    #(de acuerdo con su probabilidad)
    acceso_a_estancias_disc = rbinom(n= num_agentes, 1, prob = p_estancias_disc),
    #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
    ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
    #Seleccionamos a los agentes tendrán que acceso a una estancia para adultos mayores 
    #(de acuerdo con su probabilidad)
    acceso_a_estancias_am = rbinom(n= num_agentes, 1, prob = p_estancias_am),
    #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
    hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
    #Seleccionamos a los agentes que tendrán acceso a guarderías (de acuerdo con su probabilidad)
    acceso_a_guarderias = rbinom(n= num_agentes, 1, prob = p_guarderias),
    #Contamos el total de personas que cuidarán
    personas_cuidadas = discapacitados + hijos + ancianos,
    #Si tiene acceso a alguna estancia (de acuerdo con su necesidad), le ponemos un 1 
    acceso_a_estancias = ifelse((acceso_a_estancias_disc == 1 & discapacitados >= 1) |
                                  (acceso_a_estancias_am == 1 & ancianos >= 1) |
                                  (acceso_a_guarderias == 1 & hijos >= 1), 1, 0),
    #Eliminamos las columnas que no usaremos
    acceso_a_estancias_disc = NULL, 
    acceso_a_estancias_am = NULL, 
    acceso_a_guarderias = NULL,
    #La probabilidad de trabajar de las personas que tienen que cuidar a alguien y no tienen acceso 
    #a estancias disminuye en su probabilidad de dejar de trabajar por cuidados, dado 
    #que no pueden contratar a una persona cuidadora
    p_trabajo = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                       pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 - 
                                                                       p_cuidados))), 
                            0), p_trabajo),
    #Probabilidad de ser formal dado que trabajan
    p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
  )

# Número de periodos
num_periodos = 1000
#Transferencias 
transferencia = 1
# producción
produccion = num_agentes * transferencia
# Consumo autónomo
consumo_autonomo = 0.32
# Propensión marginal a consumir (INEGI, 2019)
pmc = 0.68

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Seleccionamos a los agentes que trabajan dadas sus probabilidad de hacerlo
                     prob = pull(agentes, p_trabajo)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes 
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE,
                #su probabilidad de recibir un ingreso depende de su probabilidad de ser formales,
                #dado que trabajan
                prob = pull(agentes%>% 
                              filter(trabaja == 1), (p_formalidad))
                )
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: Subsidio a los pobres
      subsidio = ifelse(riqueza_T < consumo_autonomo, consumo_autonomo, 0),
      # Ronda 2: Distribución del resto de la producción
      ingreso = coalesce(ingreso, 0),
      # Ingreso total
      ingreso_total = subsidio + ingreso,
      # Consumo keynesiano
      consumo = ifelse(
        # Si su riqueza actual es menor a sus capacidades de consumo
        riqueza_T + ingreso_total < consumo_autonomo + pmc * ingreso,
        # Agotar la riqueza
        riqueza_T + ingreso_total,
        # De otro modo, consumir
        consumo_autonomo + pmc * ingreso
      ),
      # Actualizamos la riqueza
      riqueza_T = riqueza_T + ingreso_total - consumo,
      # Guardamos la riqueza de este periodo
      "riqueza_{t}" := riqueza_T,
      # Borramos las columnas que no usaremos
      subsidio = NULL,
      ingreso = NULL,
      consumo = NULL,
      ingreso_total = NULL
    )
  
  # Barra de progreso
  progreso$tick()
}

# Obtenemos la riqueza final
riqueza_final = agentes %>% 
  select(id, riqueza_0, riqueza_T, sexo, region) %>% 
  #Estandarizamos los valores 
  mutate(
    riqueza_0 = (riqueza_0 - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)),
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)

plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T, col = sexo)) +
   scale_color_manual(values = c('#37A6A6', '#CD608F')) +
  # Añadimos un histograma de densidad y coloreamos por sexo
  geom_histogram(aes(x = after_stat(density), fill = sexo), col = 'gray30') +
  scale_fill_manual(values = c('#37A6A6', '#CD608F')) +
  # Añadimos un gráfico de densidad
  # geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  #geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Modificamos etiquetas
  labs(x = 'Probabilidad', 
       y = NULL)  +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

plot_individuos = riqueza_final %>% 
  # Creamos un ggplot de los individuos ordenados por riqueza 
  ggplot(aes(x = id, y = sort(riqueza_T))) +
  # Añadimos puntos
  geom_point() +
  # Modificamos los límites del eje y
  scale_y_continuous(limits = c(-0.1,1.1)) + 
  # Modificamos etiquetas
  labs(x = 'Individuos',
       y = NULL) +
  # Añadimos una línea horizontal en la riqueza inicial estandarizada
  geom_hline(yintercept = riqueza_inicial, 
             col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Añadimos etiquetas
  geom_text(label = 'Riqueza inicial', x = 500, y =  .65, col = '#D90368') + 
  # Añadimos una flecha
  annotate(
    "segment", x = 500, xend = 500, y = .6, yend = .1,  colour = '#D90368', linewidth = 0.5, 
    arrow = arrow(length = unit(.3,"cm"))
  ) +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  theme_classic()

# Unimos los dos gráficos
plot_modelo_cuidados = plot_probabilidad + plot_individuos + 
  plot_annotation(
    title = "Modelo con servicios de cuidado en México (2019)",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022) y datos de la ENOE y ENUT (2019)'
  ) 
print(plot_modelo_cuidados)

#Vemos la distribución región
plot_probabilidad + facet_wrap(~ region, nrow = 1) + 
  theme(legend.position = c(0.05,0.9),
        legend.background = element_rect(fill = 'transparent'))


# Curva de Lorenz
riqueza_final = riqueza_final %>% 
  # Ordenamos por riqueza
  arrange(riqueza_T) %>% 
  mutate(
    # Normalizamos la riqueza
    riqueza_i = riqueza_T,
    # Calculamos la proporción de la riqueza
    proporcion_riqueza = riqueza_i/sum(riqueza_i),
    # Calculamos la proporción acumulada de la riqueza (Curva de Lorenz)
    distribucion_riqueza = cumsum(riqueza_i)/ sum(riqueza_i),
    # Calculamos la proporción de perfecta igualdad
    proporcion_perfecta_igualdad = 1/num_agentes,
    # Calculamos la línea de perfecta igualdad
    perfecta_igualdad = cumsum(proporcion_perfecta_igualdad),
    # Sacamos la diferencia entre el escenario ideal y la distribución actual de la riqueza
    difference = abs(perfecta_igualdad - distribucion_riqueza))

# Guardamos la distribución de la riqueza de este modelo
modelo_cuidados = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_cuidados = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_cuidados)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T y las probabilidades 
  select(-c(riqueza_T, p_no_trabajo_x_cuidados,
            p_trabajo, p_estancias_am, p_estancias_disc,
            p_guarderias, p_informalidad, p_anciano,
            p_discapacitado, p_cuidados)) %>% 
  # Cambiamos a formato long, manteniendo el id, sexo y región intactos
  gather(time, riqueza, -c(id, sexo, region)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')),
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza para los periodos 300 y 1000
time_plot_modelo_cuidados = riqueza_series %>% 
  # Filtramos periodos
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = riqueza)) +
  # Añadimos histograma 
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1, 1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Añadimos etiquetas
  labs(
    x = 'Probabilidad',
    y = NULL,
    title = "Modelo con servicios de cuidado en México (2019)",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022) y datos de la ENOE y ENUT (2019)'
  )  +
  # División por periodo
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

print(time_plot_modelo_cuidados)

# Matriz de movilidad social
movilidad_modelo_cuidados = riqueza_series %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_cuidados)

#Guardamos el resultado como archivo xlsx
write.xlsx(movilidad_modelo_cuidados, "Matriz de movilidad cuidados 2019.xlsx")

#POR SEXO

# Matriz de movilidad por sexo
movilidad_modelo_cuidados_sexo = riqueza_series %>% 
  group_by(sexo) %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_cuidados_sexo)

#Guardamos el resultado como archivo xlsx
write.xlsx(movilidad_modelo_cuidados_sexo, "Matriz de movilidad cuidados 2019 (sexo).xlsx")

#POR REGIÓN 
# Matriz de movilidad por región
movilidad_modelo_cuidados_region = riqueza_series %>% 
  group_by(region) %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_cuidados_region)

#Guardamos el resultado como archivo xlsx
write.xlsx(movilidad_modelo_cuidados_region, "Matriz de movilidad cuidados 2019 (region).xlsx")

#POR REGIÓN Y SEXO
# Matriz de movilidad por región y sexo
movilidad_modelo_cuidados_sexo_region = riqueza_series %>% 
  group_by(region, sexo) %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_cuidados_sexo_region)

#Guardamos el resultado como archivo xlsx
write.xlsx(movilidad_modelo_cuidados_sexo_region, "Matriz de movilidad cuidados 2019 (sexo y región.xlsx")

#AÑADIMOS UN RANGO DE VALORES PARA GRAFICAR EL IMPACTO DEL CAMBIO EN ACCESO

# Si la probabilidad de acceso es 50% mayor...

#Importamos el csv de condiciones inciales
agentes = read_csv('Condiciones iniciales.csv') %>%
  mutate(
    #Las probabilidades de acceso aumentan 50%
    p_estancias_am = p_estancias_am * 1.5,
    p_estancias_disc = p_estancias_disc * 1.5,
    p_guarderias = p_guarderias * 1.5,
    #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
    discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
    #Seleccionamos a los agentes que tendrán acceso a una estancia para discapacitados 
    #(de acuerdo con su probabilidad)
    acceso_a_estancias_disc = rbinom(n= num_agentes, 1, prob = p_estancias_disc),
    #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
    ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
    #Seleccionamos a los agentes que tendrán acceso a una estancia para adultos mayores 
    #(de acuerdo con su probabilidad)
    acceso_a_estancias_am = rbinom(n= num_agentes, 1, prob = p_estancias_am),
    #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
    hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
    #Seleccionamos a los agentes que tendrán acceso a guarderías (de acuerdo con su probabilidad)
    acceso_a_guarderias = rbinom(n= num_agentes, 1, prob = p_guarderias),
    #Contamos el total de personas que cuidarán
    personas_cuidadas = discapacitados + hijos + ancianos,
    #Si tiene acceso a alguna estancia (de acuerdo con su necesidad), le ponemos un 1 
    acceso_a_estancias = ifelse((acceso_a_estancias_disc == 1 & discapacitados >= 1) |
                                  (acceso_a_estancias_am == 1 & ancianos >= 1) |
                                  (acceso_a_guarderias == 1 & hijos >= 1), 1, 0),
    #Eliminamos las columnas que no nusaremos
    acceso_a_estancias_disc = NULL, 
    acceso_a_estancias_am = NULL, 
    acceso_a_guarderias = NULL,
    #La probabilidad de trabajar de las personas que tienen que cuidar a alguien y no tienen acceso 
    #a estancias disminuye en su probabilidad de dejar de trabajar por cuidados, dado 
    #que no pueden contratar a una persona cuidadora
    p_trabajo = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                       pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 - 
                                                                       p_cuidados))), 
                            0), p_trabajo),
    #Probabilidad de ser formal dado que trabajan
    p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
  )

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Seleccionamos a los agentes que trabajan dadas sus probabilidad de hacerlo
                     prob = pull(agentes, p_trabajo)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes 
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE,
                #su probabilidad de recibir un ingreso depende de su probabilidad de ser formales, dado que trabajan
                prob = pull(agentes%>% 
                              filter(trabaja == 1), (p_formalidad))
    )
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: Subsidio a los pobres
      subsidio = ifelse(riqueza_T < consumo_autonomo, consumo_autonomo, 0),
      # Ronda 2: Distribución del resto de la producción
      ingreso = coalesce(ingreso, 0),
      # Ingreso total
      ingreso_total = subsidio + ingreso,
      # Consumo keynesiano
      consumo = ifelse(
        # Si su riqueza actual es menor a sus capacidades de consumo
        riqueza_T + ingreso_total < consumo_autonomo + pmc * ingreso,
        # Agotar la riqueza
        riqueza_T + ingreso_total,
        # De otro modo, consumir
        consumo_autonomo + pmc * ingreso
      ),
      # Actualizamos la riqueza
      riqueza_T = riqueza_T + ingreso_total - consumo,
      # Guardamos la riqueza de este periodo
      "riqueza_{t}" := riqueza_T,
      # Borramos las columnas que no usaremos
      subsidio = NULL,
      ingreso = NULL,
      consumo = NULL,
      ingreso_total = NULL
    )
  
  # Barra de progreso
  progreso$tick()
}

# Obtenemos la riqueza final
riqueza_final = agentes %>% 
  select(id, riqueza_0, riqueza_T) %>% 
  #Estandarizamos los valores 
  mutate(
    riqueza_0 = (riqueza_0 - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)),
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T))) %>% 
  # Curva de Lorenz
  # Ordenamos por riqueza
  arrange(riqueza_T) %>% 
  mutate(
    # Normalizamos la riqueza
    riqueza_i = riqueza_T,
    # Calculamos la proporción de la riqueza
    proporcion_riqueza = riqueza_i/sum(riqueza_i),
    # Calculamos la proporción acumulada de la riqueza (Curva de Lorenz)
    distribucion_riqueza = cumsum(riqueza_i)/ sum(riqueza_i),
    # Calculamos la proporción de perfecta igualdad
    proporcion_perfecta_igualdad = 1/num_agentes,
    # Calculamos la línea de perfecta igualdad
    perfecta_igualdad = cumsum(proporcion_perfecta_igualdad),
    # Sacamos la diferencia entre el escenario ideal y la distribución actual de la riqueza
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_mas_acceso= pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_mas_acceso= riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_mas_acceso)

# Si la probabilidad de acceso es 50% menor...

#Importamos el csv de condiciones inciales
agentes = read_csv('Condiciones iniciales.csv') %>%
  mutate(
    #Las probabilidades de acceso disminuyen 50%
    p_estancias_am = p_estancias_am * .5,
    p_estancias_disc = p_estancias_disc * .5,
    p_guarderias = p_guarderias * .5,
    #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
    discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
    #Seleccionamos a los agentes que tendrán acceso a una estancia para discapacitados 
    #(de acuerdo con su probabilidad)
    acceso_a_estancias_disc = rbinom(n= num_agentes, 1, prob = p_estancias_disc),
    #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
    ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
    #Seleccionamos a los agentes que tendrán acceso a una estancia para adultos mayores 
    #(de acuerdo con su probabilidad)
    acceso_a_estancias_am = rbinom(n= num_agentes, 1, prob = p_estancias_am),
    #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
    hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
    #Seleccionamos a los agentes que tendrán acceso a guarderías (de acuerdo con su probabilidad)
    acceso_a_guarderias = rbinom(n= num_agentes, 1, prob = p_guarderias),
    #Contamos el total de personas que cuidarán
    personas_cuidadas = discapacitados + hijos + ancianos,
    #Si tiene acceso a alguna estancia (de acuerdo con su necesidad), le ponemos un 1 
    acceso_a_estancias = ifelse((acceso_a_estancias_disc == 1 & discapacitados >= 1) |
                                  (acceso_a_estancias_am == 1 & ancianos >= 1) |
                                  (acceso_a_guarderias == 1 & hijos >= 1), 1, 0),
    #Eliminamos las columnas que no nusaremos
    acceso_a_estancias_disc = NULL, 
    acceso_a_estancias_am = NULL, 
    acceso_a_guarderias = NULL,
    #La probabilidad de trabajar de las personas que tienen que cuidar a alguien y no tienen acceso 
    #a estancias disminuye con base en su probabilidad de dejar de trabajar por cuidados, dado 
    #que no pueden contratar a una persona cuidadora
    p_trabajo = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                       pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 -
                                                                       p_cuidados))), 
                            0), p_trabajo),
    #Probabilidad de ser formal dado que trabajan
    p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
  )

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Seleccionamos a los agentes que trabajan dadas sus probabilidad de hacerlo
                     prob = pull(agentes, p_trabajo)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes 
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE,
                #su probabilidad de recibir un ingreso depende de su probabilidad de ser formales, dado que trabajan
                prob = pull(agentes%>% 
                              filter(trabaja == 1), (p_formalidad))
    )
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: Subsidio a los pobres
      subsidio = ifelse(riqueza_T < consumo_autonomo, consumo_autonomo, 0),
      # Ronda 2: Distribución del resto de la producción
      ingreso = coalesce(ingreso, 0),
      # Ingreso total
      ingreso_total = subsidio + ingreso,
      # Consumo keynesiano
      consumo = ifelse(
        # Si su riqueza actual es menor a sus capacidades de consumo
        riqueza_T + ingreso_total < consumo_autonomo + pmc * ingreso,
        # Agotar la riqueza
        riqueza_T + ingreso_total,
        # De otro modo, consumir
        consumo_autonomo + pmc * ingreso
      ),
      # Actualizamos la riqueza
      riqueza_T = riqueza_T + ingreso_total - consumo,
      # Guardamos la riqueza de este periodo
      "riqueza_{t}" := riqueza_T,
      # Borramos las columnas que no usaremos
      subsidio = NULL,
      ingreso = NULL,
      consumo = NULL,
      ingreso_total = NULL
    )
  
  # Barra de progreso
  progreso$tick()
}

# Obtenemos la riqueza final
riqueza_final = agentes %>% 
  select(id, riqueza_0, riqueza_T) %>% 
  #Estandarizamos los valores 
  mutate(
    riqueza_0 = (riqueza_0 - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)),
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T))) %>% 
  # Curva de Lorenz
  # Ordenamos por riqueza
  arrange(riqueza_T) %>% 
  mutate(
    # Normalizamos la riqueza
    riqueza_i = riqueza_T,
    # Calculamos la proporción de la riqueza
    proporcion_riqueza = riqueza_i/sum(riqueza_i),
    # Calculamos la proporción acumulada de la riqueza (Curva de Lorenz)
    distribucion_riqueza = cumsum(riqueza_i)/ sum(riqueza_i),
    # Calculamos la proporción de perfecta igualdad
    proporcion_perfecta_igualdad = 1/num_agentes,
    # Calculamos la línea de perfecta igualdad
    perfecta_igualdad = cumsum(proporcion_perfecta_igualdad),
    # Sacamos la diferencia entre el escenario ideal y la distribución actual de la riqueza
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_menos_acceso= pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_menos_acceso= riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_menos_acceso)

#Graficamos la Curva de Lorenz
curva_lorenz_cuidados = ggplot(riqueza_final, aes(x = perfecta_igualdad)) + 
  #Coloreamos el desplazamiento de la curva ante variaciones en el acceso a estancias
  geom_ribbon(aes(ymin = modelo_mas_acceso,
                  ymax = modelo_menos_acceso),
              fill = '#f0defc') +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo cuidados", aes(y = modelo_cuidados), 
    vjust = 1.5, linewidth = 1, color = '#6D4573', upright= TRUE
  ) +
  # Añadimos línea con texto
  geom_textline(
    label = "Perfecta igualdad", aes(y = perfecta_igualdad), 
    vjust = -0.75, linewidth = 1
  ) +
  # Añadimos porcentajes
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Añadimos etiquetas
  labs(
    x = 'Población acumulada',
    y = 'Riqueza acumulada',
    title = "Modelo con servicios de cuidado en México (2019)",
    subtitle = paste('Índice de Gini:', round(gini_cuidados, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022) y datos de la ENOE y ENUT (2019)'
  ) +
  theme_classic()

print(curva_lorenz_cuidados)

# Modelo de subsidios y sin estancias con condiciones de 2019-----------------------------------------------
#Modificamos el modelo anterior, eliminando las estancias y añadiendo un subsidio al cuidado. Los agentes
#que deben cuidar a alguien recibirán un subsidio del gobierno por cada persona que deben cuidar. 
#El 20% de la producción (monto destinado a transferencias en México en 2019 
#(PEF, 2019)) se distribuirá equitativamente entre el número de personas que requieren cuidados.

#Importamos el csv de condiciones inciales
agentes = read_csv('Condiciones iniciales.csv') %>%
  mutate(
    #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
    discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
    #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
    ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
    #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
    hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
    #Contamos el total de personas que cuidarán
    personas_cuidadas = discapacitados + hijos + ancianos,
    #La probabilidad de trabajar de las personas que tienen que cuidar a alguien  
    #disminuye con base en su probabilidad de dejar de trabajar por cuidados, dado 
    #que no pueden contratar a una persona cuidadora
    p_trabajo = ifelse(personas_cuidadas >= 1, 
                       pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 - 
                                                                       p_cuidados))), 
                            0), p_trabajo),
    #Probabilidad de ser formal dado que trabajan
    p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
  )

#Calculamos el total de personas que requieren cuidados
total_personas_cuidadas = pull(agentes,personas_cuidadas) %>%  sum()
# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Seleccionamos a los agentes que trabajan dadas sus probabilidad de hacerlo
                     prob = pull(agentes, p_trabajo)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo 
  #El 20% del PIB mexicano en 2019 se destinó a transferencias (PEF, 2019)
  transferencias =  0.2 * produccion / total_personas_cuidadas
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes 
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE,
                #Su probabilidad de recibir un ingreso depende de su probabilidad de ser formales, 
                #dado que trabajan
                prob = pull(agentes%>% 
                              filter(trabaja == 1), p_formalidad)
    )
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion * (1-0.2) - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: 
      #Subsidio a los pobres
      subsidio = ifelse(riqueza_T < consumo_autonomo, consumo_autonomo, 0),
      # Subsidio por cada persona que tienen que cuidar
      transferencia = ifelse(personas_cuidadas >= 1, personas_cuidadas * transferencias, 0), 
      # Ronda 2: Distribución del resto de la producción
      ingreso = coalesce(ingreso, 0),
      # Ingreso total
      ingreso_total = subsidio + ingreso + transferencia,
      # Consumo keynesiano
      consumo = ifelse(
        # Si su riqueza actual es menor a sus capacidades de consumo
        riqueza_T + ingreso_total < consumo_autonomo + pmc * ingreso,
        # Agotar la riqueza
        riqueza_T + ingreso_total,
        # De otro modo, consumir
        consumo_autonomo + pmc * ingreso
      ),
      # Actualizamos la riqueza
      riqueza_T = riqueza_T + ingreso_total - consumo,
      # Guardamos la riqueza de este periodo
      "riqueza_{t}" := riqueza_T,
      # Borramos las columnas que no usaremos
      subsidio = NULL,
      ingreso = NULL,
      consumo = NULL,
      ingreso_total = NULL,
      transferencia = NULL
    )
  
  # Barra de progreso
  progreso$tick()
}

# Obtenemos la riqueza final
riqueza_final = agentes %>% 
  select(id, riqueza_0, riqueza_T, sexo, region, personas_cuidadas) %>% 
  #Estandarizamos los valores 
  mutate(
    riqueza_0 = (riqueza_0 - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)),
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)

plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T, col = sexo)) +
  scale_color_manual(values = c('#37A6A6', '#CD608F')) +
  # Añadimos un histograma de densidad y coloreamos por sexo
  geom_histogram(aes(x = after_stat(density), fill = sexo), col = 'gray30') +
  scale_fill_manual(values = c('#37A6A6', '#CD608F')) +
  # Añadimos un gráfico de densidad
  # geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  #geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Modificamos etiquetas
  labs(x = 'Probabilidad', 
       y = NULL)  +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

plot_individuos = riqueza_final %>% 
  # Creamos un ggplot de los individuos ordenados por riqueza 
  ggplot(aes(x = id, y = sort(riqueza_T))) +
  # Añadimos puntos
  geom_point() +
  # Modificamos los límites del eje y
  scale_y_continuous(limits = c(-0.1,1.1)) + 
  # Modificamos etiquetas
  labs(x = 'Individuos',
       y = NULL) +
  # Añadimos una línea horizontal en la riqueza inicial estandarizada
  geom_hline(yintercept = riqueza_inicial, 
             col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Añadimos etiquetas
  geom_text(label = 'Riqueza inicial', x = 500, y =  .65, col = '#D90368') + 
  # Añadimos una flecha
  annotate(
    "segment", x = 500, xend = 500, y = .6, yend = .1,  colour = '#D90368', linewidth = 0.5, 
    arrow = arrow(length = unit(.3,"cm"))
  ) +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  theme_classic()

# Unimos los dos gráficos
plot_modelo_subsidios = plot_probabilidad + plot_individuos + 
  plot_annotation(
    title = "Modelo con subsidios en el contexto mexicano (2019)",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022) y datos de la ENOE y ENUT (2019)'
  ) 
print(plot_modelo_subsidios)

#Distribución de la riqueza por región
plot_probabilidad + facet_wrap(~ region, nrow = 1) +
  theme(legend.position = c(0.05,0.9),
        legend.background = element_rect(fill = 'transparent'))

# Curva de Lorenz
riqueza_final = riqueza_final %>% 
  # Ordenamos por riqueza
  arrange(riqueza_T) %>% 
  mutate(
    # Normalizamos la riqueza
    riqueza_i = riqueza_T,
    # Calculamos la proporción de la riqueza
    proporcion_riqueza = riqueza_i/sum(riqueza_i),
    # Calculamos la proporción acumulada de la riqueza (Curva de Lorenz)
    distribucion_riqueza = cumsum(riqueza_i)/ sum(riqueza_i),
    # Calculamos la proporción de perfecta igualdad
    proporcion_perfecta_igualdad = 1/num_agentes,
    # Calculamos la línea de perfecta igualdad
    perfecta_igualdad = cumsum(proporcion_perfecta_igualdad),
    # Sacamos la diferencia entre el escenario ideal y la distribución actual de la riqueza
    difference = abs(perfecta_igualdad - distribucion_riqueza))

# Guardamos la distribución de la riqueza de este modelo
modelo_subsidios = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_subsidios = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_subsidios)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T y las probabilidades
  select(-c(riqueza_T, p_no_trabajo_x_cuidados,
            p_trabajo, p_estancias_am, p_estancias_disc,
            p_guarderias, p_informalidad, p_anciano,
            p_discapacitado, p_cuidados)) %>% 
  # Cambiamos a formato long, manteniendo el id, sexo y región intactos
  gather(time, riqueza, -c(id, sexo, region)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')),
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza para los periodos 300 y 1000
time_plot_modelo_subsidios = riqueza_series %>% 
  # Filtramos periodos
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = riqueza)) +
  # Añadimos histograma 
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1, 1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Añadimos etiquetas
  labs(
    x = 'Probabilidad',
    y = NULL,
    title = "Modelo con subsidios"
    #caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Faceta por periodo
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

print(time_plot_modelo_subsidios)

# Matriz de movilidad social
movilidad_modelo_subsidios = riqueza_series %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_subsidios)

#Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_subsidios, "Matriz de movilidad subsidios 2019.xlsx")

#POR SEXO
# Matriz de movilidad por sexo
movilidad_modelo_subsidios_sexo = riqueza_series %>% 
  group_by(sexo) %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_subsidios_sexo)

#Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_subsidios_sexo, "Matriz de movilidad subsidios 2019 (sexo).xlsx")

#POR REGIÓN 
# Matriz de movilidad por región
movilidad_modelo_subsidios_region = riqueza_series %>% 
  group_by(region) %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_subsidios_region)

#Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_subsidios_region, "Matriz de movilidad subsidios 2019 (region).xlsx")

#POR REGIÓN Y SEXO
# Matriz de movilidad por región y sexo
movilidad_modelo_subsidios_sexo_region = riqueza_series %>% 
  group_by(region, sexo) %>% 
  #Mantenemos el período 300 (edad = 30) y 1000
  filter(time %in% c(300, 1000)) %>% 
  spread(time, riqueza, sep = '_') %>% 
  mutate(
    # Calculamos los quintiles iniciales de riqueza
    quintil_inicial = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculamos los quintiles finales de riqueza
    quintil_final = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Contamos los casos
  count(quintil_inicial, quintil_final) %>% 
  # Calculamos porcentajes
  with_groups(
    .groups = quintil_inicial,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Creamos la matriz
  pivot_wider(names_from = quintil_final, values_from = n) %>% 
  # llenamos los NA's
  mutate_if(is.numeric, coalesce, 0) 

print(movilidad_modelo_subsidios_sexo_region)

#Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_subsidios_sexo_region, "Matriz de movilidad subsidios 2019 (region y sexo).xlsx")

#AÑADIMOS UN RANGO DE VALORES PARA GRAFICAR EL IMPACTO DEL CAMBIO EN SUBSIDIOS

# Si crecen en 50% (30% de la producción)...

#Importamos el csv de condiciones inciales
agentes = read_csv('Condiciones iniciales.csv') %>%
  mutate(
    #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
    discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
    #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
    ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
    #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
    hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
    #Contamos el total de personas que cuidarán
    personas_cuidadas = discapacitados + hijos + ancianos,
    #La probabilidad de trabajar de las personas que tienen que cuidar a alguien  
    #disminuye con base en su probabilidad de dejar de trabajar por cuidados, dado 
    #que no pueden contratar a una persona cuidadora
    p_trabajo = ifelse(personas_cuidadas >= 1, 
                       pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 - 
                                                                       p_cuidados))), 
                            0), p_trabajo),
    #Probabilidad de ser formal dado que trabajan
    p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
  )

#Calculamos el total de personas que requieren cuidados
total_personas_cuidadas = pull(agentes,personas_cuidadas) %>%  sum()
# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Seleccionamos a los agentes que trabajan dadas sus probabilidad de hacerlo
                     prob = pull(agentes, p_trabajo)))
  
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo 
  #El 30% del PIB mexicano se destinará a transferencias
  transferencias =  0.3 * produccion / total_personas_cuidadas
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes 
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE,
                #su probabilidad de recibir un ingreso depende de su probabilidad de ser formales, 
                #dado que trabajan
                prob = pull(agentes%>% 
                              filter(trabaja == 1), p_formalidad)
    )
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion * (1-0.3) - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: 
      #Subsidio a los pobres
      subsidio = ifelse(riqueza_T < consumo_autonomo, consumo_autonomo, 0),
      # Subsidio por cada persona que tienen que cuidar
      transferencia = ifelse(personas_cuidadas >= 1, personas_cuidadas * transferencias, 0), 
      # Ronda 2: Distribución del resto de la producción
      ingreso = coalesce(ingreso, 0),
      # Ingreso total
      ingreso_total = subsidio + ingreso + transferencia,
      # Consumo keynesiano
      consumo = ifelse(
        # Si su riqueza actual es menor a sus capacidades de consumo
        riqueza_T + ingreso_total < consumo_autonomo + pmc * ingreso,
        # Agotar la riqueza
        riqueza_T + ingreso_total,
        # De otro modo, consumir
        consumo_autonomo + pmc * ingreso
      ),
      # Actualizamos la riqueza
      riqueza_T = riqueza_T + ingreso_total - consumo,
      # Guardamos la riqueza de este periodo
      "riqueza_{t}" := riqueza_T,
      # Borramos las columnas que no usaremos
      subsidio = NULL,
      ingreso = NULL,
      consumo = NULL,
      ingreso_total = NULL, 
      transferencia = NULL
    )
  
  # Barra de progreso
  progreso$tick()
}

# Obtenemos la riqueza final
riqueza_final = agentes %>% 
  select(id, riqueza_0, riqueza_T) %>% 
  #Estandarizamos los valores 
  mutate(
    riqueza_0 = (riqueza_0 - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)),
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T))) %>% 
  # Curva de Lorenz
  # Ordenamos por riqueza
  arrange(riqueza_T) %>% 
  mutate(
    # Normalizamos la riqueza
    riqueza_i = riqueza_T,
    # Calculamos la proporción de la riqueza
    proporcion_riqueza = riqueza_i/sum(riqueza_i),
    # Calculamos la proporción acumulada de la riqueza (Curva de Lorenz)
    distribucion_riqueza = cumsum(riqueza_i)/ sum(riqueza_i),
    # Calculamos la proporción de perfecta igualdad
    proporcion_perfecta_igualdad = 1/num_agentes,
    # Calculamos la línea de perfecta igualdad
    perfecta_igualdad = cumsum(proporcion_perfecta_igualdad),
    # Sacamos la diferencia entre el escenario ideal y la distribución actual de la riqueza
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_mas_subsidios= pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_mas_subsidios= riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_mas_subsidios)

#Si caen en 50% (10% de la producción)...

#Importamos el csv de condiciones inciales
agentes = read_csv('Condiciones iniciales.csv') %>%
  mutate(
    #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
    discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
    #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
    ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
    #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
    hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
    #Contamos el total de personas que cuidarán
    personas_cuidadas = discapacitados + hijos + ancianos,
    #La probabilidad de trabajar de las personas que tienen que cuidar a alguien  
    #disminuye con base en su probabilidad de dejar de trabajar por cuidados, dado 
    #que no pueden contratar a una persona cuidadora
    p_trabajo = ifelse(personas_cuidadas >= 1, 
                       pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 - p_cuidados))), 
                            0), p_trabajo),
    #Probabilidad de ser formal dado que trabajan
    p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
  )

#Calculamos el total de personas que requieren cuidados
total_personas_cuidadas = pull(agentes,personas_cuidadas) %>%  sum()
# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Seleccionamos a los agentes que trabajan dadas sus probabilidad de hacerlo
                     prob = pull(agentes, p_trabajo)))
  
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo 
  #El 10% del PIB mexicano se destinará a pensiones
  transferencias =  0.1 * produccion / total_personas_cuidadas
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes 
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE,
                #su probabilidad de recibir un ingreso depende de su probabilidad de ser formales, dado que trabajan
                prob = pull(agentes%>% 
                              filter(trabaja == 1), p_formalidad)
    )
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion * (1-0.1) - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: 
      #Subsidio a los pobres
      subsidio = ifelse(riqueza_T < consumo_autonomo, consumo_autonomo, 0),
      # Subsidio por cada persona que tienen que cuidar
      transferencia = ifelse(personas_cuidadas >= 1, personas_cuidadas * transferencias, 0), 
      # Ronda 2: Distribución del resto de la producción
      ingreso = coalesce(ingreso, 0),
      # Ingreso total
      ingreso_total = subsidio + ingreso + transferencia,
      # Consumo keynesiano
      consumo = ifelse(
        # Si su riqueza actual es menor a sus capacidades de consumo
        riqueza_T + ingreso_total < consumo_autonomo + pmc * ingreso,
        # Agotar la riqueza
        riqueza_T + ingreso_total,
        # De otro modo, consumir
        consumo_autonomo + pmc * ingreso
      ),
      # Actualizamos la riqueza
      riqueza_T = riqueza_T + ingreso_total - consumo,
      # Guardamos la riqueza de este periodo
      "riqueza_{t}" := riqueza_T,
      # Borramos las columnas que no usaremos
      subsidio = NULL,
      ingreso = NULL,
      consumo = NULL,
      ingreso_total = NULL,
      transferencia = NULL
    )
  
  # Barra de progreso
  progreso$tick()
}

# Obtenemos la riqueza final
riqueza_final = agentes %>% 
  select(id, riqueza_0, riqueza_T) %>% 
  #Estandarizamos los valores 
  mutate(
    riqueza_0 = (riqueza_0 - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)),
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T))) %>% 
  # Curva de Lorenz
  # Ordenamos por riqueza
  arrange(riqueza_T) %>% 
  mutate(
    # Normalizamos la riqueza
    riqueza_i = riqueza_T,
    # Calculamos la proporción de la riqueza
    proporcion_riqueza = riqueza_i/sum(riqueza_i),
    # Calculamos la proporción acumulada de la riqueza (Curva de Lorenz)
    distribucion_riqueza = cumsum(riqueza_i)/ sum(riqueza_i),
    # Calculamos la proporción de perfecta igualdad
    proporcion_perfecta_igualdad = 1/num_agentes,
    # Calculamos la línea de perfecta igualdad
    perfecta_igualdad = cumsum(proporcion_perfecta_igualdad),
    # Sacamos la diferencia entre el escenario ideal y la distribución actual de la riqueza
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_menos_subsidios= pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_menos_subsidios= riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_menos_subsidios)

#Graficamos la Curva de Lorenz
curva_lorenz_subsidios = ggplot(riqueza_final, aes(x = perfecta_igualdad)) + 
  #Coloreamos el desplazamiento de la curva ante variaciones en los subsidios
  geom_ribbon(aes(ymin = modelo_mas_subsidios,
                  ymax = modelo_menos_subsidios),
              fill = '#f0defc') +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo subsidios", aes(y = modelo_subsidios), 
    vjust = 1.5, linewidth = 1, color = '#6D4573'
  ) +
  # Añadimos línea con texto
  geom_textline(
    label = "Perfecta igualdad", aes(y = perfecta_igualdad), 
    vjust = -0.75, linewidth = 1
  ) +
  # Añadimos porcentajes
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Añadimos etiquetas
  labs(
    x = 'Población acumulada',
    y = 'Riqueza acumulada',
    title = "Modelo con subsidios al cuidado en México (2019)",
    subtitle = paste('Índice de Gini:', round(gini_subsidios, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022) y datos de la ENOE y ENUT (2019)'
  ) +
  theme_classic()

print(curva_lorenz_subsidios)


