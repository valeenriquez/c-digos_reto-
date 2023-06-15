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

# Modelo base (mercado de trabajo) ---------------------------------------------------
#Este modelo y sus variantes son una adaptación del modelo de Xiaoliang Yang y Peng Zhou (2022)
# https://www.econstor.eu/bitstream/10419/261231/1/E2022-03.pdf para analizar el impacto
#de la informalidad y las políticas de cuidados en la distribución de la riqueza y la movilidad social.
#Partimos de un modelo con 1000 agentes que poseen la misma riqueza inicial y, aleatoriamente, 
# trabajan (o no) en cada uno de los 1000 periodos modelados. Si trabajan, reciben un ingreso. 
#Asimismo, consumen: cuentan con una función de consumo Keynesiano, donde la propensión marginal 
#a consumir (PMC) es 0.68 (PMC para México en 2019 (INEGI, 2019)) y su consumo autónomo es 0.32. Si su 
#riqueza del periodo es menor a este consumo autónomo, el gobierno les transfiere esa cantidad. 

#Número de agentes
num_agentes = 1000
#Creamos a los agentes 
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3
)
# Número de periodos
num_periodos = 1000
#Transferencias 
transferencia = 1
# producción
produccion = num_agentes * transferencia
# Consumo autónomo
consumo_autonomo = 0.32
# Propensión marginal a consumir 
pmc = 0.68

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, prob = runif(num_agentes, 0, 1)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                            filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)


plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T)) + 
  # Añadimos un histograma de densidad
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Añadimos un gráfico de densidad
  geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Modificamos etiquetas
  labs(x = 'Probabilidad',
       y = NULL) +
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
plot_modelo_base = plot_probabilidad + plot_individuos + 
  plot_annotation(
    title = "Modelo base: mercado de trabajo",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  ) 

print(plot_modelo_base)

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
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución base de la riqueza  
modelo_base = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_base = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_base)

# Curva de Lorenz
curva_lorenz_base = ggplot(riqueza_final, aes(x = perfecta_igualdad)) + 
  # Añadimos líneas con texto
  geom_textline(
    label = "Curva de Lorenz", aes(y = distribucion_riqueza), 
    vjust = 1.5, linewidth = 1, color = '#D90368'
  ) +
  geom_textline(
    label = "Perfecta igualdad", aes(y = perfecta_igualdad), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Añadimos porcentajes
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Añadimos etiquetas
  labs(
    x = 'Población acumulada',
    y = 'Riqueza acumulada',
    title = "Modelo base: mercado de trabajo",
    subtitle = paste('Índice de Gini:', round(gini_base, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  ) + 
  #Cambiamos el tema
  theme_classic()

print(curva_lorenz_base)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T
  select(-riqueza_T) %>% 
  # Cambiamos a formato long, manteniendo el id intacto
  gather(time, riqueza, -c(id)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')), 
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza en los periodos 300 y 1000
time_plot_modelo_base = riqueza_series %>% 
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
    title = "Modelo base",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )  +
  # Dividimos el gráfico por periodo
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

print(time_plot_modelo_base)

# Matriz de movilidad social
movilidad_modelo_base = riqueza_series %>% 
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

print(movilidad_modelo_base)

#Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_base, "Matriz de movilidad social base.xlsx")


# Modelo con informalidad ---------------------------------------------------
#Ahora los agentes tendrán una probabilidad (asignada aleatoriamente) de ser formales, misma 
#que determinará su probabilidad de recibir un ingreso

#Creamos a los agentes y les añadimos una probabilidad de ser formales
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Añadimos una probabilidad entre 0 y 1 de que sean formales
  p_formalidad = runif(num_agentes, 0, 1)
)

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, prob = runif(num_agentes, 0, 1)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE, 
                #Ahora el ingreso dependerá de su probabilidad de ser formales
                prob = pull(agentes%>% 
                              filter(trabaja == 1), p_formalidad))
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
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)

plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T)) + 
  # Añadimos un histograma de densidad
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Añadimos un gráfico de densidad
  geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # AModificamos etiquetas
  labs(x = 'Probabilidad',
       y = NULL) +
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
plot_modelo_informalidad = plot_probabilidad + plot_individuos + 
  plot_annotation(
    title = "Modelo con informalidad",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )

print(plot_modelo_informalidad)

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
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_informalidad = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_informalidad = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_informalidad)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T
  select(-riqueza_T) %>% 
  # Cambiamos a formato long, manteniendo el id intacto
  gather(time, riqueza, -c(id)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')), 
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza en los periodos 300 y 1000
time_plot_modelo_informalidad = riqueza_series %>% 
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
    title = "Modelo con informalidad",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )  +
  # Dividimos el gráfico por periodo
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

print(time_plot_modelo_informalidad)

# Matriz de movilidad social
movilidad_modelo_informalidad = riqueza_series %>% 
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

print(movilidad_modelo_informalidad)

# Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_informalidad, "Matriz de movilidad social informalidad.xlsx")

#AÑADIMOS UN RANGO DE VALORES PARA GRAFICAR EL IMPACTO DEL CAMBIO EN LA INFORMALIDAD

# Si la probabilidad de ser formal fuera mayor...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Incrementamos la probabilidad de ser formal: ahora tomará valores entre 0.5 y 1
  p_formalidad = runif(num_agentes, 0.5, 1)
)

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, prob = runif(num_agentes, 0, 1)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE, 
                #Ahora el ingreso dependerá de su probabilidad de ser formales
                prob = pull(agentes%>% 
                              filter(trabaja == 1), p_formalidad))
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
modelo_menos_informalidad = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_menos_informalidad = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_menos_informalidad)

# Si la probabilidad de ser formal fuera menor...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Disminuimos la probabilidad de ser formal: ahora tomará valores entre 0 y 0.5
  p_formalidad = runif(num_agentes, 0, 0.5) 
)

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, prob = runif(num_agentes, 0, 1)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE, 
                #Ahora el ingreso dependerá de su probabilidad de ser formales
                prob = pull(agentes%>% 
                              filter(trabaja == 1), p_formalidad))
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
modelo_mas_informalidad = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_mas_informalidad = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_mas_informalidad)

#Graficamos la Curva de Lorenz
curva_lorenz_informalidad = ggplot(riqueza_final, aes(x = perfecta_igualdad)) + 
  #Coloreamos el desplazamiento de la curva ante variaciones en la informalidad
  geom_ribbon(aes(ymin = modelo_menos_informalidad,
                  ymax = modelo_mas_informalidad),
              fill = '#f0defc') +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo base", aes(y = modelo_base), 
    vjust = 1.5, linewidth = 1, color = '#D90368', linetype = 'dashed'
  ) +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo con informalidad", aes(y = modelo_informalidad), 
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
    title = "Modelo con informalidad",
    subtitle = paste('Índice de Gini:', round(gini_informalidad, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  ) + 
  theme_classic()

print(curva_lorenz_informalidad)

# Modelo con necesidad de cuidados ---------------------------------------------------
#Introducimos al modelo la probabilidad de tener que cuidar a alguien y, con base en ella, se 
#determina el número de personas (entre 0 y 5) que tendrá que cuidar cada agente. Si tienen que 
#cuidar a alguien, tendrán una probabilidad aleatoria de dejar de trabajar por dedicarse a cuidados y 
#su probabilidad de trabajar disminuirá en esa proporción.

#Creamos a los agentes y les añadimos nuevas probabilidades
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad entre 0 y 1 de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas 
  #a las cuales cuidar
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1, 
                                     runif(num_agentes, 0, 1), 0))

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar se reducirá en su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)

plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T)) + 
  # Añadimos un histograma de densidad
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Añadimos un gráfico de densidad
  geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Modificamos etiquetas
  labs(x = 'Probabilidad',
       y = NULL) +
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
       y= NULL) +
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
    title = "Modelo con necesidades de cuidados",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )

print(plot_modelo_cuidados)

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
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución base de la riqueza  
modelo_cuidados = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_cuidados = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_cuidados)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T
  select(-riqueza_T) %>% 
  # Cambiamos a formato long, manteniendo el id intacto
  gather(time, riqueza, -c(id)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')),
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza en los periodos 300 y 1000
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
    title = "Modelo con necesidad de cuidados",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )  +
  # Dividimos el gráfico por periodo
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

# Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_cuidados, "Matriz de movilidad social cuidados.xlsx")

#AÑADIMOS UN RANGO DE VALORES PARA GRAFICAR EL IMPACTO DEL CAMBIO EN LA NECESIDAD DE CUIDADOS

# Si la necesidad fuera mayor...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Incrementamos la probabilidad de que tengan que cuidar a una persona (ahora estará entre 0.5 y 1)
  p_cuidar = runif(num_agentes, 0.5, 1),
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas a las cuales cuidar
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1, 
                                   runif(num_agentes, 0, 1), 0))

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar se reducirá en su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
modelo_mas_necesidad = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_mas_necesidad = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_mas_necesidad)

# Si la necesidad fuera menor...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Reducimos la probabilidad de que tengan que cuidar a una persona (ahora estará entre 0 y 0.5)
  p_cuidar = runif(num_agentes, 0, 0.5),
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas a las cuales cuidar
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1, 
                                   runif(num_agentes, 0, 1), 0))

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar se reducirá en su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
modelo_menos_necesidad = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_menos_necesidad = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_menos_necesidad)

#Graficamos la Curva de Lorenz
curva_lorenz_cuidados = ggplot(riqueza_final, aes(x = perfecta_igualdad)) + 
  #Coloreamos el desplazamiento de la curva ante variaciones en la necesidad
  geom_ribbon(aes(ymin = modelo_menos_necesidad,
                  ymax = modelo_mas_necesidad),
              fill = '#f0defc') +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo base", aes(y = modelo_base), 
    vjust = 1.5, linewidth = 1, color = '#D90368', linetype = 'dashed'
  ) +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo con necesidad de cuidados", aes(y = modelo_cuidados), 
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
    title = "Modelo con necesidad de cuidados",
    subtitle = paste('Índice de Gini:', round(gini_cuidados, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  ) +
  theme_classic()

print(curva_lorenz_cuidados)

# Modelo con servicios de cuidados ---------------------------------------------------
#¿Qué pasa si en la variante anterior introducimos estancias para las personas que necesitan cuidados?
#Ahora los agentes tendrán una probabilidad homogénea (0.5) de acceder a estas estancias y, 
#con base en ella, se determinará si tienen acceso o no. Los agentes con acceso ya pueden trabajar; sin 
#embargo, los que deben cuidar a alguien y no tienen acceso a estancias se verán limitados por su 
#probabilidad de dejar de trabajar por dedicarse a cuidados.

#Creamos a los agentes y asignamos las nuevas probabilidades
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Asignamos una probabilidad homogénea de que tengan acceso a estancias
  p_estancias = 0.5,
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Seleccionamos a los agentes que tendrán acceso a una estancia (de acuerdo con su probabilidad)
  acceso_a_estancias = rbinom(n= num_agentes, 1, prob = p_estancias),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas 
  #a las cuales cuidar y no tienen acceso a estancias
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                                   runif(num_agentes, 0, 1), 0)
)

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar se reducirá en su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)

plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T)) + 
  # Añadimos un histograma de densidad
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Añadimos un gráfico de densidad
  geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Modificamos etiquetas
  labs(x = 'Probabilidad', 
       y = NULL) +
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
plot_modelo_servicios_de_cuidados = plot_probabilidad + plot_individuos + 
  plot_annotation(
    title = "Modelo con servicios de cuidados",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )

print(plot_modelo_servicios_de_cuidados)

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
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_servicio_cuidados = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_servicios_de_cuidados = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_servicios_de_cuidados)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T
  select(-riqueza_T) %>% 
  # Cambiamos a formato long, manteniendo el id intacto
  gather(time, riqueza, -c(id)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')),
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza en los periodos 300 y 1000
time_plot_modelo_servicios_de_cuidados = riqueza_series %>% 
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
    title = "Modelo servicios de cuidados",
    caption = 'Con servicios de cuidados en el modelo de Yang & Zhou (2022)'
  )  +
  # Dividimos el gráfico por periodo
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modificamos el tema
  theme(
    axis.title.y = element_blank()
  ) +
  theme_classic()

print(time_plot_modelo_servicios_de_cuidados)

# Matriz de movilidad social
movilidad_modelo_servicios_de_cuidados = riqueza_series %>% 
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

print(movilidad_modelo_servicios_de_cuidados)

#Guardamos el resultado en un archivo xlsx
write.xlsx(movilidad_modelo_servicios_de_cuidados, "Matriz de movilidad social servicios de cuidados.xlsx")

#AÑADIMOS UN RANGO DE VALORES PARA GRAFICAR EL IMPACTO DEL CAMBIO EN EL ACCESO A ESTANCIAS

# Si la probabilidad fuera 50% mayor (0.75)...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Asignamos una probabilidad homogénea de que tengan acceso a estancias
  p_estancias = 0.75,
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Seleccionamos a los agentes que tendrán que acceso a una estancia (de acuerdo con su probabilidad)
  acceso_a_estancias = rbinom(n= num_agentes, 1, prob = p_estancias),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas
  #a las cuales cuidary no tienen acceso a estancias
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                                   runif(num_agentes, 0, 1), 0)
)

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar se reducirá en su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
modelo_mas_acceso = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_mas_acceso= riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_mas_acceso)

# Si la probabilidad fuera 50% menor (0.25)...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Asignamos una probabilidad homogénea de que tengan acceso a estancias
  p_estancias = 0.25,
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Seleccionamos a los agentes que tendrán acceso a una estancia (de acuerdo con su probabilidad)
  acceso_a_estancias = rbinom(n= num_agentes, 1, prob = p_estancias),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas 
  #a las cuales cuidar y no tienen acceso a estancias
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                                   runif(num_agentes, 0, 1), 0)
)

# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar se reducirá en su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
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
modelo_menos_acceso = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_menos_acceso= riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_menos_acceso)

#Graficamos la Curva de Lorenz
curva_lorenz_servicios_de_cuidados = ggplot(riqueza_final, aes(x = perfecta_igualdad)) + 
  #Coloreamos el desplazamiento de la curva ante variaciones en el acceso a estancias
  geom_ribbon(aes(ymin = modelo_mas_acceso,
                  ymax = modelo_menos_acceso),
              fill = '#f0defc') +
  # Añadimos línea con texto
  geom_textline(
    label = ".", aes(y = modelo_base), 
    vjust = 1.5, linewidth = 1, color = '#D90368', linetype = 'dashed', upright= TRUE, alpha = .5
  ) +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo con servicios de cuidados", aes(y = modelo_servicio_cuidados), 
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
    title = "Modelo con servicios de cuidados",
    subtitle = paste('Índice de Gini:', round(gini_servicios_de_cuidados, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  ) + 
  theme_classic()

print(curva_lorenz_servicios_de_cuidados)


# Modelo con subsidios a quienes requieren cuidados ---------------------------------------------------
#¿Qué pasa si en lugar de introducir estancias se les da una transferencia a las
#personas que necesitan cuidados? Los agentes que deben cuidar a alguien seguirán viéndose limitados 
#por su probabilidad de dejar de trabajar por cuidados, pero recibirán un subsidio del gobierno por cada
#persona que deben cuidar. El 20% de la producción (monto destinado a transferencias en México en 2019 
#(PEF, 2019)) se distribuirá equitativamente entre el número de personas que requieren cuidados.

#Creamos a los agentes 
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas a 
  #las cuales cuidar
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1, 
                                   runif(num_agentes, 0, 1), 0))

#Calculamos el total de personas que requieren cuidados
total_personas_cuidadas = pull(agentes, personas_cuidadas) %>%  sum()
# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar ahora dependerá también de su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
    
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo 
  # Tranferencias para quienes necesitan cuidados (20% de la producción)
  transferencias = 0.2 * produccion / total_personas_cuidadas 
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion * (1 - 0.2) - subsidios) / num_agentes)
  
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
    riqueza_T = (riqueza_T - min(riqueza_T)) / (max(riqueza_T) - min(riqueza_T)))
#Obtenemos la riqueza inicial estandarizada
riqueza_inicial = pull(riqueza_final, riqueza_0)

plot_probabilidad = riqueza_final %>% 
  # Creamos un ggplot
  ggplot(aes(y = riqueza_T)) + 
  # Añadimos un histograma de densidad
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Añadimos un gráfico de densidad
  geom_density() +
  # Añadimos una línea horizontal en la riqueza inicial
  geom_hline(yintercept = riqueza_inicial, col = '#D90368', linetype = 'dashed', linewidth = 1) +
  # Modificamos los límites y la posición del eje y
  scale_y_continuous(position = "right", limits = c(-0.1,1.1)) + 
  # Invertimos el eje x
  scale_x_reverse() +
  # Modificamos etiquetas
  labs(x = 'Probabilidad',
       y = NULL) +
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
    title = "Modelo con subsidios",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )

print(plot_modelo_subsidios)

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
    difference = abs(perfecta_igualdad - distribucion_riqueza)
  ) 

# Guardamos la distribución de la riqueza  
modelo_subsidios = pull(riqueza_final, distribucion_riqueza)

# Calculamos el índice de Gini
gini_subsidios = riqueza_final %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(gini_subsidios)

# Transformamos la serie de tiempo a formato long
riqueza_series = agentes %>% 
  # Seleccionamos todo, menos la riqueza_T
  select(-riqueza_T) %>% 
  # Cambiamos a formato long, manteniendo el id intacto
  gather(time, riqueza, -c(id)) %>% 
  # Quitamos el texto de la columna de tiempo
  mutate(time = as.numeric(str_remove(time, 'riqueza_')),
         #Estandarizamos la riqueza
         riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))

# Graficamos la distribución de la riqueza en los periodos 300 y 1000
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
    title = "Modelo con subsidios",
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  )  +
  # dividimos el gráfico por periodo
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
write.xlsx(movilidad_modelo_subsidios, "Matriz de movilidad social subsidios.xlsx")

#AÑADIMOS UN RANGO DE VALORES PARA GRAFICAR EL IMPACTO DEL CAMBIO EN LOS SUBSIDIOS

# Si crecen en 50% (30% de la producción)...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas a las cuales cuidar
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1, 
                                   runif(num_agentes, 0, 1), 0))

#Calculamos el total de personas que requieren cuidados
total_personas_cuidadas = pull(agentes, personas_cuidadas) %>%  sum()
# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar ahora dependerá también de su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo 
  # Tranferencias para quienes necesitan cuidados (30% de la producción)
  transferencias = 0.3 * produccion / total_personas_cuidadas 
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion * (1 - 0.3) - subsidios) / num_agentes)
  
  agentes = agentes %>% 
    # Juntamos las ganancias 
    left_join(ganancias, by = 'id') %>% 
    mutate(
      # Ronda 1: 
      # Subsidio a los pobres
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

# Si caen en 50% (10% de la producción)...

#Creamos a los agentes
agentes = tibble(
  id = 1:num_agentes,
  # Riqueza inicial (todos comienzan con la misma riqueza)
  riqueza_0 = 3,
  # Riqueza en el periodo T
  riqueza_T = 3,
  #Asignamos una probabilidad de que tengan que cuidar a una persona
  p_cuidar = runif(num_agentes, 0, 1),
  #Seleccionamos a los agentes que tendrán personas que cuidar (de acuerdo con su probabilidad)
  personas_cuidadas  = rbinom(n= num_agentes, 1:5, prob = p_cuidar),
  #Les asignamos una probabilidad de no trabajar por cuidados a quienes tienen una o más personas a las cuales cuidar
  p_no_trabajo_x_cuidados = ifelse(personas_cuidadas >= 1, 
                                   runif(num_agentes, 0, 1), 0))

#Calculamos el total de personas que requieren cuidados
total_personas_cuidadas = pull(agentes, personas_cuidadas) %>%  sum()
# Barra de progreso
progreso = progress_bar$new(total = num_periodos)
# Para cada periodo
for(t in 1:num_periodos){
  #Personas que trabajan 
  agentes = agentes %>% mutate(
    trabaja = rbinom(n = num_agentes, size = 1, 
                     #Su probabilidad de trabajar ahora dependerá también de su probabilidad de no poder hacerlo por cuidar a alguien
                     prob = pmax(runif(num_agentes, 0, 1) - pull(agentes, 
                                                                 p_no_trabajo_x_cuidados), 0)))
  # Calculamos lo que se necesita para subsidios
  subsidios = sum(pull(agentes, riqueza_T) < consumo_autonomo) * consumo_autonomo 
  # Tranferencias para quienes necesitan cuidados (10% de la producción)
  transferencias = 0.1 * produccion / total_personas_cuidadas 
  # Tabla de ganancias
  ganancias  = tibble(
    # Seleccionamos una muestra al azar con reemplazo del tamaño del número de agentes
    id = sample(x = pull(agentes %>% 
                           #solo reciben ingreso quienes trabajan 
                           filter(trabaja == 1), id), 
                size = num_agentes, replace = TRUE)
  )  %>%  
    # Contamos el número de veces que cada id recibió un ingreso
    count(id, name = 'ingreso') %>% 
    mutate(ingreso = ingreso * (produccion * (1 - 0.1) - subsidios) / num_agentes)
  
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
    label = ".", aes(y = modelo_base), 
    vjust = 1.5, linewidth = 1, color = '#D90368', linetype = 'dashed', upright= TRUE
  ) +
  # Añadimos línea con texto
  geom_textline(
    label = "Modelo con subsidios", aes(y = modelo_subsidios), 
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
    title = "Modelo con subsidios",
    subtitle = paste('Índice de Gini:', round(gini_subsidios, 2)),
    caption = 'Elaboración propia con base en el modelo de Yang & Zhou (2022)'
  ) + 
  theme_classic()

print(curva_lorenz_subsidios)
