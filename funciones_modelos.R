# Fijar el directorio de trabajo 
setwd("~/Documents/Ciencia de datos/Reto")

# Le asignamos una función a cada uno de los modelos, eliminando 
# las gráficas de cada uno de sus códigos para que, al momento de correr
# la función varias veces, sea más rápido obtener los resultados 


# Función para el modelo de servicios de cuidado 2019 -----------------------------------------------

modelo_servicio_cuidados <- function(
    num_agentes = 5590,
    
    #Importamos el csv de condiciones inciales
    agentes = read_csv('Condiciones iniciales.csv') %>%
      mutate(
        #Seleccionamos a los agentes que tendrán discapacitados (de acuerdo con su probabilidad)
        discapacitados  = rbinom(n= num_agentes, 1:5, prob = p_discapacitado),
        #Seleccionamos a los agentes tendrán acceso a una estancia para discapacitados (de acuerdo con su probabilidad)
        acceso_a_estancias_disc = rbinom(n= num_agentes, 1, prob = p_estancias_disc),
        #Seleccionamos a los agentes que tendrán adultos mayores (de acuerdo con su probabilidad)
        ancianos  = rbinom(n= num_agentes, 1:5, prob = p_anciano),
        #Seleccionamos a los agentes tendrán acceso a una estancia para adultos mayores (de acuerdo con su probabilidad)
        acceso_a_estancias_am = rbinom(n= num_agentes, 1, prob = p_estancias_am),
        #Seleccionamos a los agentes que tendrán hijos (de acuerdo con su probabilidad)
        hijos = rbinom(n= num_agentes, 1:5, prob = p_hijos),
        #Seleccionamos a los agentes tendrán acceso a guarderías (de acuerdo con su probabilidad)
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
        #a estancias disminuye con base en su probabilidad de dejar de trabajar por cuidados, dado 
        #que no pueden contratar a una persona cuidadora
        p_trabajo = ifelse(personas_cuidadas >= 1 & acceso_a_estancias == 0, 
                           pmax((p_trabajo - (p_no_trabajo_x_cuidados / (p_no_trabajo_x_cuidados + 1 - p_cuidados))), 
                                0), p_trabajo),
        #Probabilidad de ser formal dado que trabajan
        p_formalidad = 1- (p_informalidad / (p_informalidad + p_trabajo))
      ),
    
    # Número de periodos
    num_periodos = 1000,
    #Transferencias 
    transferencia = 1,
    # Producción
    produccion = num_agentes * transferencia,
    # Consumo autónomo
    consumo_autonomo = 0.32,
    # Propensión marginal a consumir (INEGI, 2019)
    pmc = 0.68){
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
           riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))
  
  
  return(riqueza_series)
  
}


# Función para el modelo de subsidios -----------------------------------------------
  modelo_subsidios <- function(
    num_agentes = 5590,
    
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
      ),
    # Número de periodos
    num_periodos = 1000,
    #Transferencias 
    transferencia = 1,
    # Producción
    produccion = num_agentes * transferencia,
    # Consumo autónomo
    consumo_autonomo = 0.32,
    # Propensión marginal a consumir (INEGI, 2019)
    pmc = 0.68,
    total_personas_cuidadas = pull(agentes,personas_cuidadas) %>%  sum()
  )

  {
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
      #El 20% del PIB mexicano en 2019 se destinó a pensiones
      transferencias =  0.2 * produccion / total_personas_cuidadas
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
          ingreso_total = NULL
        )
      
      # Barra de progreso
      progreso$tick()
    }
    # Transformamos la serie de tiempo a formato long
    riqueza_series = agentes %>% 
      # Seleccionamos todo, menos la riqueza_T y las probabilidades 
      select(-c(riqueza_T, p_no_trabajo_x_cuidados,
                p_trabajo, p_informalidad, p_anciano,
                p_discapacitado, p_cuidados)) %>% 
      # Cambiamos a formato long, manteniendo el id, sexo y region intactos
      gather(time, riqueza, -c(id, sexo, region)) %>% 
      # Quitamos el texto de la columna de tiempo
      mutate(time = as.numeric(str_remove(time, 'riqueza_')),
             riqueza = (riqueza - min(riqueza)) / (max(riqueza) - min(riqueza)))
    
    
    return(riqueza_series)
    
  }









  
 