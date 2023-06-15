# Fijar el directorio de trabajo donde se guardó el código para las funciones
# que guardan los modelos que se busca correr varias veces 

setwd("~/Documents/Ciencia de datos/Reto")

# 1.  Librerías -----------------------------------------------------------
# Instalar -  cargar DBI                                                       
if(require(DBI) == FALSE){                                                
  install.packages('DBI')                                                 
  library(DBI)                                                            
}else{                                                                          
  library(DBI)                                                            
}
# Instalar - cargar RSQLite                                                       
if(require(RSQLite) == FALSE){                                                
  install.packages('RSQLite')                                                 
  detach("package:RSQLite", unload = TRUE)
}
# Instalar - cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Progress                                                       
if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress)                                                            
}

# Simulaciones para el modelo de cuidados -----

# Cargamos el código donde se guardó la función para el modelo de cuidados
source('4.1 Funciones_modelos.R')

# Construyendo la base de datos  
# Creamos la conexión para la base de datos: "modelo.db"
# este archivo db se guarda en tu directorio de trabajo 
connection <- dbConnect(RSQLite::SQLite(), "modelo.db")

# Enlistamos todas las tablas de la base de datos 
dbListTables(connection)

# ELIMINAR TABLA EN CASO DE QUE EXISTA
dbRemoveTable(connection, "modelo_cuidados_")

# Enlistamos las tablas disponibles en la base de datos 
dbListTables(connection)

# CREAMOS TABLA
statement = "CREATE TABLE modelo_cuidados_
          (
            run_id INTEGER, 
            agent_id INTEGER,
            time INTEGER,
            wealth REAL,
            sexo STRING, 
            region STRING
          );"

dbExecute(conn = connection, statement = statement)

# Enlistar las tablas disponibles 
dbListTables(connection)

# Hacemos múltiples simulaciones
for (run in 1:1000) {
  # Transformamos la serie de tiempo a long format
  modelo_servicio_cuidados() %>% 
    mutate(
      # Agregamos el run_id
      run_id = run
    ) %>% 
    # Ordenamos la estructura de la tabla
    select(run_id, agent_id = id, time, wealth = riqueza, sexo, region) %>% 
    # Guardamos los resultados en nuestra base de datos
    dbWriteTable(con = connection, name = "modelo_cuidados_", value = ., 
                 append = TRUE, row.names=FALSE, overwrite=FALSE)
  
}

# Enlistamos las tablas disponibles 
dbListTables(connection)

# Crear query
dplyr_query = tbl(connection, 'modelo_cuidados_') %>% 
  # Guardar las últimas observaciones
  filter(time == 1000)

# Resultado
result = collect(dplyr_query)
result
num_agentes = 5590
gini_cuidados = NULL
for (i in 1:1000) {
  gini_datos = result %>% 
  filter(run_id == i) %>% 
    # Ordenamos por riqueza
    arrange(wealth) %>% 
    mutate(
      # Normalizamos la riqueza
      riqueza_i = wealth,
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
  gini_cuidados[i] = gini_datos %>% 
    reframe(gini =  2 * mean(difference)) %>% 
    pull(gini)
  
}

# Promedio del índice de gini para las simulaciones 
 mean(gini_cuidados)


# Simulaciones para el modelo de subsidios ----------

# Enlistamos todas las tablas de la base de datos 
dbListTables(connection)

# ELIMINAR TABLA EN CASO DE QUE EXISTA
dbRemoveTable(connection, "modelo_subsidios_")

# Enlistamos las tablas disponibles 
dbListTables(connection)

# CREAR TABLA
statement = "CREATE TABLE modelo_subsidios_
          (
            run_id INTEGER, 
            agent_id INTEGER,
            time INTEGER,
            wealth REAL,
            sexo STRING, 
            region STRING
          );"

dbExecute(conn = connection, statement = statement)

# Enlistamos las tablas disponibles
dbListTables(connection)

# Corremos varias simulaciones
for (run in 1:1000) {
  # Transformamos la base de datos en long format
  modelo_subsidios() %>% 
    mutate(
      # Add run_id
      run_id = run
    ) %>% 
    # Ordenamos la estructura de la tabla 
    select(run_id, agent_id = id, time, wealth = riqueza, sexo, region) %>% 
    # Guardamos los resultados en nuestra base de datos
    dbWriteTable(con = connection, name = "modelo_subsidios_", value = ., 
                 append = TRUE, row.names=FALSE, overwrite=FALSE)
  
}

# Enlistamos las tablas disponibles 
dbListTables(connection)

# Crear query
dplyr_query = tbl(connection, 'modelo_subsidios_') %>% 
  # Nos quedamos con las últimas observaciones
  filter(time == 1000)

# Resultados 
result = collect(dplyr_query)
result
num_agentes = 5590
gini_subsidios = NULL
for (i in 1:1000) {
  gini_datos = result %>% 
    filter(run_id == i) %>% 
    
    # Ordenamos por riqueza
    arrange(wealth) %>% 
    mutate(
      # Normalizamos la riqueza
      riqueza_i = wealth,
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
  gini_subsidios[i] = gini_datos %>% 
    reframe(gini =  2 * mean(difference)) %>% 
    pull(gini)
  
}

# Promedio de gini de las simulaciones 
mean(gini_subsidios)

# Disconnect
dbDisconnect(conn = connection)

