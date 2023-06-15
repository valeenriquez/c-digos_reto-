## GRÁFICAS ##

# Cargamos las librerías
library(ggplot2)
library(tidyverse)
library(echarts4r)

# Población ---------------------------------------------------------------

# Íconos de la gráfica 
mujer <-"path://M20.822 19.307c-2.967-.681-6.578-2.437-5.514-4.723.684 1.126 2.801 1.777 4.45.804-4.747-1.204 2.334-9.471-3.871-14.105-1.135-.853-2.526-1.283-3.912-1.283-1.378 0-2.751.425-3.862 1.283-6.206 4.634.876 12.901-3.872 14.105 1.649.974 3.77.293 4.451-.804 1.064 2.286-2.551 4.043-5.514 4.723-2.978.682-3.178 2.466-3.178 4.004l.005.689h23.99l.005-.691c0-1.537-.2-3.32-3.178-4.002z"

hombre <- "path://M20.822 18.096c-3.439-.794-6.64-1.49-5.09-4.418 4.72-8.912 1.251-13.678-3.732-13.678-5.082 0-8.464 4.949-3.732 13.678 1.597 2.945-1.725 3.641-5.09 4.418-3.073.71-3.188 2.236-3.178 4.904l.004 1h23.99l.004-.969c.012-2.688-.092-4.222-3.176-4.935z"

# Construimos el data frame 
sexos <- data.frame(
  sexo=c("Hombres", "Mujeres"),
  value=c(47.11, 52.89),
  path = c(hombre, mujer)
)

# Construimos la gráfica con la paquetería echarts4r
sexos %>% 
  e_charts(sexo) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel= list(show=FALSE)) %>%
  e_y_axis(max=120, 
           splitLine=list(show = FALSE),
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel=list(show=FALSE)) %>%
  e_color(color = c('thistle','gainsboro')) %>%
  e_pictorial(value, 
              symbol = path, 
              z=10,
              name= 'realValue', 
              symbolBoundingData= 100, 
              symbolClip= TRUE) %>% 
  e_pictorial(value, 
              symbol = path, 
              name= 'background', 
              symbolBoundingData= 100) %>% 
  e_labels(position = "bottom", offset= c(0, 10), 
           textStyle =list(fontSize= 20, 
                           fontFamily= 'Arial', 
                           fontWeight ='bold', 
                           color= 'thistle'),
           formatter="{@[1]}% {@[0]}") %>%
  e_legend(show = FALSE) %>%
  e_theme("wonderland") %>%
  e_title("Porcentaje de hombres y mujeres de 15 años y más en 2019", left='center')

# REGIÓN -----------------------------------------------------------------

# Creamos el data frame
region <- data.frame(
  category= c("Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur", "Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur"), 
  value= c(37.93, 14.32, 18.74, 6.96, 22.05, 38.66, 14.28, 17.48, 6.85, 22.73),
  grupo=c ("Hombre","Hombre","Hombre","Hombre","Hombre","Mujer","Mujer","Mujer","Mujer","Mujer")
)

# Construimos la gráfica
ggplot(region, aes(fill=category, y=value, x=grupo)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("") +
  xlab("")+
  ylab("")+
  labs(fill = "Región" )+
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3))+
  #theme(legend.position = "none") +  
  theme_light() + 
  theme(panel.grid = element_line(color="white"))+
  facet_grid(~"Sexo por región") +               
  coord_flip()+
  scale_fill_brewer(palette=11)+
  labs(caption = "Fuente: Elaboración propia con datos de la ENUT, 2019")+
  geom_text(aes(label=paste0(value,  "%")),    
            vjust=0.5,                      
            hjust=1.2,                         
            angle=0,                           
            size=3, 
            position = position_stack(0.8)
  )

# ACCESO A GUARDERIA NIÑOS ------------------------------------------------------

# Creamos el data frame
acceso <- data.frame(
  category= c("Si tiene acceso","No tiene acceso", "No sabe si tiene accso","Si tiene acceso","No tiene acceso", "No sabe si tiene accso"),
  value=c(12.75, 42.13, 1.35, 10.31, 21.98, 0.62),
  grupo=c("Hombre","Hombre","Hombre","Mujer","Mujer","Mujer")
)
# Construimos la gráfica
ggplot(acceso, aes(fill=category, y=value, x=grupo)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("") +
  xlab("")+
  ylab("")+
  labs(fill = "" )+
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3))+
  #theme(legend.position = "none") +  
  theme_light() + 
  theme(panel.grid = element_line(color="white"))+
  facet_grid(~"Acceso a estancias/guarderías para personas que trabajan") +               
  coord_flip()+
  scale_fill_brewer(palette=11)+
  labs(caption = "Fuente: Elaboración propia con datos de la ENUT, 2019")+
  geom_text(aes(label=paste0(value,  "%")),    
            vjust=0.5,                      
            hjust=0.4,                         
            angle=0,                           
            size=3, 
            position = position_stack(0.4)
  )


# PROBABILIDADES HOGAR ----------------------------------------------------

# Construimos el data frame
hogares <- data.frame(
  region = c("Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur"), 
  pers_disc = c(6.17, 5.72, 5.62, 7.40, 7.58), 
  infantes = c(32.93, 14.86, 19.8, 7.1, 25.32)
)

# Convertir el data frame a formato largo (long format)
df_long <- tidyr::pivot_longer(hogares, cols = c("pers_disc", "infantes"), names_to = "variable", values_to = "valor")

# Creamos la gráfica
ggplot(df_long, aes(x = region, y = valor, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Región", y = "Valor", fill = "Variable") +
  theme_light() + 
  theme(panel.grid = element_line(color="white"))+
  #facet_grid(~"Probabilidad en los hogares")+ 
  xlab("")+
  ylab("")+
  labs(fill = "" )+
  facet_grid(~"Probabilidad en los hogares de tener niños y personas con discapacidad") +
  scale_fill_brewer(palette=11)+
  labs(caption = "Fuente: Elaboración propia con datos de la ENUT, 2019")+
  coord_polar(theta="y")+
  geom_text(aes(label=paste0(valor,  "%")),    
            vjust=0.5,                      
            hjust=0.4,                         
            angle=0,                           
            size=3, 
            position = position_stack(0.4))+
  coord_polar(theta="y")

# Opción 2 
ggplot(df_long, aes(x = region, y = valor, color = variable, shape = variable)) +
  geom_segment(aes(xend = region, yend = 0), color = "gray96", size=0.5) +
  geom_point(size = 3.5, fill = "white") +
  labs(x = "Región", y = "Valor", color = "Variable",
       caption = "Fuente: Elaboración propia con datos de la ENUT, 2019") +
  scale_color_manual(values = c("pers_disc" = "#CD608F", "infantes" = "#37A6A6")) +
  scale_shape_manual(values = c("pers_disc" = 22, "infantes" = 23)) +
  theme_light()+
  ggtitle("") +
  xlab("")+
  ylab("")+
  #theme(panel.grid = element_line(color="white"))+
  scale_x_discrete(position = "top")+
  facet_grid(~"Probabilidad en los hogares de tener niños y personas con discapacidad")


# CUIDAR A UN ADULTO MAYOR  -----------------------------------------------

# Construimos el data frame
adulto<- data.frame(
  region = c("Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur", "Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur"), 
  sexo=c("Hombres", "Hombres", "Hombres", "Hombres", "Hombres", "Mujeres","Mujeres", "Mujeres", "Mujeres", "Mujeres"),
  valor= c(2.15, 2.96, 2.3, 2.41, 2.87, 3.17, 3.79, 3.48, 3.91, 3.05)
)

# Creamos la gráfica de parallel plot
ggplot(adulto, aes(x = region, y = valor, group = sexo, color = sexo)) +
  geom_line(aes(linetype = sexo)) +
  geom_point(size = 5, shape = 21) +
  labs(x = "Región", y = "Valor", color = "Sexo") +
  scale_linetype_manual(values = c("solid", "solid"))+ 
  theme_light()+
  ggtitle("") +
  xlab("")+
  ylab("")+
  theme(panel.grid = element_line(color="white"))+
  #facet_grid(~"Probabilidad de tener que cuidar a un adulto mayor")+
  scale_color_manual(values=c("#37A6A6","#CD608F"))+
  labs(caption = "Fuente: Elaboración propia con datos de la ENUT, 2019")+
  scale_x_discrete(position = "top")+
  facet_grid(~"Porcentaje de personas que tienen que cuidar a un adulto mayor")

# INFORMALIDAD ------------------------------------------------------------

# Construimos el data frame
df <- data.frame(
  region = c("Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur", "Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur"), 
  sexo=c("Hombres", "Hombres", "Hombres", "Hombres", "Hombres", "Mujeres","Mujeres", "Mujeres", "Mujeres", "Mujeres"),
  informalidad = c(42.58, 41.58, 28.03, 40.44, 53.28, 26.79, 25.17, 18.95, 24.77, 29.08 )
)

# Creamos el mapa de calor 
ggplot(df, aes(x = region, y = sexo, fill = informalidad)) +
  geom_tile() +
  labs(fill = "Informalidad",
       caption = "Fuente: Elaboración propia con datos de la ENOE, 2019") +
  scale_fill_gradient(low = "white", high = "#CD608F") +
  theme_light()+
  ggtitle("") +
  xlab("")+
  ylab("")+
  theme(panel.grid = element_line(color="white"))+
  #scale_x_discrete(position = "top")+
  geom_text(aes(label=paste0(informalidad,  "%")),    
            vjust=0.5,                      
            hjust=0.4,                         
            angle=0,                           
            size=3, 
            position = position_stack(0.4))+
            facet_grid(~"Porcentaje de la población informal por sexo y región")

# TRABAJO -----------------------------------------------------------------

# Construimos el data frame
df_1 <- data.frame(
  region = c("Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur", "Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur"), 
  sexo=c("Hombres", "Hombres", "Hombres", "Hombres", "Hombres", "Mujeres","Mujeres", "Mujeres", "Mujeres", "Mujeres"),
  valor= c(73.13, 75.67, 74.61, 75.12, 76.24, 44.44, 44.38, 45.96, 45.58, 40.49)
)

# Creamos la gráfica de parallel plot
ggplot(df_1, aes(x = region, y = valor, group = sexo, color = sexo)) +
  geom_line(aes(linetype = sexo)) +
  geom_point(size = 2, shape = 21) +
  labs(x = "Región", y = "Valor", color = "Sexo",
       caption = "Fuente: Elaboración propia con datos de la ENOE, 2019") +
  scale_linetype_manual(values = c("solid", "solid"))+ 
  theme_light()+
  ggtitle("") +
  xlab("")+
  ylab("")+
  theme(panel.grid = element_line(color="white"))+
  scale_color_manual(values=c("#37A6A6","#CD608F"))+
  #labs(caption = "Fuente: ENUT, 2019")+
  scale_x_discrete(position = "top")+
  facet_grid(~"Porcentaje de personas que trabajan ")


# NO EMPLEO POR CUIDADOS -------------------------------------------------------------

# Construimos el data frame
df_2 <- data.frame(
  region = c("Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur", "Centro", "Centro-norte", "Norte", "Norte-occidente", "Sur"), 
  sexo=c("Hombres", "Hombres", "Hombres", "Hombres", "Hombres", "Mujeres","Mujeres", "Mujeres", "Mujeres", "Mujeres"),
  no_trabajo = c(0.05, 0.05, 0.09, 0.05, 0.04, 3.43, 2.47, 2.81, 3.36, 3.73) )

# Crear el gráfico de lollipop plot dividido por sexo
ggplot(df_2, aes(x = region, y = no_trabajo, color = sexo)) +
  geom_segment(aes(xend = region, yend = 0), size = 1) +
  geom_point(aes(shape = sexo), size = 3, fill = "white") +
  labs(x = "Región", y = "No Trabajo", color = "Sexo") +
  scale_shape_manual(values = c("Hombres" = 21, "Mujeres" = 23)) +
  theme_light()+
  ggtitle("") +
  xlab("")+
  ylab("")+
  #theme(panel.grid = element_line(color="white"))+
  scale_color_manual(values=c("#37A6A6","#CD608F"))+
  labs(caption = "Fuente: Elaboración propia con datos de la ENOE, 2019")+
  #scale_x_discrete(position = "top")+
  facet_grid(~"Porcentaje de personas que quieren o necesitan trabajar, pero no pueden porque realizan servicio de cuidados")
  

# INFORMALIDAD NACIONAL ---------------------------------------------------

# Construimos el data frame
data33<- data.frame(
  sexo= c("Hombres", "Mujeres"),
  value = c(41.88,25.54),
  path = c(hombre, mujer))

# Creamos la gráfica con íconos
data33 %>% 
  e_charts(sexo) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel= list(show=FALSE)) %>%
  e_y_axis(max=120, 
           splitLine=list(show = FALSE),
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel=list(show=FALSE)) %>%
  e_color(color = c("#CD608F",'gainsboro')) %>%
  e_pictorial(value, 
              symbol = path, 
              z=10,
              name= 'realValue', 
              symbolBoundingData= 100, 
              symbolClip= TRUE) %>% 
  e_pictorial(value, 
              symbol = path, 
              name= 'background', 
              symbolBoundingData= 100) %>% 
  e_labels(position = "bottom", offset= c(0, 10), 
           textStyle =list(fontSize= 20, 
                           fontFamily= 'Arial', 
                           fontWeight ='bold', 
                           color= "#CD608F"),
           formatter="{@[1]}% {@[0]}") %>%
  e_legend(show = FALSE) %>%
  e_theme("wonderland") %>%
  e_title("Porcentaje de la población informal por sexo", left='center')

