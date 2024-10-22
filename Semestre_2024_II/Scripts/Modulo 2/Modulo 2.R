
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
## INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- INICIO: MÓDULO 2 ----- #######
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

library(tidyverse)

#-----------------------------------------#
# PRIMERA PARTE: MANIPULACIÓN DE DATOS    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------#

# Los paquetes en R son colecciones de funciones, datos y documentación
# El objetivo es extender las capacidades básicas de R

# Instalar paquete
install.packages("tidyverse")

# Cargar paquete
library(tidyverse)

# Documentación del paquete
help("tidyverse")
??tidyverse
help(filter)

dplyr::filter()

#------------------------------------------------#
# Importar bases de datos en diferentes formatos #
#------------------------------------------------#

# Como primer paso, importamos la base de datos
# Definir directorio de trabajo
file.choose()
setwd("C:/Users/PC/Desktop/Curso-EDA-2024/Semestre_2024_I/")

# Importar formato .xlsx
dataset <- readxl::read_excel("Datos/Formatos/geih_dataset.xlsx")

# Véase las rutas alternativas
dataset <- readxl::read_excel(file.choose())
# Lo mismo se puede alcanzar mediante ventanas

# Intente importar usando otras funciones
df1 <- read_csv("Datos/Formatos/geih_dataset.csv", col_names = T)
df2 <- haven::read_sav("Datos/Formatos/geih_dataset.spss")
df3 <- haven::read_dta("Datos/Formatos/geih_dataset.dta")
df4 <- haven::read_sas("Datos/Formatos/geih_dataset.sas")

# Guardemos la base de datos en .RDS
saveRDS(dataset, "Datos/Formatos/geih_dataset.rds")

# Cargar el archivo en .RDS
readRDS("Datos/Formatos/geih_dataset.rds")

# Examinar los datos importados:
View(dataset)  # Ver los datos

str(dataset)   # Ver la estructura del dataset

head(dataset)  # Mostrar las primeras filas

summary(dataset) # Resumen estadístico básico

colnames(dataset) # Nombres de columnas

##### Pregunta: Añadir un resumen estadístico sobre unas 
##### variables específicas: ingreso, horas trabajadas a la semana,
##### y tiempo de viaje

summary(dataset[c()])

##### ¿Y sobre las variables categóricas?

#-----------#
# Tidyverse #
#-----------#

# Tidyverse es una colección de paquetes disponibles en R
# para la manipulación, importación, exploración y 
# visualización de datos (dplyr, readr, ggplot2, purrr, etc.)

# Crear id en la base de datos
dataset$id <- seq(1, nrow(dataset), by = 1)

# Pipeline (%>%) o tuberías de comandos es una herramienta de "dplyr"
# para el encadenamiento de funciones

# Forma estándar:
dataset_2 <- filter(dataset, edad > 25) 

# Forma encadenada:
dataset_2 <- dataset %>% filter(edad > 25)

# En el caso anterior no es muy útil. 
# La sintaxis es más útil en expresiones extensas
# Veamos la siguiente forma estándar


# Forma estándar:
dataset_3 <- select(dataset, id, area, edad, ingreso) # seleccionar variables
dataset_3 <- filter(dataset_3, edad > 25 & edad < 40) # filtrar para (25, 40)
dataset_3 <- arrange(dataset_3, desc(edad))

# Forma encadenada
dataset_4 <- dataset %>% select(id, area, edad, ingreso) %>%
  filter(edad > 25 & edad < 40) %>% arrange(desc(edad))


#####
##### Práctica: exprese las siguientes expresiones
##### en su forma encadenada usando %>%
##### 

# Forma estándar
dataset_4 <- select(dataset, id, edad, sexo, ingreso, cotiza_fondo)
dataset_4 <- filter(dataset_4, cotiza_fondo %in% c("Colpensiones",
                                                   "Fondo privado")
                    & ingreso > 2500000)
dataset_4 <- arrange(dataset_4, desc(ingreso))

dataset_4 %>% select(id, edad, sexo, ingreso,
                     cotiza_fondo) %>% filter(cotiza_fondo %in% c("Colpensiones",
                                                                  "Fondo privado")) %>%
  arrange(desc(ingreso))

#-------------------------------#
# D. Algunas funciones en dplyr #
#-------------------------------#

# Arrange(): reordenar los datos con base en columnas
# Arrange(dataframe, variables)
new_dataset <- dataset %>% arrange(edad, ingreso)       # notar las primeras filas
new_dataset <- dataset %>% arrange(edad, desc(ingreso)) # notar las primeras filas

# Mutate(): modifica o agrega nuevas variables
new_dataset <- dataset %>% mutate(edad_int = cut(edad,
                                                 breaks = seq(0,100, by = 20),
                                                 right = F))

new_dataset <- dataset %>% mutate(edad_int = cut(edad,
                                                 breaks = seq(0,100, by = 20),
                                                 right = F,
                                                 labels = c("0-19",
                                                            "20-39",
                                                            "40-59",
                                                            "60-79",
                                                            "70-99"))) # Asignar etiquetas
View(new_dataset)

# Count(): contar categorías en una tabla
# Count(dataframe, variable, sort, name)
new_table <- new_dataset %>% count(cotiza_fondo, sort =T, name = "n")
new_table <- new_dataset %>% count(cotiza_fondo, sexo, sort = T, name = "n")

# Filter(): subconjunto de datos definido por una condición lógica
newest_dataset <- new_dataset %>% filter(sexo == "M")

# Group_by(): subconjunto de datos según categorías. (VER FIGURA)
# (Generalmente se emplea con summarize() para descriptivas diferenciadas)
newest_dataset <- new_dataset %>% group_by(area) %>% summarize(mean_y = 
                                                                 mean(ingreso, na.rm = T),
                                                               sd_y = sd(ingreso, na.rm = T),
                                                               q1_y = quantile(ingreso,0.25, na.rm = T),
                                                               q2_y = quantile(ingreso, 0.5, na.rm = T),
                                                               q3_y = quantile(ingreso, 0.75, na.rm = T))

# Rename(): renombrar columnas (también rename_with())
newest_dataset <- new_dataset %>% rename(income = ingreso,
                                         sex = sexo,
                                         age = edad) #renombrar
newest_dataset <- new_dataset %>% rename_with(tolower) # Todo a minúsculas
newest_dataset <- new_dataset %>% rename_with(toupper) # Todo a mayúsculas

# Select(): seleccionar de un subconjunto de variables (VER FIGURA)
newest_dataset <- new_dataset %>% select(id, edad, sexo,
                                         ingreso, cotiza_fondo)

# Summarize(): resumen descriptivo general y diferenciado 
table_1 <- new_dataset %>% filter(area == "Cali") %>% 
  summarize(mean_y = mean(ingreso, na.rm = T),
            sd_y = sd(ingreso, na.rm = T)) # Resumen descriptivo con filtro

table_2 <- new_dataset %>% group_by(area) %>% 
  summarize(mean_y = mean(ingreso, na.rm = T),
            sd_y = sd(ingreso, na.rm = T)) # Resumen descriptivo diferenciado

# Resumen descriptivo aplicado a varias columnas
table_3 <- new_dataset %>% group_by(area) %>% summarise(across(c("edad", "ingreso"),
                                                               ~ mean(.x, na.rm = T)))

# Resumen descriptivo aplicado a varias columnas: todas las variables numéricas
table_4 <- new_dataset %>% select(-c(id)) %>% group_by(area) %>% summarise(across(where(is.numeric),
                                                                                  ~ mean(.x, na.rm = T)))

# Resumen descriptivo: múltiples funciones a múltiples columnas
table_5 <-  new_dataset %>% select(-c(id)) %>% group_by(area) %>% summarise(across(c("edad", "horas_semana"),
                                                                                   list(mean = mean,
                                                                                        sd = sd)))
# ¿Y si la función arroja un data frame?
md_df <- function(x){
  data.frame(mean = mean(x, na.rm = T),
             sd = sd(x, na.rm = T))
}

table_5 <-  new_dataset %>% group_by(area) %>% summarise(across(c("edad"), md_df))

#### Práctica: prepare el siguiente resumen descriptivo para ingresos entre 2 y 10 millones:
####
#### muestre cómo cambian las principales descriptivas del ingreso laboral
#### (media, sd, mediana, q1 y q3) según el nivel educativo
#### Recomendación: cree una función que permita hacer el resumen descriptivo


#-------------------------------#
# E. Uniones de bases de datos  #
#-------------------------------#

# Para el ejemplo, eliminemos la descripción de la actividad económica:
dataset <- dataset %>% select(-actividad)

# Recuperar datos sobre el hogar y la vivienda
ciiu <- readxl::read_excel("Datos\\CIIU_REV_4.xls")

# Mantener la división
ciiu <- ciiu %>% select(División, Descripción)

# Mantener los valores numéricos y distintos de NA
ciiu <- ciiu %>%
  filter(!is.na(as.numeric(División))) 
ciiu$División <- as.numeric(ciiu$División)

dataset_ciiu <- left_join(dataset,
                   ciiu, by = c("rama_2" = "División"))

dataset_ciiu <- right_join(dataset,
                    ciiu, by = c("rama_2" = "División"))

dataset_ciiu <- inner_join(dataset,
                    ciiu, by = c("rama_2" = "División"))

dataset_ciiu <- full_join(dataset,
                   ciiu, by = c("rama_2" = "División"))

# Se puede seguir la sintaxis %>%
# Agregar definitivamente a la base de datos
dataset <- dataset %>% left_join(ciiu, by = c("rama_2" = "División"))

# Renombrar la variable como "Actividad"
dataset <- dataset %>% rename(Actividad = Descripción)

#----------------------#
# F. Sintaxis: ggplot2 #
#----------------------#

# Ggplot2 es una librería para la visualización de datos
# Sintaxis general: ggplot(dataset, estética) + geometría + 
# opciones + faceta (ver diapositiva)

# Nos centramos en las personas que cotizan
new_dataset2 <- new_dataset %>% filter(ingreso > 2000000 &
                                         ingreso < 10000000 &
                                         cotiza_fondo != "No cotiza")

ggplot2::ggplot(new_dataset2, aes(x = edad,
                                  y = ingreso, 
                                  color = cotiza_fondo))+
  geom_point() 

# Agregar etiquetas y definir límites
ggplot2::ggplot(new_dataset2, aes(x = edad,
                                  y = ingreso/1000, 
                                  color = cotiza_fondo))+
  geom_point() + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones"
    
  )


# Ajustar el color
library(RColorBrewer) # Examinar paletas de colores

ggplot2::ggplot(new_dataset2, aes(x = edad,
                                  y = ingreso/1000, 
                                  color = cotiza_fondo))+
  geom_point() + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones") + 
  scale_color_brewer(palette = "PuOr") # Reds

# Agregar el ajuste lineal: geom_smooth()
ggplot2::ggplot(new_dataset2, aes(x = edad,
                                  y = ingreso/1000, 
                                  color = cotiza_fondo))+
  geom_point() +
  geom_smooth(method = "lm", color = "black") + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones") + 
  scale_color_brewer(palette = "PuOr")


# Gráfico en ggplot2 con facetas
# Incorporamos el plan de pensiones como una faceta
ggplot2::ggplot(new_dataset2, aes(x = edad,
                                  y = ingreso/1000, 
                                  color = cotiza_fondo))+
  geom_point() + 
  facet_wrap(~cotiza_fondo, scale = "free_y") + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones") + 
  scale_color_brewer(palette = "PuOr")


#-----------------------------------------------------------------------#
# Anexo*: funciones útiles para el tratamiento de variables categóricas #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------#

#--------------------------------------------------------------#
# G. Funciones: forcats (tratamiento de variables categóricas) #
#--------------------------------------------------------------#

# Declarar variable como factor
new_dataset$edu = factor(new_dataset$edu)

# fct_relevel()
# Reordenar manualmente los niveles de un factor
# Organiza los niveles después del 4to
fct_relevel(new_dataset$edu, c("Primaria", "Secundaria"),
            after = 4)


# fct_infreq()
# Reordenar los valores de acuerdo con su frecuencia
fct_infreq(new_dataset$edu)

# fct_shift()
# Reordenar moviendo los niveles de izquierda a derecha
fct_shift(new_dataset$edu)

# fct_recode()
# Recodificar manualmente los niveles de las variables categóricas
fct_recode(new_dataset$edu,
           `Maestria` = "Maestría")

# fct_collapse()
# Agrupar distintos niveles en un grupo específico
fct_collapse(new_dataset$edu,
             `Posgrado` = c("Maestría", "Doctorado"))

# fct_lump_prop()
# Agrupar niveles en categoría "Otros" de acuerdo con una proporción
fct_lump_prop(new_dataset$edu, 
              prop = 0.10, other_level = "Other") 

fct_lump_prop(new_dataset$edu, 
              prop = 0.04, other_level = "Other") 

# fct_lump_n()
# Agrupa niveles según la posición

fct_lump_n(new_dataset$edu, 
           n = 2, other_level = "Other") 

# fct_reorder()
# Reordenar factor variable ordenando según otra variable
ggplot2::ggplot(new_dataset, aes(x = cotiza_fondo,
                                   y = edad, fill = cotiza_fondo)) + 
  geom_boxplot() + scale_fill_brewer(palette = "PuOr") # Gráfico no ordenado

ggplot2::ggplot(new_dataset, aes(x = fct_reorder(cotiza_fondo, edad),
                                 y = edad, 
                                 fill = cotiza_fondo)) + 
  geom_boxplot() + scale_fill_brewer(palette = "PuOr") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Gráfico ordenado


#### Práctica: cree un mapa similar para contrastar los ingresos y el área:
####
#### emplee la geometría geom_boxplot() y ordene el área según los ingresos
#### recuerde ajustar la escala ylim()

  
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
## FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2 ----- FIN DEL MÓDULO 2---- ##
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
