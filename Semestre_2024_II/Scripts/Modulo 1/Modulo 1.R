
############################
############################
## Módulo I: básicos de R ##
############################
############################

############################################
############################################
#------------------------------------------#
# PRIMERA PARTE ESTRUCTURAS DE DATOS EN R  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------#
############################################
############################################


#------------#
# Vectores   #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------#

#### Los objetos son nombres para referirnos a datos almacenados
x <- 1:6



#### Sensible a mayúsculas
A1 <- 1 

a1 <- 2

a1 + 1



#### Crear un vector
text <- c("x", "y", "z") 



#### Crear un vector usando secuencias
num <- 50:54



#### Seleccionar elementos del vector
num[1] # elemento 1



num[-1] # descartar elemento 1



num[c(2,3)] #  elemento 2 y 3



num[-c(2,3)] # descartar elementos 2 y 3



#### Características del vector
length(num) # longitud 



class(num) # tipo

names(num) <- c("uno", "dos", "tres",
                "cuatro", "cinco") # asignar nombres

# Nota: la clase del vector también se puede verificar usando:
is.numeric(num)     # TRUE
is.character(num)   # FALSE
is.factor(num)   # FALSE
is.matrix(num)      # FALSE
is.data.frame(num)  # FALSE

# Y para convertir los objetos a otras clases:
as.numeric(num)
as.character(num)
as.factor(num)
as.matrix(num)
as.data.frame(num)

#### Aritmética con vectores (ejecución por elementos)
v1 <- c(2,3,5,7,11, 13)

v2 <- c(2,4,6,8,10,12)

v1*v2 

v1+v2

# ejecución por elementos con distinta dimesión
# cuando la longitud de v2 es múltiplo de la longitud de v1
v2 <- c(1,2,3)

v1*v2

v1+v2

#### Construcción de patrones
seq1 <- seq(1, 20, by = 2)

rep1 <- rep(8,12)


#### Los vectores pueden ser concatenados usando c()
v2 <- c(num, 2:40)

##### Crear factores
gender <- factor(c("male", "female", "female", "female"))

# Recodificar niveles
gender.recod <- factor(gender,
                      levels = c("male", "female"),
                      labels = c("M", "F")) 


#### Ejercicio ####################

# Crear un vector de longitud 5 que contiene edades
# Validar la clase del objeto
# Calcular el vector de la edad al cuadrado
# Seleccionar el 4 elemento
# Remover los elementos 1 y 3

#### Solución ####################

#------------#
# Matrices   #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------#

#### Crear matriz

# Crear según columnas (default)
m1 <- matrix(c(1,2,4,6,7,4,1,3,2),nrow = 3, ncol = 3, byrow = F)

# Crear según filas
m2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = T)

# Seleccionar entradas
m1[2,1] # entrada (2,1)
m1[2,]  #  fila 2
m1[,1]  #  columna 1

# Descartar entradas
m1[-2,] # fila 2
m1[,-1] # columna 1
m1[-2,-1] # ambas

# Agregar filas o columnas
rbind(m1,m2)
cbind(m1,m2)

# Verificar dimensión
dim(m1)
nrow(m1)
ncol(m1)

# Extraer diagonal 
diag(m1)

#----------------------------#
# Operaciones con matrices   #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------#

# Suma y resta
m1 + m2
m1 - m2

# La ejecución por elementos no corresponde a la operación por matrices
m1*m2

# Producto de matrices
m1 %*% m2

# Otras operaciones útiles
t(m1) # Traspuesta
2*m1  # Producto por escalar
det(m1) # Determinante
solve(m1) # Inversa

#### Ejercicio 1 ####################

#####  Crear una matriz de dimensión 5x2 a partir de
edad <- c(12, 16, 24, 8, 56)
edu <- c(6, 11, 16, 2, 20)

##### Validar que el objeto es una matriz
#####  Encuentre la dimensión 
#####  Encuentre el número de filas
#####  Encuentre el número de columnas
#####  Encuentre el elemento de la fila 2 y columna 1
#####  Elimine las filas 3 y 4

#### Solución ####################

#### Ejercicio 2 ####################

##### Añadir la siguiente matriz a la matriz del ejercicio anterior (función rbind)
#####  Cree una matriz de tamaño 5x2 donde las columnas corresponden a
#####  la edad y la educación al cuadrado.
##### Añadir la nueva a la matriz anterior (función cbind)


#### Solución ####################

#    [,1] [,2] [,3] [,4]
#[1,]   12    6  144   36
#[2,]   16   11  256  121
#[3,]   56   20  576  256
#[4,]   25   13   64    4
#[5,]   10    4 3136  400

#---------------#
# Data frames   #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------#

# Creemos un data frame a partir de los siguientes vectores
id <- 1:4
sexo <- c("M", "M", "F", "F")
edad <- c(15, 26, 43, 56)

df <- data.frame(id, sexo, edad)

# La selección de entradas es análoga a las matrices
df[1,2]
df[c(1,2),]
df[,c(2,3)]

df[-c(1,2),]
df[,-2]

# Adicionalmente, la siguiente sintaxis es útil:
df$edad                
df$edad[1]              
df[c("id", "edad")]   

# Usar la función subset
subset(df, select = c(id, edad))
subset(df, select = -c(id, edad))

# Usar summary() para un resumen del data frame
summary(df)

#### Ejercicio ####################

##### Verifique que rbind() y cbind() también
##### se puede usar en el caso de data frames. 
##### Cree dos data frames nuevos y emplee las funciones

#-------------#
# Listas      #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------#

# La lista nos permite incluir todos los objetos definidos anteriormente
lista <- list(vector = v1,
              matriz = m1,
              dataf = df)
lista

# Sintaxis para listas
lista$vector2        # seleccionar según el nombre
lista[[2]]           # segundo objeto de la lista
lista[[2]][1]        # primer elemento del segundo objeto de la lista

# Añadir nuevos elementos a una lista
lista$new1 <- seq(1, 20, by = 3)              # usando operador $

lista <- c(lista, 
           list(names = c("a", "b")))         # usando c()

lista <- append(lista,
                list(names2 = c("d", "e")),   # usando la función append()
                after = 2)

# Note que NO es posible vectorizar operaciones
lista/2

#### Ejercicio sobre Indexación ####################

##### Remover el vector de la lista
##### Seleccionar el la matriz de la lista
##### Seleccionar la segunda columna de la matriz en la lista 
##### Seleccionar la edad de la segunda observación del dataframe en la lista

############################################
############################################
#------------------------------------------#
# SEGUNDA PARTE: CONDICIONALES Y FUNCIONES #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------#
############################################
############################################

# Probar operadores
x <- 3

x > 3
x >= 3
x == 3
x != 3

# Usar operadores lógicos en la indexación:
v1[v1 > 3]

m1[m1[,2] > 3,]

df[df$edad > 30,]


# Siguiendo la estructura general, definimos el siguiente condicional
x <- 2
if (x > 5) {
  print("x es mayor que 5")
}

# De otro modo:
x <- 2
if (x > 5) {
  print("x es mayor que 5")
} else {
  print("x es menor o igual que 5")
}

# Siguiendo la estructura general, definimos una función para estandarizar 
# variables
f_z = function(x){
  z <- (x - mean(x))/sd(x)
  return(z)
}

x <- 1:20
z <- f_z(x)
mean(z)
sd(z)

# Ambas utilidades: definición de la función a partir de un condicional
# Operador de módulo: %%

par_impar <- function(x){
  if(x %% 2 == 0){
    return("El número es par")
  } else {
    return("El número es impar")
  }
}

par_impar(2)


#### Ejercicio ##############################

##### Usando la base de datos *cars*, cree un vector que contenga los valores 
##### de la variable *speed*. Calcule la media y la desviación estándar sobre 
##### el vector (funciones `mean()` y `sd()`)

##### Crear una función que, si la entrada es un vector numérico, la salida 
##### sea la media, la desviación estándar, la mediana, el percentil 25 y 75. 
##### (Para la salida use una lista). 

#--------------------#
# Librerías          #
#--------------------#

# Los paquetes en R son colecciones de funciones, datos y documentación
# El objetivo es extender las capacidades básicas de R

# Instalar paquete
install.packages("tidyverse")

# Cargar paquete
library(tidyverse)

# Documentación del paquete
help("tidyverse")

# Evitar ambigüedad
dplyr::filter()

# Pedir ayuda sobre una función
?filter
help(filter)

# Evitar ambigüedad
?dplyr::filter
help("filter", package = "dplyr")

# Usar el buscador
??plot


##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
## FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ----- FIN DEL MÓDULO 1 ## 
##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
