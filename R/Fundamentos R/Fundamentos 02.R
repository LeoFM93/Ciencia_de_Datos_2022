#Doble corchete con Arrays

df1 <- data.frame(peso=c(45,36,78,95,38,63), 
                  tipo=c("a","b","c","d","e","f"), 
                  seleccion= c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE))
df1
df1$peso # Accedemos al elemento "peso" en forma de lista
df1[2]
df1[[2]]
class(df1[2])
class(df1[[2]])

#y las listas
lista <- list(letters[1:6], matrix(letters[1:9],3,3), df1) #creamos la lista
#En esta caso se trata de una lista compuesta por 03 elementos: una lista, una
#matriz y un data frame.
lista
names(lista) <- c("letra","mat","dfr") #le damos nombres a cada elemento

lista[1] #Se imprime el primer elemento
lista[[1]] #No se imprimen los nombres asignados por 'names'
lista["letra"] #Imprime el elemento cuyo nombre coincida con el str entre corchetes.
lista$letra

###### 0.2. FUNCIONES ###########################################################
# nombre_funcion <- function(arg1, arg2, ...){
# OBJETO <- CÓDIGO A SER EJECUTADO
# RETORNAR(OBJETO)
# }

#Ejemplo 01
#Creamos la función resta_2_numeros
resta_2_numeros <- function(n1,n2){
 y <- n1-n2
 return(y)
}

resta_2_numeros(n1=45, n2=15)
resta_2_numeros(45,15)

#Podemos mejorarla
n1 <- 74
n2 <- "a"
resta_2_numeros <- function(n1,n2){
  if(!is.numeric(n1)|!is.numeric(n2)){ #el "!" significa negación, se lee "el número 1 no es numérico"
    warning("Al menos uno de los argumentos no es numérico")
  }
  if(is.numeric(n1)|is.numeric(n2)){
    y <- n1 - n2
    return(y)
  }
}
resta_2_numeros(n1,n2))

#Creamos función es_par en Grupos
es_par <- function(arg1){
  if(arg1 %% 2==0){
    message("El número es par")
  }
  else{
    message("El número es impar")
  }
}

es_par(5)
es_par(4)

#Solución
es_par <- function(arg1){
  if(arg1 %% 2==0){
    x <- "El número es par"
  }
  else{
    x <- "El número es impar"
  }
  return(x)
}

es_par(5),
es_par(4),

###### 0.3 FUNCIONES DEL R ######################################################

seq(),
seq(from = 10, to = 100, by = 10),

v1 = c(35,2,65,156,6)
median(v1)

###### 0.4 TIDYVERSE ############################################################