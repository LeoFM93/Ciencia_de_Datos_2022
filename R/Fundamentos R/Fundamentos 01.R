#Fundamental de R

#Función Class: nos muestra la clase del objeto
class(5)
class("rojo")
class(TRUE)
class(NULL)
class(1+2i)

#Función as. : coerciona un objeto a otra clase
as.character(5)
as.numeric(TRUE)
as.integer(5.67)
as.factor("1","2")

#Asignamos objetos a nuestras variables
x <- 9.3
y <- "17.2"
z <- TRUE
class(x)
class(y)
class(z)

#Programar funciones
x <- 6
y <- 9
x^2+2*x*y+y^2

#Función concatenar c y length
a <- c(1,2,3,4,5)
a
length(a)

b <- c("arbol","planta",NA,"pasto")
b
length(b)

###### Matrices ################################################################
matrix(1:12)

#Crear una matriz a partir de vectores crados
v1 <- c(1,2,3)
v2 <- c(4,5,6)
cbind(v1,v2) #Unir vectores usando cada uno como una columna
rbind(v1,v2) #Unir vectores usando cada uno como una fila

#Crear una matriz con matrix
matrix(1:12)
matrix(c("Verde","Rojo","Azul","Amarilo","Marrón"))
matrix(1:28,nrow=7,ncol=4)
matrix(1:25,nrow=5,ncol=7)#el largo [25] no es un múltiplo del número de columnas [7]
matrix(c("Verde","Green","Rojo","Red","Blue","Azul","Yellow","Amarilo","Marrón","Brown"),nrow=2,ncol=5)

#Las operaciones aritméticas son victorizadas en una matriz
mi_matriz+1
mi_matriz*2
mi_matriz**2

#Transposición
mi_matriz
t(mi_matriz)

###### ARRAY ####################################################################
mi_array <- array(data=1:8,dim=c(2,2,2)) #Hazme un array con los números del 1 al 8 con las dimensiones 2 fils, 2 columnas y 2 capas.
mi_array
#Redes neuronales convolucionales -> Visión computacional

###### DATA FRAME ###############################################################
mi_df <- data.frame(
  "entero" = 1:4,
  "número" = c(1.2, 3.4, 4.5, 5.6),
  "cadena" = as.character(c("a","b","c","d")),
  "factor" = as.factor(c("1","2","3","4")))
mi_df
str(mi_df)

#Propiedades
dim(mi_df) #Dimensión
length(mi_df) #Largo (número de casos)
names(mi_df) #Nombre de las variables
colnames(mi_df)
rownames(mi_df)

#Coercionar una matriz a un data frame
mi_matriz
as.data.frame(mi_matriz)

##### ÍNDICES

#Los corchetes: Sirven para acceder a los elementos de un objeto

#Con arrays
mi_array <- array(data=1:8,dim=c(2,2,2)) #El "c" significa concatenar
mi_array
mi_array[, , 2] #pórque acá tenemos tres valores

#con matrices
v1
v1[2] #Una sola dimensión
mi_matriz[2,3] #filas,columnas

mi_df[,2]

