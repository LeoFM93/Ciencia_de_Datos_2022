# SESIÓN 1: ANÁLISIS DE COMPONENTES PRINCIPALES -----

# Librerías necesarias

install.packages("psych")
install.packages("factoextra")
install.packages("jpeg")

library(psych)
library(tidyverse)
library(ggrepel)
library(factoextra)

# EJERCICIO 1: ACP en un estudio criminológico ------

## PASO 0: Data y subdata---- 
?USArrests
data<-USArrests

# Crear una subdata con las variables numéricas que se van a utilizar en el ACP
subdata<-USArrests |> select(1,2,4)
View(subdata)
summary(subdata)

## PASO 1: Análisis exploratorio de datos -----

### 1.1. Summary y análisis visual -----

subdata |> ggplot(aes(x=Murder, y=Assault)) + geom_point()
subdata |> ggplot(aes(x=Murder, y=Rape)) + geom_point()
subdata |> ggplot(aes(x=Rape, y=Assault)) + geom_point()

### 1.2. Matriz de correlación ------

## Analizar las relaciones entre las variables (Matriz de correlaciones)
## Para aplicar un PCA, se necesita que los atributos estén correlacionados. 
matrixcor<-cor(subdata)
matrixcor

cor.plot(matrixcor)

## PASO 2: Verificación de supuestos sobre la matrixcor -----

### 2.1. Kaiser-Meyer-Olkin (KMO) -----
### Midel el nivel de correlación global en nuestra data. 
### KMO devuelve valores entre 0 y 1. Queremos que salga lo más cercano al 1.
### Recomendable a partir de 0.6. Ojo, referencial. 

psych::KMO(subdata)

### 2.2 Prueba de Esfericidad de Bartlett ----
cortest.bartlett(matrixcor,n=dim(subdata)[1])

## PASO 3: PCA -----

### 3.1 Determinar el número de componentes ----
### Podemos utilizar el gráfico de sedimentación. 
### Normalmente el criterio es que el eigenvalue sea mayor a 1. 
### Buscamos el punto de quiebre (codo) en el gráfico.

scree(subdata, pc=TRUE, factors=FALSE)

### O también el análisis paralelo (utiliza simulaciones con bootstrap). 
### En este caso explícitamente nos recomienda un número de componentes.

fa.parallel(subdata,fa="pc")

### 3.2 Cálculo de los componentes principales ----
pc <- prcomp(x=subdata,scale=TRUE, center=TRUE)

## PASO 4: Interpretación -----

# Siempre se obtienen tantos componentes como variables.

### 4.1 Variabilidad explicada ----
### % de varianza explicada
summary(pc)
fviz_eig(pc)

### 4.2 Loadings o cargas de cada PC ----
pc$rotation

### 4.3 Visualización ----

fviz_pca_ind(pc)
fviz_pca_var(pc)
biplot(pc)

## PASO 5: Utilización del PCA -----

### 5.1 Creación de variable ----
### Se extrae el número de componentes que se desea. En este caso, sólo 1.
nueva_data<- data |> mutate(pc1=pc$x[,1])
View(nueva_data)
library(BBmisc)
nueva_data$pc1_norm = normalize(nueva_data$pc1, 
                                method = "range", 
                                margin=2, # by column
                                range = c(0, 100))

### 5.2 Análisis descriptivo ----

### Cuál es el top10 de Estados según cada variable?
nueva_data |> 
  select(1) |> 
  arrange(desc(Murder)) |> 
  head(10)

nueva_data |> 
  select(2) |> 
  arrange(desc(Assault)) |> 
  head(10)

nueva_data |> 
  select(4) |> 
  arrange(desc(Rape)) |> 
  head(10)

### Cuál es el top10 de Estados con menor seguridad (más inseguros) según el componente (normalizado) creado?
### Qué similitudes hay?
nueva_data |> 
  select(1,2,4,6) |> 
  arrange(pc1_norm) |> 
  head(10)



# EJERCICIO 2: ACP para reducir tamaño de imágenes ------

## Abrir imagen -----
library(jpeg) #leer y escribir archivos jpeg

# Convertir la imagen en un arreglo de 3 dimensiones (array!)

gato <- readJPEG('gato.jpg') 
dim(gato)
ncol(gato)
nrow(gato)

## Crear los componentes por cada capa -----

r <- gato[,,1]
g <- gato[,,2]
b <- gato[,,3]

# Extraer las componentes principales
gato.r.pca <- prcomp(r, center = FALSE)
gato.g.pca <- prcomp(g, center = FALSE)
gato.b.pca <- prcomp(b, center = FALSE)

## Recolectar los componentes -----

rgb.pca <- list(gato.r.pca, gato.g.pca, gato.b.pca)

## Generación de imágenes comprimidas -----

# Guardar la imagen comprimida considerando distinto n?mero de componentes
for (i in seq.int(3, round(nrow(gato) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('comprimido/gato_comprimido_', round(i,0), '_componentes.jpg', sep = ''))
}

## Analizar la diferencia del tamaño y el ratio de compresión -----

original <- file.info('gato.jpg')$size / 1000
imgs <- dir('comprimido/')

for (i in imgs) {
  full.path <- paste('comprimido/', i, sep='')
  print(paste(i, ' tamaño: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}