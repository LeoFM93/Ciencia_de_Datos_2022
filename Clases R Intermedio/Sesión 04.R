# SESIÓN 4: ANÁLISIS CLUSTER: MÉTODOS DE PARTICIÓN Y BASADOS EN DENSIDAD  -----

# Librerías necesarias

options(scipen = 999)    # Eliminar la notación científica
options(digits = 3) # Decimales

library(pacman)
p_load(rio, cluster, factoextra, tidyverse, ggrepel, scatterplot3d, 
       tictoc, NbClust, dbscan)

# MÉTODO DE PARTICIÓN -----

## EXPLICACIÓN DEL ALGORITMO K-means (Lloyd) ------

# Veamos una data ficticia

Caso <- c("A", "B", "C", "D", "E", "F","G")
Var1  <- c(1, 2, 3, 5, 5, 4, 3)
Var2  <- c(3, 1, 1, 4, 6, 6, 4)
data<- data.frame(Caso, Var1, Var2)
row.names(data) <- data$Caso
data$Caso<- NULL

# Grafiquemos la data en un scatterplot

data |> ggplot() + 
  aes(x=Var1, y= Var2, label=rownames(data)) +
  geom_point()+
  geom_text_repel()+ 
  xlim(0, 7) + ylim (0, 7)


### Paso 1: Selección del número de clústeres ------ 
# En este caso, se ha decidido que se utilizarán dos clusters
# Se pueden utilizar algoritmos para que nos recomiende el número de clústeres

# En este caso nuestro K=2

### Paso 2: Selección aleatoria de centroides iniciales ------ 
# Selección aleatoria de centroides iniciales: Se seleccionan aleatoriamente
# dos puntos del conjunto de datos, que se convertirán en los centroides
# iniciales de los dos clusters.

# En este caso seleccionaremos los:  centroide 1 (3, 6) y centroide 2 (5, 0)


### Paso 3: Cálculo de distancia ------ 

data |> 
  mutate(dist_eucl_C1= sqrt((Var1-3)**2 + (Var2 -6)**2),
         dist_eucl_C2= sqrt((Var1-5)**2 + (Var2 -0)**2)) -> data

data |> View()


### Paso 4: Asignación de puntos a clústers ------ 

data |> 
  mutate(cluster_asignado=case_when(dist_eucl_C1<dist_eucl_C2~1, 
                                    dist_eucl_C1>dist_eucl_C2~2)) -> data

data |> 
  ggplot()+
  aes(x=Var1, y= Var2, label=rownames(data), colour=as.factor(cluster_asignado)) +
  geom_point()+
  geom_text_repel()+ 
  xlim(0, 7) + ylim (0, 7)+
  scale_colour_manual(breaks = data$cluster_asignado, values = c("red", "blue"))

data |> View()

### Paso 5: Cálculo del centroide ------ 
# Se asignan los diez puntos al cluster cuyo centroide está más cercano al punto.
# Para calcular la distancia entre un punto y un centroide, 
# se utiliza la distancia euclidiana.

data |> 
  group_by(cluster_asignado) |> 
  summarise(mean(Var1), mean(Var2))


### Paso 6: Iteración del paso 3, 4 y 5 -----
# Calcular distancias (paso 3)+ asignación de clúster (paso 4)

data |> 
  mutate(dist_eucl_C1= sqrt((Var1-3.6)**2 + (Var2 -4.6)**2),
         dist_eucl_C2= sqrt((Var1-2.5)**2 + (Var2 -1)**2)) |> 
  mutate(cluster_asignado=case_when(dist_eucl_C1<dist_eucl_C2~1, 
                                    dist_eucl_C1>dist_eucl_C2~2)) |> 
  ggplot()+
  aes(x=Var1, y= Var2, label=rownames(data), colour=as.factor(cluster_asignado)) +
  geom_point()+
  geom_text_repel()+ 
  xlim(0, 7) + ylim (0, 7)+
  scale_colour_manual(breaks = data$cluster_asignado, values = c("red", "blue"))

# Recálculo de los centroides
data |> 
  mutate(dist_eucl_C1= sqrt((Var1-3.6)**2 + (Var2 -4.6)**2),
         dist_eucl_C2= sqrt((Var1-2.5)**2 + (Var2 -1)**2)) |> 
  mutate(cluster_asignado=case_when(dist_eucl_C1<dist_eucl_C2~1, 
                                    dist_eucl_C1>dist_eucl_C2~2)) |> 
  group_by(cluster_asignado) |> 
  summarise(mean(Var1), mean(Var2))


### Paso 7: Resultado final -----

# Al final del proceso, se obtienen dos clusters de puntos. 
# Los puntos dentro de cada clúster son similares entre sí en términos 
# de distancia euclidiana, mientras que los puntos de clusters diferentes 
# son significativamente diferentes en distancia euclidiana.


## EJEMPLO CON DATA CRIMINOLÓGICA ------

# Base de datos

USArrests
data<-USArrests[1:30,c(1,2,4)]
subdata<-as.data.frame(scale(data))

### Paso 0:
# Exploración de la data

### Paso 1: Selección del número de clústeres -----

#### Usando el criterio de Suma de Cuadrados -----

library(factoextra)

tic()
set.seed(2023)
fviz_nbclust(subdata, kmeans, method = "wss", k.max = 10) +
  #geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Método Elbow") + theme_bw()
toc()

# Tenemos que identificar en qué punto comienza a haber estabilidad
# en la suma de cuadrados total dentro de los clúster


#### Usando el gráfico de silueta -----

library(factoextra)

tic()
set.seed(123)
fviz_nbclust(subdata, kmeans, 
             method = "silhouette", k.max = 10) +
  labs(subtitle = "Silhouette method")
toc()


#### Usando el paquete NbClust -----

library(NbClust)
set.seed(2023)
res.nbclust <- NbClust(subdata, distance = "euclidean",
                       min.nc = 2, max.nc = 5, 
                       method = "average", index ="all") 


### Paso 2: Aplicación del K-means -----

set.seed(2023)
tic()
km <- kmeans(subdata, 
             centers = 2,     # Número de Cluster
             iter.max = 100,  # Número de iteraciones máxima
             nstart = 15,     # Número de puntos iniciales
             algorithm = "Lloyd")
toc()


# nstart = El argumento nstart de la función kmeans en R se utiliza 
# para especificar cuántas veces se debe ejecutar el algoritmo k-means
# con diferentes centroides iniciales. En otras palabras, nstart 
# es el número de veces que se inician los centroides aleatorios y 
# se ejecuta el algoritmo para encontrar los clusters.

# iter.max = El número máximo de interaciones de los pasos 3-5. 
# Esto previene un bucle infinito (poco usual)

# algorithm = Tenemos diferentes opciones: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"


### Paso 3: Explorando el cluster creado -----

#### General -----

print(km)

km$cluster

km$size
prop.table(km$size)

km$centers


#### Visualizando -----

library(factoextra)
fviz_cluster(km, data = subdata, ellipse.type = "convex") +
  theme_classic()

# Se podría usar dendograma????

### Paso 4: Validando el clúster creado ---------

#### Usando el índice de Silueta

km_clusters <- eclust(x = subdata, FUNcluster = "kmeans", 
                      k = 2, seed = 2023,
                      hc_metric = "euclidean",
                      graph = FALSE) 
# nstart = 50,
# FUNcluster = c("kmeans", "pam", "clara", "fanny", 
#                "hclust", "agnes", "diana")

km_clusters$centers

km_clusters$silinfo

fviz_silhouette(sil.obj = km_clusters, 
                print.summary = TRUE, 
                palette = "jco",
                ggtheme = theme_classic()) 


### Paso 5: Caracterizando el clúster -----

# Caracterizando los clúster con las variables 

data |> 
  mutate(cluster_kmeans=km$cluster) -> data.k


data.k|> 
  group_by(cluster_kmeans) |> 
  summarise(Murder=mean(Murder), 
            Assault=mean(Assault), 
            Rape=mean(Rape))


# MÉTODO DE BASADO EN DENSIDAD -----

# 1. Analizando la data multishapes ---------------------------
data(multishapes)
head(multishapes)

datos        <- multishapes[, 1:2]
cluster_real <- multishapes[3]
table(cluster_real)

plot(datos)

head(datos)


# 2. Realizando un cluster k-means ----------------------------
set.seed(2023)
km.res <- kmeans(x = datos, 
                 centers = 6, 
                 nstart = 25)
km.res

# Visualización de los clusters con k-means
fviz_cluster(km.res, 
             datos, 
             ellipse.type = "convex", 
             geom = "point")

# Sabemos que hay 6 grupos en los datos, pero se puede
# ver que el método k-means identifica incorrectamente
# los 6 grupos

# 3. Realizando un cluster jerárquico --------------------------------
distancias= daisy(datos, metric="gower")
aglomerativo = hcut(distancias, k = 2,hc_func='agnes',hc_method = "ward.D") 
fviz_cluster(object = list(data=datos, cluster = aglomerativo$cluster),
             geom = c("text"), 
             ellipse.type = "convex")


# 4. Realizando un DBSCAN -------------------------------------

# Selección del valor óptimo de epsilon.
# Como valor de minPts se emplea 5.

# La función kNNdistplot () en el paquete dbscan
# se puede usar para dibujar el diagrama de k-distancia
dbscan::kNNdist(datos, k = 5) 
# k	 number of nearest neighbors used for the distance
#    calculation.

dbscan::kNNdistplot(datos, k = 5)
# El gráfico muestra para k=5, el número de puntos y su
# distancia
# Hay pocos puntos a una menor distancia
# Hay muchos puntos a una mayor distancia
# Observamos un punto de quiebre o de inflexión en 0.15
abline(h = 0.15, lty = 2, col = "red")

# DBSCAN con epsilon = 0.15 y minPts = 5

# La función fpc::dbscan() proporciona un objeto de la clase 
# 'dbscan' que contiene los siguientes componentes
# CLUSTER: Membresía de clúster de codificación vectorial 
#          entera con observaciones de ruido codificadas como 0.
#  ISSEED: Vector lógico que indica si un punto es punto centro
#     EPS: Parámetro eps
#  MINPTS: Parámetro MinPts

# Usando el paquete fpc
set.seed(2023)
dbscan_cluster1 <- fpc::dbscan(data = datos, 
                               eps = 0.15, 
                               MinPts = 5)

# Resultados de la asignación
print(dbscan_cluster1)

## dbscan Pts = 1100 MinPts = 5 eps = 0.15

##         0   1   2   3  4  5
## border 31  24   1   5  7  1
## seed    0 386 404  99 92 50
## total  31 410 405 104 99 51

# Muestra una estadística del número de puntos que pertenecen a
# los clústers que son semillas o centrales (core point) y los
# puntos de borde.

# En la tabla anterior, los nombres de la columna son el 
# número de clúster. El grupo 0 corresponde a los valores
# atípicos (puntos negros en el diagrama DBSCAN)

# Usando el paquete dbscan
set.seed(2023)
dbscan::dbscan(datos, 0.15, 5)

# Visualización de los clusters con DBSCAN

# Primera forma de mostrar el gráfico con la función
# fviz_cluster del paquete factoextra
fviz_cluster(dbscan_cluster1, 
             datos, stand = FALSE, 
             ellipse = FALSE, 
             geom = "point") + 
  labs(title = "DBSCAN") + theme_bw()

# Segunda forma de mostrar el gráfico, solo ploteando
plot(dbscan_cluster1, 
     datos, 
     main = "DBSCAN", 
     frame = FALSE)

# Se puede ver que DBSCAN funciona mejor para estos conjuntos
# de datos y puede identificar los clústeres "correctos" en 
# comparación con el algoritmo k-means y jerárquico.

# Probando diferentes valores de los parámetros del DBSCAN
# 1. Usando eps = 0.15 y MinPts = 5
dbscan_cluster1 <- fpc::dbscan(data = datos, eps = 0.15, 
                               MinPts = 5)
print(dbscan_cluster1)
fviz_cluster(dbscan_cluster1, datos, stand = FALSE, 
             ellipse = FALSE, geom = "point") + theme_bw()

# 2. Usando eps = 0.01 y MinPts = 5
dbscan_cluster2 <- fpc::dbscan(data = datos, eps = 0.01,
                               MinPts = 5)
print(dbscan_cluster2)
fviz_cluster(dbscan_cluster2, datos, stand = FALSE, 
             ellipse = FALSE, geom = "point") + theme_bw()


# 3. Usando eps = 0.30 y MinPts = 5
dbscan_cluster3 <- fpc::dbscan(data = datos, eps = 0.30,
                               MinPts = 5)
print(dbscan_cluster3)
fviz_cluster(dbscan_cluster3, datos,stand = FALSE, 
             ellipse = FALSE, geom = "point") + theme_bw()

# 4. Usando eps = 0.80 y MinPts = 5
dbscan_cluster4 <- fpc::dbscan(data = datos, eps = 0.80,
                               MinPts = 5)
print(dbscan_cluster4)
fviz_cluster(dbscan_cluster4, datos,stand = FALSE, 
             ellipse = FALSE, geom = "point") + theme_bw()

# SI DESEA VISUALIZAR CÓMO FUNCIONAR EL ALGORITMO PASO A PASO VISITA LA PÁGINA:
#https://www.naftaliharris.com/blog/visualizing-dbscan-clustering/