
# PRACTICA CALIFICADA N°1 ####


## EJERCICIO N°1 ----

### a) Busque y presente una base de datos que tenga las siguientes características. ----

#### Descripción:
#### Se ha seleccionado una base de datos sobre Enfermedades del Corazón de la 
#### plataforma 'Kaggle'con las siguientes variables:
#### GENDER: Género de la persona (male, female)
#### AGE: Edad de la persona (33 - 69 años)
#### EDUCATION: Nivel educativo alcanzado por la persona ("postgraduate","primary school","uneducated","graduate")
#### CURRENTSMOKER: Estatus de fumador de la personas (No=0, Sí=1)
#### CIGSPERDAY: Número de cigarros por día que fuma la persona (0 - 70)
#### BPMEDS: Consumo de Medicamentos Antihipertensivos (No=0, Sí=1)
#### PREVALENTSTROKE: Existencia de Antecedentes de Accidentes Cerebrovascular o ACV (No, Yes)
#### PREVALENTHYP: Padecimiento de Hipertensión Arterial (No=0, Sí=1)
#### DIABETES: Padecimiento de Diabetes (No=0, Sí=1)
#### TOTCHOL: Nivel total de colesterol en la sangre (107 - 696)
#### SYSBP: Presión arterial sistólica (83.5 - 295.0)
#### DIABP: Presión arterial diastólica (48.0 - 142.5)
#### BMI: Indice de Masa Corporal (15.54 - 56.80)
#### HEARTRATE: Frecuencia Cardiaca del paciente (44.0 - 143.0)
#### GLUCOSE: Nivel de glucosa en la sangre (40.0 - 394.0)
#### HEART_STROKE: Ocurrencia de un Ataque Cardiaco (No, Yes)

#### Con esta base de datos se intentará explorar los componentes principales
#### que expliquen en mayor proporción el comportamiento de la variable 'Ataque
#### al Corazón' ('Heart_ stroke').


#### Cargamos los paquetes y las librería que usaremos:
library(readr) # lector de archivos separado por comas
library(naniar) # Conjunto de herramientas para trabajar con valores perdidos
library(tidyr) # Conjunto de herramientas para ordenar, limpiar o restructurar bases de datos para su análisis y visualización
library(BBmisc) # Usaremos la función 'normalize()' para la selección de los componentes principales
library(psych) # Usaremos la función 'cor.plot()' para graficar la matriz de correlaciones
library(factoextra) # Usaremos la función de 'fviz_eig()' para extraer y visualizar la variancia de las dimensiones
library(dplyr) # Usaremos la función 'mutate()' para agregar las columnas de nuestros 02 componentes principales
library(cluster) # Utilizaremos la función 'daisy()' para generar la matriz de disimilitud para el análisis de cluster.
library(corrplot) # utilizaremos la función 'corrplot()' para visualizar el triángulo superior de la matriz de distancias
library(dbscan) # Utilizaremos el algoritmo DBSCAN para realizar clausterización acumulada.
library(fpc) # Utilizamos la función plotcluster para graficar los resultados de la clusterización con DBSCAN

#### Cargamos nuestro base de datos, la asignamos a nuestro objeto y generamos una visualización.
heart_disease_00 <- read_csv("heart_disease.csv")
View(heart_disease_00)

#### Exploramos nuestra base de datos.
summary(heart_disease_00) # Exploro las principales características de las variables de mi base de datos.
str(heart_disease_00) # Exploro la estructura de la base de datos y las dimensiones de mis variables.
pct_miss(heart_disease_00) # Exploro la magnitud de los casos perdidos en mi base de datos.
miss_var_summary(heart_disease_00) # Exploro en qué variables se concentran los valores perdidos para la selección de columnas más adelante.

#### Seleccionamos una muestra de 1,000 casos y generamos una visualización.
heart_disease_01 <- heart_disease_00[sample(nrow(heart_disease_00), 100), ]

#### Seleccionamos nuestras variables y asignamos como data frame al nuevo objeto.
heart_disease_02 <- as.data.frame(heart_disease_01[, c("age",
                                                       "cigsPerDay",
                                                       "totChol",
                                                       "sysBP",
                                                       "diaBP",
                                                       "BMI",
                                                       "heartRate",
                                                       "glucose")])
View(heart_disease_02)

#### Exploramos nuestra sub base de datos.
summary(heart_disease_02)
str(heart_disease_02)
pct_miss(heart_disease_02)
miss_var_summary(heart_disease_02)

####  y reemplazamos los valores perdidos por la media de cada variable.
heart_disease_03 <- replace_na(heart_disease_02, 
                               as.list(colMeans(heart_disease_02, 
                                                na.rm = TRUE)))
View(heart_disease_03)

#### Exploramos nuestra sub base de datos.
summary(heart_disease_03)
str(heart_disease_03)
pct_miss(heart_disease_03)
miss_var_summary(heart_disease_03)




### b) Aplique Análisis de Componentes Principales a la base de datos. ----

#### Analizamos la relación entre las variables
matrixcor <- cor(heart_disease_03)
cor.plot(matrixcor)
psych::KMO(heart_disease_03)
cortest.bartlett(matrixcor,
                 n=dim(heart_disease_03)[1])

#### Aunque la matriz de correlación no nos arroja muchos valores altos de correlación 
#### entre nuestras variables (exceptuando quizás la correlación entre 'diaBP' o 
#### 'sysBP'); el coeficiente de Kaiser-Meyer-Olkin (KMO) nos arroja valores por
#### encima de 0.5 y la Prueba de Esfericidad de Bartlett nos arroja un valor 'p' 
#### extremedamente bajo, lo que nos indica que nuestra base de datos es adecuada
#### para ACP.


#### Determinamos el número de componentes con el gráfico de sedimentación 
#### y el análisis paralelo

scree(heart_disease_03, 
      pc=TRUE, 
      factors=FALSE)

fa.parallel(heart_disease_03,
            fa="pc")

#### En el primer caso podemos observar que hay 03 componentes cuyos valores eigen
#### son mayores a 01, sin embargo, el análisis paralelo sugiere el número ideal
#### de componentes sea 01.

#### Calculamos los componentes principales, lo asignamos a nuestro objeto y 
#### exploramos la nueva base de datos.

pc <- prcomp(x=heart_disease_03, 
             scale=TRUE, 
             center=TRUE)
summary(pc)
fviz_eig(pc)
cor.plot(pc$rotation)

#### En el retorno de la función 'summary()' como en el gráfico de barras generado
#### con 'fviz_eig()' nos muestra que los 02 componentes principales explican 
#### separadamente el 33.37% y el 15.35%; lo que significa que juntas explican
#### menos de la mitad de la varianza de los casos. 

#### Aunque no existe un umbral determinado para seleccionar componentes principales,
#### se recomiendaría ampliar la selección a 03 componentes para superar el 50% de
#### la variabilidad. Quizás evaluar la posibilidad de un 04 componente, teniendo
#### en cuenta los riesgos de utilizar demasiados componentes: ajustar el modelo al
#### ruido en lugar de a patrones subyacentes, y incrementa la complejidad del
#### modelo dificultando su interpretación.

#### En el retorno de la función 'cor.plot()' podemos observar que las variables
#### que más asociadas están al PC1 son la presión sistólica ('sysBP'), la presión
#### diastólica ('diaBP'), eñ índice de masa corporal ('BMI'), y la edad ('age'), 
#### superando cada una una correlación del 40% (52%, 48%, 44%, y 41% respectivamente). 

#### Por su lado, el PC2 se ve asociada de manera inversa o negativa con las 
#### variables de ritmo cardiaco ('heartRate') y glucosa ('glucose'), con 67% y
#### 55% respectivamente.

#### Observando el PC3, vemos que ve asociada de manera inversa con la variable de
#### cigarros por día ('cigsPerDay') y directa con la variable de glucosa ('glucose'),
#### con 67% y 63% respectivamente.




### c) Presente una descripción del resultado, teniendo en cuenta que deseamos ----
### obtener los dos primeros componente. 

#### Seleccionamos nuestros 02 componentes principales

heart_disease_04 <- heart_disease_03 |> mutate(pc1=pc$x[,1], pc2=pc$x[,2])

heart_disease_04$pc1_norm = normalize(heart_disease_04$pc1, 
                                      method = "range", 
                                      margin=2, # by column
                                      range = c(0, 100))

heart_disease_04$pc2_norm = normalize(heart_disease_04$pc2, 
                                      method = "range", 
                                      margin=2, # by column
                                      range = c(0, 100))

View(heart_disease_04)




### d) Muestre una visualización de los casos en dos dimensiones. ----

#### Visualización

fviz_pca_ind(pc)
fviz_pca_var(pc)
biplot(pc)

#### 1. Del primer gráfico podemos observar poca concentración de los casos en 
#### el plano que se genera del cruce de los 02 componentes principales. Los casos
#### se dispersan en el plano sin obvias concentraciones. Aunque se parece que se
#### aprecia una menor densidad de casos en el tercer cuadrante, así como también 
#### menor densidad en la mitad del plano debajo del eje del PC1.

#### 2. Del segundo gráfico, de acuerdo a la dirección de las flechas y el ángulo
#### entre estas, podemos observar una fuerte correlación entre algunas variables.
#### Por un lado tenemos variables como la presión sanguínea sistólica ('sysBP'), 
####  y la edad ('age'); por otro lado, tenemos el indice de masa corporal ('BMI')y 
#### el total de colesteral en al sangre ('totChol'. Entre estos dos pares, muy 
#### pegado al plano vemos solo la presión sanguínea diastólica ('diaBP').

#### En la dirección contraria, tenemos a cigarros por día ('cigsPerDay'), única
#### flecha en nuestro tercer cuadrante. Y casi perpendicularmente hacia abajo y
#### apegadas, tenemos glucosa ('glucose') y ritmo cardíaco ('heartRate').

#### Mirando la extensión de las flechas y la sombra que proyectan en
#### las dimensiones, vemos que algunas variables como presión sanguínea sistólica
#### ('sysBP'), diastólica('diaBP') y edad ('age'), el índice de masa corporal ('BMI')
#### y el total de glucosa en la sangre ('totChol'), tienen una alta contribución
#### a la primera dimensión; mientras que las variables de cigarros por día
#### ('cigsPerDay'), ritmo cardíaco ('heartRate'), y glucosa ('glucose') a la 
#### segunda dimensión.

#### 3. Del tercer gráfico podemos observar que una gran proporción de los casos se
#### encuentran la mitad derecha superior que se aprecia cuado trazamos una diagonal
#### positiva pasando por el punto de origen del plano (0,0). Esto nos indica que 
#### que en esta área se concentran los casos que no están influidas significativamente
#### por nuestras variables, por lo que no explican su variablidad.





## EJERCICIO N°2 ----

### a) Explore la data. ¿Previamente puede identificar algún número de clústers ----
### potenciales? Confirme ello a través de la aplicación del algún algoritmo de 
### recomendación de número de clúster. 

#### Como se comentó en el ejercicio N°1, la exploración visual no revelaba
#### la existencia de concentraciones altas o evidentes, de manera general los 
#### casos se distribuyen con similar dispersión, aunque existe menor densidad
#### en el tercer cuadrante, y demanera general debajo del eje del PC1.

#### Estandarizamos nuestra base de datos y generamos una visualización
heart_disease_05 <- scale(heart_disease_04)
View(heart_disease_05)

#### Generamos nuestra Matriz de Disimilitud, escogemos nuestro tipo de distancia,
#### en este caso la euclidiana, y redondeamos los valores con 01 decimal.
heart_disease_06 <- daisy(heart_disease_05,
                          metric= "euclidean")

round(heart_disease_06, 1)
     
#### Como se trata de demasiados casos, la visualización es bastante saturada. Se puede
#### Se puede utilizar la siguiente variación si se desea visibilizar una muestra
#### del ouput podemos utilizar la función 'sample()'

#### Utilizamos 'fviz_dist()'para generar una visualización de la Matris de Distancias.

fviz_dist(heart_disease_06,
          gradient = list(low = "blue",
                          high = "#FC4E07")) 

#### Incluso habiendo reducido la muestra de 1,000 casos a solo 100, la visualización 
#### es bastante saturada, sin embargo podemos observar sutiles cuadrículas, 
#### trazadas por líneas rojas que estarían expresando poca similitud entre distintas 
#### observaciones con cierta regularidad lo que genera los patrones cuadriculares.
#### Por otro lado,observamos también una línea azul diagonal y ascendente hacia 
#### la derecha, cuya forma perfecta de diagonal y ascendente estaría expresando
#### la correspondencia de los casos con sigo mismo.

#### Finalmente visualizamos solo el triángulo superior de nuestra matriz de distancias:

corrplot(as.matrix(heart_disease_06), 
         is.corr = FALSE, 
         method = "color",
         order="hclust",
         type = "upper")

### b) Aplique al menos dos tipos de algoritmos de clasificación. Puede elegir ----
### entre jerárquico (AGNES, DIANA), partición (K-means) o basados en densidad 
### (DBSCAN). Tome de referencia la recomendación obtenida en el punto anterior.

#### Aplicando el algoritmo AGNES:

heart_disease_agnes = hcut(heart_disease_06, k = 2,hc_func='agnes',hc_method = "single")
plot(heart_disease_agnes)

#### Lo que notamos en esta representación es que obtenemos 02 clusters a partir
#### de una altura de 8 puntos.


#### Aplicando el algoritmo DBSCAN:

#### Primero Graficamos un scaterplot:
heart_disease_07 <- as.data.frame(as.matrix(heart_disease_05))

heart_disease_07 |> ggplot() + 
  aes(x=pc1_norm, y=pc2_norm, label = rownames(heart_disease_07)) +
  geom_point()+
  geom_text_repel()+ 
  xlim(-3, 3) + ylim (-7, 7) # Escogemos nuestros rangos utilizando 'range(as.data.frame(heart_disease_05)$pc2_norm)'

#### Ahora asignamos nuestros puntos clusters, en este ejercicio seleccionamos
#### intuitivamente los casos 85 y 71. Averiguamos sus coordenadas 
heart_disease_07[85, c("pc1_norm", "pc2_norm")] # (-1.47,0.606)
heart_disease_07[71, c("pc1_norm", "pc2_norm")] # (0.0494,0.593)

#### Ahora calculamos las distancias entre nuestros componentes.
heart_disease_07 |> 
  mutate(dist_eucl_C1= sqrt((pc1_norm-(-1.47))**2 + (pc2_norm-0.606)**2),
         dist_eucl_C2= sqrt((pc1_norm-0.0494)**2 + (pc2_norm-0.593)**2)) -> heart_disease_08

heart_disease_08 |> 
  mutate(cluster_asignado=case_when(dist_eucl_C1<dist_eucl_C2~1,
                                    dist_eucl_C1>dist_eucl_C2~2)) -> heart_disease_09
                    
heart_disease_09 |> 
  ggplot()+
  aes(x=pc1_norm, y= pc2_norm, label=rownames(heart_disease_09), colour=as.factor(cluster_asignado)) +
  geom_point()+
  geom_text_repel()+ 
  xlim(-3, 3) + ylim (-7, 7)+
  scale_colour_manual(breaks = data$cluster_asignado, values = c("red", "blue"))

#Con esta primera selección de puntos hemos obtenido dos clusters.

### c) Compare los resultados de los dos métodos utilizados. ¿Cuál agrupó de ----
### mejor manera? 





### d) Presente una caracterización sencilla de cada grupo creado a partir de ----
### las variables originales.




