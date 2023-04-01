# SESIÓN 2: ANÁLISIS FACTORIAL EXPLORATORIO -----

# Librerías necesarias

library(psych)
library(tidyverse)
library(ggrepel)
library(factoextra)

# EJERCICIO: ACP en un estudio criminológico ------

## PASO 0: Data y subdata---- 
library(readxl)
data<-read_xlsx("All_data_FIW_2013-2022.xlsx")
View(data)
# El Índice de Freedom House mide la libertad política y civil en los países de todo el mundo.
#  

# https://freedomhouse.org/sites/default/files/2021-02/Freedom_in_the_World_2020_Methodology.pdf
# Crear una subdata con las variables numéricas que se van a utilizar en el ACP
# A: Electoral Process subcategory 
# B: Political Pluralism and Participation subcategory
# C: Functioning of Government subcategory
# D: Freedom of Expression and Belief subcategory
# E: Associational and Organizational Rights subcategory
# F: Rule of Law subcategory
# G: Personal Autonomy and Individual Rights subcategory

data<-data |> filter(Edition==2020) |> select(1,2, A,B,C, D, E, F, G)
#colnames(data)= c("pais","region","electoral_process", "political_pluralism_and_participation",
#                  "functioning_of_government", "freedom_of_expression_and_belief",
#                  "associational_and_organization_rights", "rule_of_law", "personal_autonomy_individual_rights")

colnames(data)= c("pais","region","elec_pro", "pol_plural",
                  "funct_gov", "expression_and_belief",
                  "assoc_org_rights", "rule_of_law", "personal_autonomy")

View(data)

subdata<- data |> select(-1, -2)
View(subdata)

## PASO 1: Análisis exploratorio de datos -----

### 1.1. Summary y análisis visual -----

summary(subdata)

### 1.2. Matriz de correlación ------

## Analizar las relaciones entre las variables (Matriz de correlaciones)
## Para aplicar un PCA, se necesita que los atributos estén correlacionados. 
matrixcor<-cor(subdata)
matrixcor

## Y luego podemos graficarlo
cor.plot(matrixcor)


## PASO 2: Verificación de supuestos sobre la matrixcor -----

### 2.1. Kaiser-Meyer-Olkin (KMO) -----
### Mide el nivel de correlación global en nuestra data. 
### KMO devuelve valores entre 0 y 1. Queremos que salga lo más cercano al 1.
### Recomendable a partir de 0.6. Ojo, referencial. 

psych::KMO(subdata)

## PASO 2: Verificación de supuestos sobre la matrixcor -----

### 2.1. Kaiser-Meyer-Olkin (KMO) -----
### Midel el nivel de correlación global en nuestra data. 
### KMO devuelve valores entre 0 y 1. Queremos que salga lo más cercano al 1.
### Recomendable a partir de 0.6. Ojo, referencial. 

psych::KMO(subdata) 

### 2.2 Prueba de Esfericidad de Bartlett ----
### La matriz de correlaciones NO debe ser una matriz de identidad.
### Esto quiere decir que tendrá 1 en la diagonal y todo lo demás será 0, 
### lo que indicaría que no hay asociación entre las variables. 
cortest.bartlett(matrixcor,n=dim(subdata)[1])
# H0: Es una matriz de identidad (Las variables analizadas NO están correlacionadas en la muestra).
# H1: No es una matriz de identidad (Las variables analizadas SÍ están correlacionadas en la muestra).

## PASO 3: Análisis factorial exploratorio -----

### 3.1 Determinar el número de factores ----
### Podemos utilizar el gráfico de sedimentación. 
### Normalmente el criterio es que el eigenvalue sea mayor a 1. 
### Buscamos el punto de quiebre (codo) en el gráfico.

fa.parallel(subdata,fa="fa") 

### 3.2 Cálculo de los factores ----
factorial <- fa(subdata,nfactors = 2,cor = 'mixed',rotate = "varimax",fm="minres")