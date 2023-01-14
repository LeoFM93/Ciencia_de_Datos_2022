##### SESIÓN 03 TIDYVERSE ####
# Importación de datos ####

library(rio)
copa_río <- import("Worldcups.csv")
class(copa_río)

#Archivo csv ####
install.packages("tidyverse")
library(tidyverse)# NO ENCUENTRABA EL PAQUETE TIDYVERSE CON LIBRARY PERO PUDE 
#INSTALARLO CON 'install.packages("tidyverse)'

str(copa_río) # Estructura
copas_río <- read.csv("Worldcups.csv") #read.csv es una función del r base, "read_csv" es del paquete readr
class(copas_río)
str(copas_río)
copas_río

#Archivo xlsx ####
library(readxl)
copas_xlsx<-read_xlsx("WorldCups.xlsx")
str(copas_xlsx)
class(copas_xlsx)

#Archivo sap ####
library(haven)
lapop_spss <- read_spss("sub_lapop.sav")
class(lapop_spss)
str(lapop_spss)

attributes(lapop_spss)
attributes(lapop_spss$interes)

#Archivos csv desde Github #### 
link01 <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCups.csv"
link01 # Link a través de ´raw´. Se obtiene de la dirección de la página a la que nos dirige la opción 'raw'
copas_GH_csv <- read_csv(link01)
str(copas_GH_csv)
class(copas_GH_csv)
copas_GH_csv

#Archivos sav desde Github ####
link02 <- "https://github.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/raw/main/BDs/sub_lapop.sav"
link02 # Link a través de ´download'. Se obtiene dando click derecho a 'download' y dando click en 'copiar enlace'
copas_GH_sav <- read_spss(link02)
str(copas_GH_sav)
class(copas_GH_sav)
copas_GH_sav

#Problema original: Funciones anidadas (nested functions)
##DATA: Copas del mundo ####
names(copas)

#Year: Year of the worldcoup
#Country: Country of the worldcoup
#Winner: Tema who wonthe worldcoup
#INCOMPLETO (REVISAR VIDEO)

##### FUNCIONES PRINCIPALES #####

# Función SELECT ####
copas <- import("WorldCups.csv")
select(.data = copas, Winner)
select(.data = copas, Winner, Country)
select(.data = copas, 2, 3)
# O con '|>' que tiene una mejor visibilidad. Primero el elemento y luego la acción
# de seleccionar una o más filas.
copas |> 
  select(Winner,Country)
copas |> 
  select(2,3)

#Función FILTER ####
filter(.data = copas, Country == "France")
# O con '|>' que tiene una mejor visibilidad. Primero el elemento y luego la acción
# de seleccionar una o más filas.
copas |> 
  filter(Country == "France")
copas |> 
  filter(Year > 1950)
copas |> 
  filter(Year > 1950 , Year < 1980) # funcionar como 'y'
copas |> 
  filter(GoalsScored > 20 & GoalsScored < 120) # funciona como 'y'
copas |> 
  filter(Year > 1950 | GoalsScored < 150) # funciona como 'o'

#Función ARRENGE ####
#Ordena las columnas de acuerdo a ciertos criterios.
#Los NA los deja al final

#Sin PIPE

#Con PIPE
copas |> 
  arrange(GoalsScored)
copas |> arrange (GoalsScored, MatchesPlayed)

## Función SUMMARISE ---- 
## Sirve para crear estadísticas resumen (con grupos o sin grupos)

### FUNCIONES VECTORIZADAS: Que se aplican a un VECTOR

# Tendencia central: mean(), median()
# Dispersión: sd(), IQR()
# Rango: min(), max(), quantile()
# Conteo: n(), n_distinct()

## SIN PIPE:
summarise(.data=copas, mean(GoalsScored))
summarise(.data=copas, min(GoalsScored))

## CON PIPE:
copas |> 
  summarise(min(GoalsScored))
copas |> 
  summarise(median(GoalsScored))
copas |> 
  summarise(sd(GoalsScored))
copas |> 
  summarise(min(GoalsScored))
copas |> 
  summarise(max(GoalsScored))
copas |> 
  summarise(maximo=max(GoalsScored), minimo=min(GoalsScored)) # Podemos asignar el nombre de las columnas en los términos
copas |> 
  summarise(n_distinct(Country))

## Función MUTATE ---- 
# Agrega nuevas variables (y preserva las que ya existen)

# SIN PIPE:
mutate(.data=copas, nueva_variable=1)

# CON PIPE:
mutacion <- copas |> mutate(antiguedad=2023-Year)
mutacion

## visualización de objeto ####
ej1<-copas |> mutate(antiguedad=2023-Year) # Calculamos la antiguedad como nueva variable
View(ej1)

ej1<- copas |>  mutate(goles_acumulados=cumsum(GoalsScored)) #Nueva variable: Acumulado de goles
View(ej1)

copas_new<-copas |>  mutate(numero_goles_categorica=case_when(GoalsScored<100~"Bajo", 
                                                              GoalsScored<150~"Medio", 
                                                              TRUE~"Alto"))
View(copas_new)
# https://dplyr.tidyverse.org/reference/case_when.html

## Función GROUP_BY -----
# Agrupa los datos de acuerdo a categorías. 
# OJO: Si se desea desagrupar colocar ungroup() como una nueva sentencia 

# sin pipe
group_by(.data=copas_new, numero_goles_categorica) # Establece grupos de acuerdo a los valores de una columna, pero no te genera un output.
summarise(group_by(.data=copas_new, numero_goles_categorica), mean(GoalsScored)) #Sobre el objeto agrupado aplica la función SUMMARISE

# CON PIPE
# Utilizando el pipe para evitar las funciones anidadas 
copas_new |> 
  group_by(numero_goles_categorica) |> 
  summarise(sum_goles=sum(GoalsScored))

copas_new |> 
  filter(Country=="France") |> 
  group_by(numero_goles_categorica) |> 
  summarise(promedio_goles=mean(GoalsScored)) |> 
  arrange(promedio_goles)

view(copas_new)
## Función COUNT ----
copas_new |> 
  count(GoalsScored)

## Función SLICE ----
copas_new |> 
  slice(1:10)

## Función TRANSMUTE ----
# Mientras MUTATE te arroja la table con todos sus valores agregándo una variable
copas |> 
  mutate(antiguedad=2023-Year) |> 
  view()
#TRANSMUTE arroja una tabla solo con la nueva variable
copas |> 
  transmute(antiguedad=2023-Year) |> 
  view()

## Función RENAME ----
copas |> 
  rename(goles_categorica=numero_goles_categorica) |>
  view()

## Función FILTER ----
copas |> filter(Country %in% c("France","Brazil"))

## Función ADD_ROW ----
copas |> 
  add_row()

## DATA RELACIONAL ----
#Una base de datos relacional es un tipo de base de datos que almacena

names(copas_new)
names(partidos)

#Mutating joins. Wich add new variables to one data frame from matching
### Función LEFT_JOIN
partidos_new |> 
  partidos |> 
  left_join(copas_new[,c(1,3)],by="Year")