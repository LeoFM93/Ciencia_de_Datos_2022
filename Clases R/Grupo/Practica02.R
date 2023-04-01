
#### PRACTICA CALIFICADA N° 02 ####

#Habilitamos los paquetes necesarios para correr las funciones
library(readxl)
library(readr)
library(dplyr)
library(timechange)
library(lubridate)
library(ggplot2)
library(gganimate)
library(haven)
library(tidyverse)
library(pacman)
p_load("VIM","DEoptimR","minqa","nloptr","simputation", "mice", "tidyverse", "DMwR2", "naniar")



#### Pregunta 01 ####


# Primera parte ----

#Cargamos la BBDD de un archivo excel y la describimos
casos_covid <- read_csv("positivos_covid.csv")
View(casos_covid)


#Filtramos la data por los departamentos que se busca analizar
casos_covid <- casos_covid |> 
  filter(DEPARTAMENTO=="AMAZONAS"|DEPARTAMENTO=="APURIMAC"|DEPARTAMENTO=="AYACUCHO"|DEPARTAMENTO=="HUANCAVELICA"|DEPARTAMENTO=="PASCO"|DEPARTAMENTO=="TACNA")

#Crear variable fecha con formato ymd
casos_covid <- casos_covid|> 
  mutate(fecha = ymd(FECHA_RESULTADO))

# Crear year_start para el año de inicio de la pandemia (2020)
year_start <- 2020

# Crear month_start para el mes de inicio de la pandemia (3 para marzo)
month_start <- 3

# Crear variable para el número de mes desde el inicio de la pandemia
n_meses <- 12 * (year(casos_covid$fecha) - year_start) + 
  (month(casos_covid$fecha) - month_start) + 1

# Integrar la variable n_meses en la bd principal
casos_covid <- casos_covid |>
  mutate(n_meses=n_meses)

# Creamos un df nuevo con solo las variables de n_meses y casos
cc_mes<- casos_covid |>
  filter(n_meses >= 1, n_meses <= 21) |>
  count(n_meses)

# Creamos el grafico animado y lo guardamos como GIF
cc_mes |>
  ggplot(aes(x = n_meses, y = n, color = n_meses)) +
  geom_line() +
  ggtitle("Evolución del número total de casos positivos por mes") +
  xlab("Mes") +
  ylab("Casos positivos") +
  transition_reveal(n_meses) 

animate(cc_mes, duration = 5, fps = 20, width = 200, renderer = gifski_renderer)
anim_save("gif_p1A.gif")

#Podemos ver que entre las regiones |>  Amazonas, Apurímac, Tacna, Ayacucho, Huancavelica,
#y Pasco se tuvo el pico para el mes 6 (Ago-20), reduciéndose desde ahí los casos
#hasta el mes 10 (Dic-20) y elevándose constantemente hasta entre los meses 13 (Mar-21)
#y 14 (Abr-21), para finalmente tener una reducción de casos hasta el mes 20 (Oct-21)
#y empezando un nuevo incremento de casos en el mes 21 (Nov-21)
#La etapa más grave de los primeros 21 meses de pandemia se presentó en Ago-20, llegando
#a tener alrededor de 23 mil casos positivos en el mes. Asimismo, podemos apreciar
#en el gráfico la 1ra y 2da ola de la pandemia, así como el inicio de la 3ra ola (Nov-21)



# Segunda parte ----

# Crear la variable Macroregion a partir de las regiones seleccionadas.
casos_covid <- casos_covid |>
  mutate(Macroregion=case_when(DEPARTAMENTO=="AMAZONAS"~"Selva",
                               DEPARTAMENTO=="APURIMAC"~"Sur", DEPARTAMENTO=="TACNA"~"Sur",
                               DEPARTAMENTO=="AYACUCHO"~"Centro", DEPARTAMENTO=="HUANCAVELICA"~"Centro",
                               DEPARTAMENTO=="PASCO"~"Centro"))

# Crear un df nuevo con solo las variables de Macroregion, n_meses y casos
# agrupado por Macroregion
cc_mes2<- casos_covid |>
  group_by(Macroregion) |>
  filter(n_meses >= 1, n_meses <= 21) |>
  count(n_meses)

# Crear la variable media_movil para 3 meses calculandola desde el mes junto a los
# dos meses anteriores para que la media movil se presente hasta el mes 21
cc_mes2$media_movil <-
  zoo::rollmean(cc_mes2$n, k = 3, align = "right", na.pad = TRUE)

# Creamos el grafico animado con línea spara cada macroregion, y 
# lo guardamos como GIF
ggplot(cc_mes2, aes(x = n_meses, y = media_movil, color = Macroregion)) +
  geom_line() +
  ggtitle("Evolución del número total de casos positivos por mes a nivel de Macroregion") +
  xlab("Mes") +
  ylab("Casos positivos") +
  transition_reveal(n_meses) +
  anim_save("gif_p1B.gif")

#En el gráfico 2 distinguiendo los primeros 21 meses de pandemia por Macroregion observamos
#también las dos primeras olas de pandemia, aunque el inicio de la 3ra ola solo se
#presenta con claridad en la Macroregion Sur.
#Asimismo, hay diferencias en los picos observados ya que estos se presentan posteriores
#a los que se veían en el gráfico 1, siendo el 1er pico en el mes 7 (Set-20) y
#el 2do pico en el mes 15 (May-21).




#### Pregunta 02 ####


## Primera parte ----

# Haga un diagnóstico de los valores perdidos de este subset. ¿Qué tan grave es
# el problema? 
  
# Importamos nuestra base de datos y creamos el objeto 'lapop'. 
lapop <- read_dta("Ciencia de Datos/Ciencia_de_Datos_2022/Clases R/Grupo/PER_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

# Creamos el subset 'sublapop' con las columnas solicitadas y examinamos nuestro 
# objeto.
sublapop <- select(.data=lapop,"q2","q1tb","prov1t","b2","it1","cses6n","ur1new",
                   "ing4","gi0n","anestg")
view(sublapop)
summary(sublapop)

# Obtenemos el número de valores perdidos, el número de valores completos, la 
# proporción de valores perdidos y el porcentaje que representan los valores 
# perdidos del total de valores en nuestra base de datos

n_miss(sublapop)
n_complete(sublapop)

prop_miss(sublapop) 

Porcentaje <- pct_miss(sublapop) # creamos el objeto 'Porcentaje'

# Con una estructura condicional hacemos un primer ejercicio diagnóstico del 
# problema según los rangos trabajados en clase.
if (Porcentaje>15){
  message("INTERPRETACIÓN PERJUDICIAL")
} else if (Porcentaje>5){
  message("MÉTODOS SOFÍSTICADOS")
} else if (Porcentaje>1){
  message("MANEJABLE")
} else if (Porcentaje<=1){
  message("IRRELEVANTE")
}

# Obtenemos el número de valores perdidos agrupadas por variables y el número de 
# variables agrupados por el número de valores perdidos.
miss_var_summary(sublapop)
miss_var_table(sublapop)

# Estimamos el porcentaje que represetan los valores perdido de la variable 'b2'
# del total de valores perdidos en nuestro objeto
pct_miss_b2 <- (n_miss(sublapop$b2)/n_miss(sublapop))*100
pct_miss_b2

# Obtenemos el número de valores perdidos agrupadas por caso (o fila), y el 
# número de casos (o filas) agrupados por el número de valores perdidos.
miss_case_summary(tao)
miss_case_table(tao) 

# RESULTADO: 
# 1. Observamos que nuestro objeto 'sublapop' contiene valores perdidos en 8 de
# sus 10 variables con un total de 1844 valores perdidos.
# 2. Los valores perdidos representan aproximadamente 6% de los valores de 
# nuestro objeto, por lo que el código imprime el mensaje correspondiente 
# señalando que se requieren METODOS SOFISTICADO para el manejo del problema.
# 3. Vemos que el problema se concentra en la variable 'b2' con 1547 valores 
# perdidos,lo que representa el 50.9% de sus casos y el 83% de todos los casos 
# perdidos en nuestro objeto.


## Segunda parte ----

# ¿La variable ING4 (úsela como vector numérico) está relacionada con la 
# ausencia o presencia de datos en IT1? Realice exploración gráfica y prueba de 
# hipótesis.

# Creamos el subset 'sublapop_02' con las columnas solicitadas y examinamos nuestro objeto.
sublapop_02 <- select(.data=sublapop,"ing4","it1")
view(sublapop_02)
summary(sublapop_02)

# Obtenemos el número de valores perdidos, el número de valores completos, la 
# proporción de valores perdidos y el porcentaje que representan los valores 
# perdidos del total de valores en nuestra base de datos

n_miss(sublapop_02)
n_complete(sublapop_02)

prop_miss(sublapop_02) 

pct_miss(sublapop_02)

# Convertimos las variables a 'numeric' como solicitan las indicaciones.
sublapop_02$ing4 <- str_remove_all(sublapop_02$ing4, ",")
sublapop_02$ing4 <- as.numeric(sublapop_02$ing4)
class(sublapop_02$ing4)

sublapop_02$it1 <- str_remove_all(sublapop_02$it1, ",")
sublapop_02$it1 <- as.numeric(sublapop_02$it1)
class(sublapop_02$it1)

# Generamos un gráfico o ggplot sobre la presención y concentración de valores perdidos en nuestro objeto.
vis_miss(sublapop_02)

# Generamos un gráfico o ggplot con el número de valores perdidos por caso.
gg_miss_case(sublapop_02)

# Generamos un gráfico o ggplot con el número de valores perdidos por variable y sus posibles combinaciones
gg_miss_upset(sublapop_02) 

# RESULTADO:
# 1. Según la exploración gráfica notamos que la distribución de los casos perdidos
# en la variable 'it1' no se corresponden con la misma en la variable 'ing4'. 
# 2. De un total de 123 valores perdidos, 105 se concentran en la variable'it1'
# 14 en la variable 'ing4', y en solo 02 casos ambos presentan valores perdidos.
# 3. Por todo lo observado podemos concluir que la variable 'ing4' no está relacionada
# con la presencia o ausencia de valores en 'it1'.
