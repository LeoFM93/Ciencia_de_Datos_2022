#Habilitamos los paquetes necesarios para correr las funciones
library(readxl)
library(readr)
library(dplyr)
library(timechange)
library(lubridate)
library(ggplot2)

#Cargamos la BBDD de un archivo excel y la describimos
casos_covid <- read_csv2("positivos_covid.csv")
str(casos_covid)

#Filtramos la data por los departamentos que se busca analizar
casos_covid <- casos_covid |> 
  filter(DEPARTAMENTO=="AMAZONAS"|DEPARTAMENTO=="APURIMAC"|DEPARTAMENTO=="AYACUCHO"|DEPARTAMENTO=="HUANCAVELICA"|DEPARTAMENTO=="PASCO"|DEPARTAMENTO=="TACNA")

#Crear variable fecha con formato ymd
casos_covid <-casos_covid|> 
  mutate(fecha = ymd(FECHA_RESULTADO))

# Crear year_start para el año de inicio de la pandemia (2020)
year_start <- 2020

# Crear month_start para el mes de inicio de la pandemia (3 para marzo)
month_start <- 3

# Crear variable para el número de mes desde el inicio de la pandemia
n_meses<- 12 * (year(casos_covid$fecha) - year_start) + 
  (month(casos_covid$fecha) - month_start) + 1

# Integrar la variable n_meses en la bd principal
casos_covid<- casos_covid |>
  mutate(n_meses=n_meses)

# Creamos un df nuevo con solo las variables de n_meses y casos
cc_mes<- casos_covid |>
  filter(n_meses >= 1, n_meses <= 21) |>
  count(n_meses)

# Creamos el grafico animado y lo guardamos como GIF
library(tidyverse)
library(gganimate)
cc_mes |>
  ggplot(aes(x = n_meses, y = n, color = n_meses)) +
  geom_line() +
  ggtitle("Evolución del número total de casos positivos por mes") +
  xlab("Mes") +
  ylab("Casos positivos") +
  transition_reveal(n_meses) +
  anim_save("gif_p1A.gif")

#Podemos ver que entre las regiones Amazonas, Apurímac, Tacna, Ayacucho, Huancavelica,
#y Pasco se tuvo el pico para el mes 6 (Ago-20), reduciéndose desde ahí los casos
#hasta el mes 10 (Dic-20) y elevándose constantemente hasta entre los meses 13 (Mar-21)
#y 14 (Abr-21), para finalmente tener una reducción de casos hasta el mes 20 (Oct-21)
#y empezando un nuevo incremento de casos en el mes 21 (Nov-21)
#La etapa más grave de los primeros 21 meses de pandemia se presentó en Ago-20, llegando
#a tener alrededor de 23 mil casos positivos en el mes. Asimismo, podemos apreciar
#en el gráfico la 1ra y 2da ola de la pandemia, así como el inicio de la 3ra ola (Nov-21)

# B)
# Crear la variable Macroregion a partir de las regiones seleccionadas
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

