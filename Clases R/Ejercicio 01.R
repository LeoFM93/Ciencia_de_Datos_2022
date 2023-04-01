##### EJERCICIO 01 #####

# Pregunta 02.a ####
# Abra las siguientes bases de datos GitHub: WorldCupMatches.csv y WorldCups.csv. 

library(rio)
library(tidyverse)
library(haven)

# Primer Método: Importado desde archivo en ordenador
world_cups_matches<-import("WorldCupMatches.csv")
world_cups<-import("WorldCups.csv")
view(world_cups_matches)
view(world_cups)

# Segundo Método: Importado con enlace de internet obtenido de opción RAW
link01 <- "https://raw.githubusercontent.com/LeoFM93/Diplomado-2022-Fundamentos-R/main/BDs/WorldCupMatches.csv"
link02 <- "https://raw.githubusercontent.com/LeoFM93/Diplomado-2022-Fundamentos-R/main/BDs/WorldCups.csv"
world_cups_matches<-read_csv(link01)
world_cups<-read_csv(link02)
view(world_cups_matches)
view(world_cups)

# Pregunta 02.b ####
# Genere una lista con los países donde se ha realizado una copa mundial. Sólo deben figurar valores únicos.

# Con la función UNIQUE extraemos los valores únicos de la columna COUNTRY y con la función LIST convertimos
# el objeto generado en lista.
lst_pais <- list(unique(world_cups$Country))
print(lst_pais)
class(lst_pais)

# Pregunta 02.c ####
# Presente cuál fue el estadio que ha acogido a la mayor cantidad de espectadores o asistentes en 
# la historia de los mundiales.

a = which.max(world_cups_matches$Attendance)
world_cups_matches[[4]][a]

# Pregunta 02.d ####
# Indique cuál es el referee que más partidos ha arbitrado.

# Primer metodo

cont_ref<-world_cups_matches |> 
  mutate(Num_part=1) |> 
  group_by(Referee) |> 
  summarise(Num_part=sum(Num_part)) |> 
  arrange(desc(Num_part)) |> 
  slice(2)

# Segundo método

cont_ref<-as.data.frame(table(world_cups_matches$Referee))
cont_ref |> 
  names()=c("Referee","Num_part")
cont_ref_arr<-arrange(cont_ref,desc(Num_part))
cont_ref_arr[[1]][2]

# Pregunta 02.e ####
# Cuál es el partido en el que se anotaron más goles.

world_cups_matches |> 
  mutate(gol_tot = `Home Team Goals` + `Away Team Goals`) |> 
  group_by(MatchID) |> 
  arrange(desc(gol_tot)) |> 
  summarise(max_gol=max(gol_tot)) |> 
  arrange(desc(max_gol)) |> 
  slice(1)

# Pregunta 02.f ####
# En la base “WordCupMatches” cree las siguientes nuevas variables 
# (a partir de la base “WorldCups”): equipo ganador, equipo 2do puesto y equipo 3er puesto.

world_cups_matches |> 
  left_join(world_cups[c(1,3,4,5)],by="Year") |> 
  view()

# Pregunta 02.g ####
# En la base “WorldCups” genere dos columnas que indiquen: 1) el número de goles
# anotados por el campeón (primer puesto) y 2) el número de goles anotados por equipo 
# que quedó segundo puesto en cada mundial.

camp_seg_goles<-world_cups_matches |> 
  left_join(world_cups[c(1,3,4,5)],by="Year") |> 
  group_by(Year) |> 
  summarize(camp_goles=sum(`Home Team Goals`[`Home Team Name`==Winner])+sum(`Away Team Goals`[`Away Team Name`==Winner]), 
            seg_goles=sum(`Home Team Goals`[`Home Team Name`==`Runners-Up`])+sum(`Away Team Goals`[`Away Team Name`==`Runners-Up`]))|> 
  view()

world_cups |> 
  left_join(camp_seg_goles[c(1,2,3)],by="Year") |> 
  view()

# Solución 
world_cups_matches |> 
  mutate(goles_campeon=case_when(`Home Team Name`=="Winner"~`Home Team Goals`, 
                                 `Away Team Name`=="Winner"~`Home Team Goals`,
                                 TRUE ~ 0))  |> 
  group_by(Year) |> 
  summarise(sum(goles_campeon)) |> 
  view()

