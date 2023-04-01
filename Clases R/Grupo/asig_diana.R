
# 3.1
#importando csv: bd_pytos_reactiva
#read_xlsx("~/Ciencia de Datos/Ciencia_de_Datos_2022/Clases R/Grupo")
library(tidyverse)
library(readxl)

bd_reactiva<-read_xlsx("~/Ciencia de Datos/Ciencia_de_Datos_2022/Clases R/Grupo/pyto_reactica_12_original_.xlsx", col_names = TRUE)
view(bd_reactiva)

str(bd_reactiva)

bd_reactiva %>%
  select(MONTODETRANSFERENCIA2020,TIPOLOGIA) %>%
  ggplot() +
  aes(x=TIPOLOGIA) +
  aes(color=TIPOLOGIA) +
  geom_bar()


bd_reactiva %>%
  select(MONTODEINVERSIÓN,TIPOLOGIA) %>%
  ggplot() +
  aes(x=TIPOLOGIA) +
  aes(color=TIPOLOGIA) +
  geom_bar()



