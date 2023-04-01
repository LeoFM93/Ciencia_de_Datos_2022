#Instalación de paquetes----

options(scipen = 999)

#install.packages("mapsPERU")
#install.packages("readr")
#install.packages("stringr")
#install.packages("remotes")
#install_github("musajajorge/mapsPERU")
#install.packages("tidyverse")
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%


library(mapsPERU)
library(remotes)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)   
library(tidyverse)

#Descarga de la BBDD de github----

library(readr)
link<- ("https://raw.githubusercontent.com/Diane-12/BD_Pytoreactiva/main/pyto_reactica_12_original_vs2.csv")
#str(object=bd_reactiva_pyto)
bd_reactiva_pyto <- read.csv(link)
#sapply(bd_reactiva_pyto, class) #para conocer las clases de cada variable
bd_reactiva_pyto <- bd_reactiva_pyto[-c(526), ]


## Preparación de la BBDD: cambiando la clase de variable (MONTODETRANSFERENCIA2020, MONTODEINVERSION) de "character" a numeric----
bd_reactiva_pyto$MONTODETRANSFERENCIA2020 <- str_remove_all(bd_reactiva_pyto$MONTODETRANSFERENCIA2020, ",") 
bd_reactiva_pyto$MONTODETRANSFERENCIA2020 <- as.numeric(bd_reactiva_pyto$MONTODETRANSFERENCIA2020)

bd_reactiva_pyto$MONTODEINVERSIÓN <- str_remove_all(bd_reactiva_pyto$MONTODEINVERSIÓN, ",") 
bd_reactiva_pyto$MONTODEINVERSIÓN <- as.numeric(bd_reactiva_pyto$MONTODEINVERSIÓN)

class(bd_reactiva_pyto$POBLACIONBENEFICIARIA)
class(bd_reactiva_pyto$MONTODETRANSFERENCIA2020)
class(bd_reactiva_pyto$MONTODEINVERSIÓN)

bd_reactiva_pyto <-bd_reactiva_pyto %>% 
  mutate(COD_REGION = sprintf("%06d", COD_REGION),
         COD_PROVINCIA = sprintf("%06d", COD_PROVINCIA))
  
#Mapa con la distribución de recursos por región----

library(mapsPERU)
df <- map_REG

##Generación de una tabla que presenta por región el monto de transferencia
TABLA <- bd_reactiva_pyto %>%
  group_by(REGION) %>%
  summarise(suma=sum(MONTODETRANSFERENCIA2020))

#Artificio: creación de una fila adicional porque no existe en nuestra BBDD
nuevafila=data.frame(REGION="Madre de Dios", suma=0)
TABLA=rbind(TABLA,nuevafila)

#Ejecutando el merge entre TABLA y df1 (BBDD con las locaciones, regiones, provincias, etc.)
library(dplyr)
library(sf)
df1 <- left_join(df, TABLA, by="REGION")
str(TABLA)

##Generación del mapa por región
library(ggplot2)
ggplot(df1, aes(geometry=geometry)) +
  geom_sf(aes(fill=suma)) +
  theme_bw() +
  labs(fill = "soles") +
  labs(title = "GRAFICO 01. MONTOS TRANSFERIDOS \n POR DEPARTAMENTO") +
  scale_fill_gradient (low="snow", high="orange")
#Anexo: Página de colores --> https://r-charts.com/es/colores/


#Mapa con la distribución de recursos por provincia

library(mapsPERU)
df2 <- map_PROV

## Generación de una tabla que presenta por provincia el monto de transferenciA
TABLA2 <-bd_reactiva_pyto %>%
  group_by(COD_PROVINCIA) %>%
  summarise(suma=sum(MONTODETRANSFERENCIA2020))

#Artificio: creación de filas adicionales (68) porque no existen en nuestra BBDD de Reactiva
nuevafilas=data.frame(COD_PROVINCIA=c("010400", "010600", "020100", "020200", "020300", "020400", "020700", "020900", "021100", "021200", "021300", "021400", "021500", "021700", "022000", "030100", "030500", "040300", "040400", "040500", "040600", "040700", "040800", "050800", "061200", "080200", "080400", "080500", "080600", "081000", "081100", "081200", "081300", "100600", "100700", "100900", "101000", "110200", "110300", "110400", "120700", "120800", "130300", "130500", "130700", "130800", "140200", "150300", "150900", "160300", "160500", "160800", "170100", "170200", "170300", "180100", "180200", "190200", "200800", "210400", "220200", "230200", "230300", "230400", "240200", "250200", "250300", "250400"), suma=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
TABLA2=rbind(TABLA2,nuevafilas)

#Ejecutando el merge entre TABLA2 y df2 (BBDD con las locaciones, regiones, provincias, etc.)
library(dplyr)
library(sf)
df2 <- left_join(df2, TABLA2, by="COD_PROVINCIA")
str(TABLA2)

##Generación del mapa por provincia
library(ggplot2)
ggplot(df2, aes(geometry=geometry)) +
  geom_sf(aes(fill=suma)) +
  theme_bw() +
  geom_sf(aes(fill=suma)) +
  labs(fill = "soles") +
  labs(title = "GRAFICO 02. MONTOS TRANSFERIDOS \n POR PROVINCIA") +
  scale_fill_gradient (low="snow", high="orange")


#Mapa con la transferencia de recursos asumidos por el GN a nivel regional ----

#Creación de una columna adicional con el porcentaje del costo del proyecto cubierto por recursos transferidos.
bd_reactiva_pyto <- cbind(bd_reactiva_pyto, PorcCostoAsumido=c(bd_reactiva_pyto$MONTODETRANSFERENCIA2020 / bd_reactiva_pyto$MONTODEINVERSIÓN))

library(mapsPERU)
df <- map_REG

## Generación de una tabla que presenta por región el monto de transferencia -
TABLA3 <-bd_reactiva_pyto %>%
  group_by(REGION) %>%
  summarise(PromedioPorceAsumido=mean(PorcCostoAsumido))

#Artificio: creación de una fila adicional porque no existe en nuestra BBDD
nuevafila3=data.frame(REGION="Madre de Dios", PromedioPorceAsumido=NA)
TABLA3=rbind(TABLA3,nuevafila3)

#Ejecutando el merge entre TABLA y df1 (BBDD con las locaciones, regiones, provincias, etc.)
library(dplyr)
library(sf)
df3 <- left_join(df, TABLA3, by="REGION")
str(TABLA3)

##Generación del mapa por región 
library(ggplot2)
ggplot(df3, aes(geometry=geometry)) +
  geom_sf(aes(fill=PromedioPorceAsumido)) +
  theme_bw() +
  labs(fill = "Porcentaje ")+
  labs(title = "GRAFICO 03. PARTICIPACIÓN DE LAS TRANFERENCIAS \n EN EL MONTO DE INVERSIÓN")+
  scale_fill_gradient (low="snow", high="orange")
  
# Porcentaje de población beneficiaria por región y por proyectos concluidos ----

#Artificio: creación de una fila adicional porque no existe en nuestra BBDD

library(readr)
link<- ("https://raw.githubusercontent.com/Diane-12/BD_Pytoreactiva/main/reporte.csv")
#str(object=bd_reactiva_pyto)
pob <- read.csv(link)
#sapply(bd_reactiva_pyto, class) #para conocer las clases de cada variable

bdpob<-bd_reactiva_pyto |>
  left_join(pob[c(1,2,5,6,7)], by= "UBIGEO", "COD_PROVINCIA") |>
  view()

pb_reg<-bdpob |>
  group_by(REGION) |> 
  summarize(pob_ben=sum(`POBLACIONBENEFICIARIA`[`ESTADO`=="Concluido"]),
            pob_reg=mean(POB_REG[`ESTADO`=="Concluido"])) |>
  view()

pb_reg<-pb_reg |>
  mutate(porc_pb=(pob_ben/pob_reg)*100)

nuevafila2=data.frame(REGION="Madre de Dios", pob_ben=NA, pob_reg=NA, porc_pb=NA)
pb_reg=rbind(pb_reg,nuevafila2)

pb_reg2 <- left_join(pb_reg, df, by="REGION")
str(pb_reg2)

pb_reg2$porc_pb

library(ggplot2)
ggplot(pb_reg2, aes(geometry=geometry)) +
  geom_sf(aes(fill=porc_pb)) +
  theme_bw() +
  labs(fill = "Porcentaje ")+
  labs(title = "GRAFICO 04. PORCENTAJE DE POBLACIÓN BENEFICIARIA \n POR REGIÓN")+  
  scale_fill_gradient (low="snow", high="orange")

  
# Número de proyectos por tipología ----
bd_reactiva_pyto %>%
  group_by(TIPOLOGIA) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=reorder(TIPOLOGIA, (-count)), y=count)) +
  geom_col(colour="black") +
  geom_bar(stat = "identity", fill="orange", color="grey40") +
  theme_bw() +
  #coord_flip() +
  geom_text(aes(label=after_stat(y), fontface= "bold"), vjust=-0.2) +
  theme(legend.title = element_blank()) +
  labs(x=" ", y=" ", title = "GRAFICO 04. NÚMERO DE PROYECTOS POR TIPO", subtitle= " ")

# Montos transferidos por tipo de proyecto ----
bd_reactiva_pyto %>%
  group_by(TIPOLOGIA) %>%
  summarise(SOLES=sum(MONTODETRANSFERENCIA2020)) %>%
  ggplot(aes(x=reorder(TIPOLOGIA, (-SOLES)), y=SOLES, fill=TIPOLOGIA)) +
  geom_col(colour="black") +
  geom_bar(stat = 'identity', fill="orange", color="grey40") +
  theme_bw() +
  geom_text(aes(label=scales::comma(round(SOLES/1000), accuracy = 1), fontface="bold"), vjust=-0.2) +
  #scale_y_continuous(labels = label_dollar())
  labs(x=" ", y=" ", title = "GRAFICO 05. MONTOS TRANSFERIDOS POR TIPO DE PROYECTO", subtitle= "Miles de Millones de soles") +
  theme(legend.position = "none") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Relación entre empleos generados y monto de inversión/transferido (LEONEL)----


library(readr)
link<- ("https://raw.githubusercontent.com/Diane-12/BD_Pytoreactiva/main/pyto_reactica_12_original_vs2.csv")
bd_reactiva_pyto <- read.csv(link)
View(bd_reactiva_pyto)
# head(bd_reactiva_pyto,n=5)
# str(object=bd_reactiva_pyto)
sapply(bd_reactiva_pyto, class)



## Manejo de la data----


### Generamos 01 data frame con nuestras 08 variables de interés.----

# Para esto:
# 1. Completamos los valores de 'COD_REGION' y 'COD_PROVINCIA' con '0' hasta completar las 6 cifras.
# Al ser importado como 'character', no considera los ceros a la izquierda, lo que genera problemas con la graficación más adelante.
# 2. Convertimos de 'character' a 'numeric' la clase de 'MONTOTRANSFERENCIA2020' y 'MONTODEINVERSIÓN'.
# Removemos la coma separador de miles para que reconozca nuestros valores como número y no texto.
# 3. Seleccionamos las variables que nos interesan.
# 4. El resultado es asignado al objeto 'emp_mont' para el anális de relación entre empleos generados y monto de inversión/transferido

emp_mont <- bd_reactiva_pyto %>% 
  mutate(COD_REGION = sprintf("%06d", as.integer(COD_REGION)),
         COD_PROVINCIA = sprintf("%06d", as.integer(COD_PROVINCIA))) %>% 
  mutate(MONTODETRANSFERENCIA2020 = str_remove_all(MONTODETRANSFERENCIA2020, ","),
         MONTODETRANSFERENCIA2020 = as.numeric(MONTODETRANSFERENCIA2020),
         MONTODEINVERSIÓN = str_remove_all(MONTODEINVERSIÓN, ","),
         MONTODEINVERSIÓN = as.numeric(MONTODEINVERSIÓN)) %>%
  select('COD_REGION','COD_PROVINCIA','REGION', 'PROVINCIA', 'DISTRITO', 'TOTALEMPLEOS', 'MONTODEINVERSIÓN', 'MONTODETRANSFERENCIA2020') 



# Examinamos las variables de nuestro objeto
View(emp_mont)
summary(emp_mont)
miss_var_summary(emp_mont)
miss_case_summary(emp_mont) # Observamos que los 05 valores perdidos se concentran en la fila 526.


### Generamos data frames para los 03 niveles de análisis: Regional, Provincial y Distrital ----

#### 1. Por Región:

emp_mont_reg <- emp_mont %>%
  group_by(REGION) %>%
  summarise("Reg"=REGION,"Cod_Reg"=COD_REGION,"Cod_Prov"=COD_PROVINCIA,"Tot_Emp" = sum(TOTALEMPLEOS), "Mon_Inv" = sum(MONTODEINVERSIÓN), "Mon_Tran" = sum(MONTODETRANSFERENCIA2020)) 

View(emp_mont_reg)

#### 2. Por Provincia:

emp_mont_prov <- emp_mont %>%
  group_by(PROVINCIA) %>%
  summarise("Reg"=REGION,"Cod_Reg"=COD_REGION,"Cod_Prov"=COD_PROVINCIA,"Tot_Emp" = sum(TOTALEMPLEOS), "Mon_Inv" = sum(MONTODEINVERSIÓN), "Mon_Tran" = sum(MONTODETRANSFERENCIA2020)) 

View(emp_mont_reg)

#### 3. Por Distrito:

emp_mont_dist <- emp_mont %>%
  group_by(DISTRITO) %>%
  summarise("Reg"=REGION,"Cod_Reg"=COD_REGION,"Cod_Prov"=COD_PROVINCIA,"Tot_Emp" = sum(TOTALEMPLEOS), "Mon_Inv" = sum(MONTODEINVERSIÓN), "Mon_Tran" = sum(MONTODETRANSFERENCIA2020)) 

View(emp_mont_reg)


### Generamos gráficos de la relación entre Total de Empleos, Monto de Inversión y Monto Transferido.----


#### Gráfico Total de Empleos y el Monto de Inversión.

#### 1. Por Región:

ggplot(emp_mont_reg, aes(x = Total_Empleos, y = Monto_de_Inversión)) + 
  geom_point() +
  ggtitle("Relación entre Total de Empelos y el Monto de Inversión a nivel Regional ") +
  xlab("Total Empleos") +
  ylab("Monto de Inversión") +
  scale_x_continuous(limits = c(0, 1200), labels = scales::comma) +
  scale_y_continuous(limits = c(1000, 21000000), labels = scales::comma) +
  theme_classic()

#### 2. Por Provincia:

ggplot(emp_mont_prov, aes(x = Total_Empleos, y = Monto_de_Inversión)) + 
  geom_point() +
  ggtitle("Relación entre Total de Empelos y el Monto de Inversión a nivel Provincial ") +
  xlab("Total Empleos") +
  ylab("Monto de Inversión") +
  scale_x_continuous(limits = c(0, 1200), labels = scales::comma) +
  scale_y_continuous(limits = c(1000, 21000000), labels = scales::comma) +
  theme_classic()

#### 3. Por Distrito:

ggplot(emp_mont_dist, aes(x = Total_Empleos, y = Monto_de_Inversión)) + 
  geom_point() +
  ggtitle("Relación entre Total de Empelos y el Monto de Inversión a nivel Provincial ") +
  xlab("Total Empleos") +
  ylab("Monto de Inversión") +
  scale_x_continuous(limits = c(0, 1200), labels = scales::comma) +
  scale_y_continuous(limits = c(1000, 21000000), labels = scales::comma) +
  theme_classic()


#### Gráfico Total de Empleos y el Monto de Transferencia

#### 1. Por Región:

ggplot(emp_mont_reg, aes(x = Total_Empleos, y = Monto_de_Transferencia)) + 
  geom_point() +
  ggtitle("Relación entre Total de Empelos y el Monto de Inversión a nivel Regional ") +
  xlab("Total Empleos") +
  ylab("Monto de Inversión") +
  scale_x_continuous(limits = c(0, 1200), labels = scales::comma) +
  scale_y_continuous(limits = c(1000, 21000000), labels = scales::comma) +
  theme_classic()

#### 2. Por Provincia:

ggplot(emp_mont_prov, aes(x = Total_Empleos, y = Monto_de_Transferencia)) + 
  geom_point() +
  ggtitle("Relación entre Total de Empelos y el Monto de Inversión a nivel Provincial ") +
  xlab("Total Empleos") +
  ylab("Monto de Inversión") +
  scale_x_continuous(limits = c(0, 1200), labels = scales::comma) +
  scale_y_continuous(limits = c(1000, 21000000), labels = scales::comma) +
  theme_classic()

#### 3. Por Distrito:

ggplot(emp_mont_dist, aes(x = Total_Empleos, y = Monto_de_Transferencia)) + 
  geom_point() +
  ggtitle("Relación entre Total de Empelos y el Monto de Inversión a nivel Provincial ") +
  xlab("Total Empleos") +
  ylab("Monto de Inversión") +
  scale_x_continuous(limits = c(0, 1200), labels = scales::comma) +
  scale_y_continuous(limits = c(1000, 21000000), labels = scales::comma) +
  theme_classic()



### INCOMPLETO Generamos mapas de la relación entre Total de Empleos, Monto de Inversión y Monto Transferido.----


#### Importamos los mapas que necesitamos para cada nivel de análisis
library(mapsPERU)
map_reg_00 <- map_REG
map_prov_00 <- map_PROV
map_dist_00 <- map_DIST


#### Integramos nuestros datas frames con los mapas por niveles de análisis:

##### 1. Por Región:
map_reg_01 <- left_join(map_reg_00, emp_mont_reg, by="REGION")
str(map_reg_01)

View(map_reg_01)

##### 2. Por Provincia:
map_prov_01 <- left_join(map_prov_00, emp_mont_prov, by="PROVINCIA")
str(map_prov_01)

##### 3. Por Distrito:
map_dist_01 <- left_join(map_dist_00, emp_mont_dist, by="DISTRITO")
str(map_dist_01)



#### Mapas Total de Empleos y Monto de Inversión:

##### 1. Por Región:
ggplot(map_reg_01, aes(geometry=geometry)) +
  geom_sf(aes(fill=Mon_Inv)) +
  labs(fill = "Monto de Inversión")+
  scale_fill_gradient (low="snow", high="red")


##### 2. Por Provincia:
ggplot(map_prov_01, aes(geometry=geometry)) +
  geom_sf(aes(fill=Monto_de_Inversión)) +
  labs(fill = "Monto de Inversión")+
  scale_fill_gradient (low="snow", high="red")



#### Mapas Total de Empleos y Monto de Transferencia:

##### 1. Por Región:
ggplot(map_reg_01, aes(geometry=geometry)) +
  geom_sf(aes(fill=Monto_de_Transferencia)) +
  labs(fill = "Monto de Transferencia")+
  scale_fill_gradient (low="snow", high="red")


##### 2. Por Provincia:
ggplot(map_prov_01, aes(geometry=geometry)) +
  geom_sf(aes(fill=Monto_de_Transferencia)) +
  labs(fill = "Nuevos Soles") +
  labs(title = "Gráfico 02. Monto de Inversión por Provincia") +
  scale_fill_gradient (low="snow", high="red")



# Monto transferido y costo de Proyecto por tipo de intervención ----

library(ggplot2)
#install.packages("ggplot")
#library(ggplot)
library(reshape2)
#install.packages("magrittr")
library(tidyr)
#install.packages("scales")
library(scales)
library(dplyr)


#Creación de tabla de resumen:
TABLA4 <-bd_reactiva_pyto %>%
  group_by(TIPOLOGIA) %>%
  summarise(TRANSFERENCIA=sum(MONTODETRANSFERENCIA2020), COSTO_INVERSION=sum(MONTODEINVERSIÓN))


#Creación de gráfico

df_1 <- TABLA4 |>
  pivot_longer(cols = TRANSFERENCIA:COSTO_INVERSION, names_to= "soles", values_to = "valor" )
#View(df_1)

ggplot(df_1, aes(x=TIPOLOGIA, y=valor, fill=soles)) +
  geom_bar(position = position_dodge(width=0.9), stat="identity") +
  geom_text(aes(label=scales::comma(round(valor/1000000)), hjust=1.15,  fontface="bold"), position=position_dodge(width=0.9) ) +
  theme_bw() +
  scale_y_continuous(label = label_number()) +
  theme(legend.title = element_blank()) +
  labs(x=" ", y=" ", title = "GRAFICO 06. MONTO TRANSFERIDO \n Y COSTO DE PROYECTO POR TIPO", subtitle= "Millones de soles") +
  coord_flip()


