#Instalación de paquetes----

options(scipen = 999)

#install.packages("mapsPERU")
#install.packages("readr")
#install.packages("stringr")
#install.packages("remotes")
#install_github("musajajorge/mapsPERU")

#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%


library(mapsPERU)
library(remotes)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)   

#Cargar BBDD de github----

library(readr)
link<- ("https://raw.githubusercontent.com/Diane-12/BD_Pytoreactiva/main/pyto_reactica_12_original_vs2.csv")
bd_reactiva_pyto <- read.csv(link)
#head(bd_reactiva_pyto,n=2)
str(object=bd_reactiva_pyto)
#sapply(bd_reactiva_pyto, class) #para conocer las clases de cada variable


##Manejo de la data: cambiando la clase de variable (MONTODETRANSFERENCIA2020, MONTODEINVERSION) de "character" a numeric----
bd_reactiva_pyto$MONTODETRANSFERENCIA2020 <- str_remove_all(bd_reactiva_pyto$MONTODETRANSFERENCIA2020, ",") 
bd_reactiva_pyto$MONTODETRANSFERENCIA2020 <- as.numeric(bd_reactiva_pyto$MONTODETRANSFERENCIA2020)

bd_reactiva_pyto$MONTODEINVERSIÓN <- str_remove_all(bd_reactiva_pyto$MONTODEINVERSIÓN, ",") 
bd_reactiva_pyto$MONTODEINVERSIÓN <- as.numeric(bd_reactiva_pyto$MONTODEINVERSIÓN)

class(bd_reactiva_pyto$POBLACIONBENEFICIARIA)
class(bd_reactiva_pyto$MONTODETRANSFERENCIA2020)
class(bd_reactiva_pyto$MONTODEINVERSIÓN)

#Mapa con la distribución de recursos por región ----

library(mapsPERU)
df <- map_REG
view(df)

## Genero una tabla que presenta por región el monto de transferencia ----
TABLA <- bd_reactiva_pyto %>%
  group_by(REGION) %>%
  summarise(suma=sum(MONTODETRANSFERENCIA2020))

#Artificio: estoy creando una fila adicional porque no existe en nuestra BBDD
nuevafila=data.frame(REGION="Madre de Dios", suma=0)
TABLA=rbind(TABLA,nuevafila)

#Acá estoy haciendo el merge entre TABLA y df1 (BBDD con las locaciones, regiones, provincias, etc.)
library(dplyr)
library(sf)
df1 <- left_join(df, TABLA, by="REGION")
str(TABLA)

##Generación del mapa por región ----
library(ggplot2)
ggplot(df1, aes(geometry=geometry)) +
  geom_sf(aes(fill=suma)) +
  labs(fill = "Monto transferido 2020")+
  scale_fill_gradient (low="snow", high="red")
#Anexo: Página de colores --> https://r-charts.com/es/colores/


#Mapa con la distribución de recursos por provincia ----

library(mapsPERU)
df2 <- map_PROV

## Genero una tabla que presenta por provincia el monto de transferencia ----
TABLA2 <-bd_reactiva_pyto %>%
  group_by(COD_PROVINCIA) %>%
  summarise(suma=sum(MONTODETRANSFERENCIA2020))

#Artificio: estoy creando varias filas adicionales (68) porque no existen en nuestra BBDD de Reactiva
nuevafilas=data.frame(COD_PROVINCIA=c("010400", "010600", "020100", "020200", "020300", "020400", "020700", "020900", "021100", "021200", "021300", "021400", "021500", "021700", "022000", "030100", "030500", "040300", "040400", "040500", "040600", "040700", "040800", "050800", "061200", "080200", "080400", "080500", "080600", "081000", "081100", "081200", "081300", "100600", "100700", "100900", "101000", "110200", "110300", "110400", "120700", "120800", "130300", "130500", "130700", "130800", "140200", "150300", "150900", "160300", "160500", "160800", "170100", "170200", "170300", "180100", "180200", "190200", "200800", "210400", "220200", "230200", "230300", "230400", "240200", "250200", "250300", "250400"), suma=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
TABLA2=rbind(TABLA2,nuevafilas)

#Acá estoy haciendo el merge entre TABLA2 y df2 (BBDD con las locaciones, regiones, provincias, etc.)
library(dplyr)
library(sf)
df2 <- left_join(df2, TABLA2, by="COD_PROVINCIA")
str(TABLA2)

##Generación del mapa por provincia----
library(ggplot2)
ggplot(df2, aes(geometry=geometry)) +
  geom_sf(aes(fill=suma)) +
  labs(fill = "Monto transferido 2020")+
  scale_fill_gradient (low="snow", high="red")


#Mapa con la distribución de recursos por región ----

#Creamos una columna adicional con el porcentaje del costo del proyecto cubierto por recursos transferidos.
bd_reactiva_pyto <- cbind(bd_reactiva_pyto, PorcCostoAsumido=c(bd_reactiva_pyto$MONTODETRANSFERENCIA2020 / bd_reactiva_pyto$MONTODEINVERSIÓN))

library(mapsPERU)
df <- map_REG

## Genero una tabla que presenta por región el monto de transferencia ----
TABLA3 <-bd_reactiva_pyto %>%
  group_by(REGION) %>%
  summarise(PromedioPorceAsumido=mean(PorcCostoAsumido))

#Artificio: estoy creando una fila adicional porque no existe en nuestra BBDD
nuevafila3=data.frame(REGION="Madre de Dios", PromedioPorceAsumido=NA)
TABLA3=rbind(TABLA3,nuevafila3)

#Acá estoy haciendo el merge entre TABLA y df1 (BBDD con las locaciones, regiones, provincias, etc.)
library(dplyr)
library(sf)
df3 <- left_join(df, TABLA3, by="REGION")
str(TABLA3)

##Generación del mapa por región ----
library(ggplot2)
ggplot(df3, aes(geometry=geometry)) +
  geom_sf(aes(fill=PromedioPorceAsumido)) +
  labs(fill = "Porcentaje asumido con las transferencias")+
  scale_fill_gradient (low="snow", high="red")
