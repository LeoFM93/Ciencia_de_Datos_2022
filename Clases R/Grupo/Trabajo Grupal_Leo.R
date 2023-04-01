
library(readr)
library(naniar)
library(mapsPERU)
library(dplyr)
library(sf)
library(ggplot2)
library(scales) #usamos el objeto 'comma'

options(scipen = 999)

link <- "https://raw.githubusercontent.com/Diane-12/BD_Pytoreactiva/main/pyto_reactica_12_original_vs2.csv"
bd_reactiva_pyto <- read.csv(link)
bd_reactiva_pyto <- bd_reactiva_pyto[-c(527), ]
View(bd_reactiva_pyto)

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
  filter(row_number() != 526) %>%
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








