###############################
##### SESIÓN 03 TIDYVERSE #####
###############################

#### Visualización de Datos con GGPLOT2 ####
#Forma parte del conjunto de librerías llamado TIDYVERSE: 
# INSTALL.PACKAGES("TIDYVERSE") -> LIBRARY(TIDYVERSE)
library(tidyverse)

# La gramática del GGPLOT2 se basa en el libro THE GRAMMAR OF GRAPHICS. 
# A diferencia de los gráficos con el paquete base de pasos sucesivos, GGPLOT2 
# se basa en una gramática de gráficos, añadiendo elementos a un graphical 
# device, donde distintos componentes independientes se pueden combinar de 
# muchas maneras diferentes.

# Son 07 capas: 

# 01. Data: 
# Es la materia prima sobre la cual se van a posicionar el resto de capas y los datos que se dean representar.
# El GGPLOT solo acepta un tipo de datos: data.frames/tibbles. No permite vectores.
# Vamos a utilizar la data del paquete {gadminder}
install.packages("gapminder")
library(gapminder)
data <- gapminder::gapminder
head(data,3)

# 02. Aesthetics:
# Indican las variables que se van a graficar, tanto en el eje horizontal(x) como en el eje vertical(y).
# GGPLOT2 no está pensando para gráficos tridimensionales, pero ciertamente podemos incluir una tercera variable, por ejemplo, indicando el color si deseamos identificar grupos, o indicando el tamaño (dr los puntos scatterplot) para agregar una nueva variable cuantitativa
# aes()

# 03. Geometric Objects:
# Funciones: geom-line(), geom-boxplot(), etc.
# Indica qué tipo de gráfico (geometría) se va a construir: gráfico de barras, columnas, 
# puntos, histográmas, boxplots, líneas, densidad, entre otros.
# En el paquete GGPLOT existen 30 geometrías disponibles. Puedes ver el detalle de estos
# en la documentación del paquete.
# Cada geometría tiene su propia función y, como ya hemos visto, cada una puede 
# tener distintos argumentos.

#Hasta aquí generamos algunos gráficos base

# Gráfico 01
data |> ggplot()
  aes(x = lifeExp) +
  # geom_histogram()
  geom_histogram(binwidth=5,color="white") # El número indica el grueso de las 
  # columnas del gráfico, y el color el color del delineado: geom_histogram(binwidth=10,color="red")

# Gráfico 02
data |> ggplot() +
  aes(y = lifeExp) +
  # geom_boxplot()
  geom_boxplot(notch = TRUE,color="blue")

# Gráfico 03
data |> ggplot() +
  aes(x = lifeExp) +
  aes(y = gdpPercap) +
  geom_point()

# Gráfico 03
data %>%
  ggplot() +
  aes(x = lifeExp) +
  aes(y = gdpPercap) +
  geom_point() +
  aes(color=continent) +
  geom_smooth()

#Gráfico 04
data %>%
  filter(country=="Peru",year>1960)%>%
  ggplot() +
  aes(x = year) +
  aes(y = gdpPercap) +
  geom_line() +
  geom_point()

# Gráfico 05
data %>%
  select(country,continent) %>%
  group_by(continent) %>%
  filter(!duplicated(country)) %>%
  ungroup() %>% 
  ggplot() +
  aes(x=continent) +
  geom_bar()

# Gráfico 06
data %>%
  select(-4,-5) %>%
  mutate(gdp_cat=case_when(
    gdpPercap<4000~"Bajo GDPpc",
    TRUE~"Alto GDPpc")) %>%
  filter(year==2007) %>%
  ggplot() +
  aes(x=continent,fill=gdp_cat) +
  geom_bar()

# Gráfico 07
data %>%
  delect(-4,-5) %>%
  
# Gráfico 08
data %>%
  filter(continent=="Oceania") %>%
  ggplot()+
  aes(x=year) +
  aes(y=lifeExp) +
  aes(color=country) +
  geom_line() + 
  geom_point()

# Gráfico 09
data %>%
  filter(continent=="Oceania") %>%
  ggplot()+
  aes(x=year) +
  aes(y=lifeExp) +
  aes(group=country) +
  geom_line() +  
  geom_point()  

  

# 04. Facets:
# Permite descomponer un gráfico en subgráficos, también llamadas cuadrículas o 
# facetas según una variable cualitativa.
# Sirve para comparar grupos, separándolos y así facilitando la 
# Utilizamos FACET_WRAP()

data %>%
  ggplot() +
  aes(x = lifeExp) +
  aes(y = gdpPercap) +
  geom_point() +
  aes(color=continent) +
  facet_wrap(~continent)

# Usamos FACET_GRIP() para cruzar las categorías de dos vairables cualitativas

data %>%
  filter(continent %in% c("Africa","Americas", "Europe")) %>%
  filter(year==2007) %>%
  mutate(gdp_cat=case_when(gdpPercap<4000~ "Bajo GDPpc",
                           TRUE~ "Alto GDPpc")) %>%
  ggplot() +
  aes(x = lifeExp) +
  aes(y = gdpPercap) +
  geom_point() +
  aes(color=continent) +
  facet_grid(cols = vars(continent), rows = vars(gdp_cat))

# 05. Statistical Transformations:

data %>%
  mutate(gdp_cat=case_when(
    gdpPercap<4000~ "Bajo GDPpc",
    TRUE~ "Alto GDPpc")) %>%
  filter(year==2007) %>%
  ggplot()+
  aes(x=continent, group=gdp_cat, color=gdp_cat)+
  stat_summary(aes(y=gdpPercap),
               fun ="mean",
               geom="point") +
  stat_summary(aes(y=gdpPercap),  #<<
               fun ="mean",  #<<
               geom="line")


data %>%
  select(-4, -5) %>%
  mutate(gdp_cat=case_when(
    gdpPercap<4000~ "Bajo GDPpc",
    TRUE~ "Alto GDPpc")) %>%
  filter(year==2007) %>%
  ggplot()+
  aes(x=continent)+ aes(y=gdpPercap) +
  aes(color=continent) +
  geom_boxplot()+
  stat_summary(fun ="mean",
               colour="red",
               size = 5,
               geom="point") +
  stat_summary(fun ="median",  #<<
               colour="blue",  #<<
               size = 5,  #<<
               geom="point")

# 06. Coordinates o Cordenadas:

data %>%
  filter(continent=="Asia", year==2007) %>%
  ggplot()+
  aes(x = gdpPercap, y = lifeExp,
      size = pop, color = country) %>%
  geom_point(show.legend = F, alpha = 0.7) +
  scale_x_log10() +
  labs(x = 'GDP Per Capita',  
       y = 'Life Expectancy')

# 07. Theme: 

# Funciones: theme_gray(), bw(), theme_classic()
# Es la capa que le da la apariencia final que tendrá el gráfico
# Se utiliza para personalizar el estilo del gráfico, pues modifica elementos 
# del gráfico no ligado al 

data %>%
  filter(continent=="Asia", year==2007) %>%
  ggplot() +
  aes(x = gdpPercap, y = lifeExp,
      size = pop, color = country) +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_x_log10() +
  labs(x = 'GDP Per Capita',
       y = 'Life Expectancy') +
  scale_size(range = c(2, 15)) +
  theme_gray()

##### GGANIMATE #####

install.packages("gganimate")
install.packages("gifski")
install.packages("av")
install.packages("png")
install.packages("ggplot2")
library(gganimate)
library(gifski)
library(av)
library(png)

# Luego añadimos la transición animada y lo personalizamos

data |> 
  ggplot() +
  aes(x = gdpPercap, y = lifeExp,
      size = pop, color = country) +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_x_log10() +
  labs(x = 'GDP Per Capita',
       y = 'Life Expectancy') +
  scale_size(range = c(2, 15)) +
  transition_time(year) +
  labs(title = "Producto Bruto Interno vs Esperanza de Vida",
       subtitle = "Year:{frame_time}") +
  shadow_wake(0.5)


install.packages("ggrepel")
# vemos la diferencia entre el GGPLOT2 y REPEL