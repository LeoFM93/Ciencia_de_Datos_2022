#Resolución Ejercicio 05
load("datos_RLM.RData")

head(datos)
summary(datos)

# Módelo Lineal (Linear model = lm) ("~" = "predicho por")
modelo <- lm(ciudadania ~ autoef_ciud, data = datos)
modelo

# Normalidad de los residuales: Contrasta los datos empíricos de los residuales
# con los cuantiles de una distribución normal estándar
modelo$residuals
#Grafica la distribución de los residuales y vemos que cumple con los supuestos de normalidad
qqnorm(modelo$residuals) 

# Resumen del modelo
summary(modelo) #El (intercept) es el B0 y el autoef_ciud es el B1

# intervalos de confianza
confint(modelo, level = c(0.95))