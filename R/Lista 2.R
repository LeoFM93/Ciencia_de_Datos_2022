#Ejercicio 10
pbar = .8
p0 = .9
n = 100
alpha = .01
z = (pbar-p0)/sqrt(p0*(1-p0)/n)
z.alpha = qnorm(1-alpha)
pval = 1-pnorm(z)


#Ejercico 2
media = 500
sd = 150
n = 300
1-pnorm(510,media,sd/sqrt(n))
ERROR = 1-pnorm(510,500,150)

#Ejercicio 12
n = 60
p = 0.3
confianza = .99
alpha = 1-confianza
p+qnorm(1-(alpha/2))*sqrt(p*(1-p)/n)
#Tambien se puede reemplazar "z" = qnorm(1-(alpha/2)) y reemplazarlo en el texto.
z = qnorm(1-(alpha/2))
li = p-z*sqrt(p*(1-p)/n)
ls = p+z*sqrt(p*(1-p)/n)

#Ejercicio 17
xbar = 115
mu0 = 115
sigma = 20
n = 50
z = (xbar-mu0)/()

#FUNCIONES
X = 65
Y = 784
Z = 46
sum(X,Y,Z)
X^2+(12*X)+36