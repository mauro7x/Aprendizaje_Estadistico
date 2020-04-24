# -------------------------------------------------------------------------------------------
# 1. Analizaremos el set de datos “Boston Houssing“ en R (del paquete mlbench)
# El set de datos cuenta con 14 variables, de las cuales estudiaremos las siguientes:

# (nox) Concentracion de oxido de nitrogeno (partes por 10 millones)
# (rm) Promedio de cantidad de habitaciones por vivienda
# (lstat) Status mınimo de la poblacion (porcentaje)
# (medv) Valor mediano de las casas ocupadas por duenios en miles de dolares

# -------------------------------------------------------------------------------------------

# setup de librerias
library(ggplot2)
library(mlbench)
library(GGally)

# -------------------------------------------------------------------------------------------

data("BostonHousing")

# attach(BostonHousing) # me define las variables sin tener que definirlas

nox <- BostonHousing$nox
rm <- BostonHousing$rm
lstat <- BostonHousing$lstat
medv <- BostonHousing$medv

X <- cbind(nox, rm, lstat, medv)

df<- data.frame(X)

cor(X)

# -------------------------------------------------------------------------------------------
# a) ¿Que puede decir a partir de la tabla de correlaciones entre las variables? 
# Si tuviera que elegir una sola variable para plantear el modelo, ¿Cual eligirıa?

# Eligiria lstat, pues tiene correlacion mas alta (en valor absoluto).


# Grafico visto en clase. Descomentar para armarlo, tarda en procesar.
# ggpairs(as.data.frame(X))

# -------------------------------------------------------------------------------------------
# Ajustamos el modelo.

ajuste <- lm(medv ~ nox + rm + lstat, data=df)
summary(ajuste)

# Significacion de la regresion (ver carpeta).

# -------------------------------------------------------------------------------------------
# b) A partir de la tabla de coeficientes estimados,

# ¿Que variables son significativas?
# rm y lstat, y el intercept no lo sacamos.

# ¿A que nivel?
# Practicamente a nivel alpha = 1.

# ¿Cual es el valor de s?
# s = 5.543

# Especificar las hipotesis nulas y alternativas de cada uno de los test t reportados en la tabla.
# seria que cada B sea 0, vs que no sea 0.

# ¿Como se calculan el valor p de este test?
# Se calcula el t obs, utilizando el estadistico con la muestra, y posteriormente se calcula la
# probabilidad de que la variable caiga por encima de este t obs.

# -------------------------------------------------------------------------------------------
# c) ¿Es la regresion significativa?
# Si, es significativa, pues en el test rechazamos H0 con un p-v practicamente 0.

# Especificar las hipotesis nula y alternativa de este test.
# h0: todos los B son 0 vs h1: algun B no es 0.

# ¿Como se calcula el p-valor en este caso?
# En este caso se busca un Fobs evaluando el estadistico con la muestra, y se calcula la 
# probabilidad de que nuestra variable que distribuye como F_3_502 caiga por encima de el F obs.

# ¿Rechazarıa a un nivel de significacion de 0.05?
# Si, pues el p-v es practicamente 0.

# -------------------------------------------------------------------------------------------
# d)  ¿Como se calcula la matriz de correlacion de los estimadores?
# Lo hago en la carpeta para usar lapiz y papel (ver hojas 9 y 10 del cuaderno).

# Verificamos en R lo obtenido.
#  ((solve(t(X) %*% X))[i][j]) / (sqrt(((solve(t(X) %*% X))[i][i])*((solve(t(X) %*% X))[j][j]))) 

#correlacion <- function(X, i, j) {
  #return(((solve(t(X) %*% X))[i][j]) / (sqrt(((solve(t(X) %*% X))[i][i])*((solve(t(X) %*% X))[j][j]))))
#}

#for (i in 1:4) {
  #for (j in 0:3){
    #print(((solve(t(X) %*% X))[j*4 + i]) / (sqrt(((solve(t(X) %*% X))[(i-1)*4 + i])*((solve(t(X) %*% X))[j*4 + (j+1)]))))
  #}
#}

# funciona! :)

# -------------------------------------------------------------------------------------------
# e) ¿Que supuestos necesita realizar para que las conclusiones anteriores sean validas?
# Es necesario asumir que se cumple el modelo omega donde Y ~ Nn(XB, sigma^2*I),
# ademas de que rg(X) = p, B = R^p, y tenemos n observaciones.

# SUPONER ERROR NORMAL Y DE ESPERANZA 0.

# -------------------------------------------------------------------------------------------
# 2. Hecho en otro R.
# -------------------------------------------------------------------------------------------









