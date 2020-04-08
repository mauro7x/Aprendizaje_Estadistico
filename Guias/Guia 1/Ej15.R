# -------------------------------------------------------------------------------------------
# 15. Generacion de modelos.
# -------------------------------------------------------------------------------------------

# setup de librerias
library(ggplot2)

# cargamos los datos
# -

# funciones a utilizar

cuadrados_minimos <- function(X, y) {
  estimador <- solve(t(X) %*% X) %*% t(X) %*% y
  return(estimador)
}

# -------------------------------------------------------------------------------------------
# a) Generar el siguiente modelo:

# Crear dos vectores de datos de tamaño 100 x1 y x2 a partir de una distribución uniforme en
# el intervalo (0, 1).
x1 <- runif(100)
x2 <- runif(100)

# Crear el vector y = 2 + 2∗x1 + 0.3∗x2 + e, con e que contenga 100 observaciones provenientes
# de una distribución N (0, 1).
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

# ¿Cuales son los coeficientes de regresión?
# Los coeficientes de regresion son, justamente, {2, 2, 0.3}.

# Estimar la correlación entre x1 y x2.
# Teoricamente no deberia haber...
e_x1 <- mean(x1)
e_x2 <- mean(x2)
e_x1x2 <- mean(x1*x2)
cov_x1x2 <- e_x1x2 - e_x1*e_x2
coef_correlacion <- cov_x1x2/((sd(x1))*(sd(x2)))
coef_correlacion # estimacion del coeficiente de correlacion

# Correlacion utilizando la funcion de R
cor(x1, x2)


# Realizar un scatterplot en el que pueda observarse la relación entre x1 y x2 .
df <- data.frame("x1" = x1, "x2" = x2, "y" = y)

g <- ggplot(data = df, aes(x=x1, y=x2)) +
  geom_point(col= "coral") +
  geom_smooth(col="coral4", formula = y~x, method = loess)
plot(g)

# Utilizando los datos generados, ajustar a un modelo lineal para predecir y en función de
# x1 y x2, utilizando el método de cuadrados minimos, y comparar los valores exactos de β
# con sus valores estimados.

X <- cbind(rep(1,100), x1, x2)
est_lineal <- cuadrados_minimos(X, y)
modelo_lineal <- function(x1, x2) est_lineal[1] + est_lineal[2]*x1 + est_lineal[3]*x2

est_lineal

# -------------------------------------------------------------------------------------------
# b) Repetir el inciso a) pero con el siguiente modelo:

# Crear dos vectores de datos de tamaño 100 x1 a partir de una distribución uniforme en el
# intervalo (0, 1), y x2 = 0.5 ∗ x1 + rnorm(100)/10.
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10

# Crear el vector y = 2 + 2∗x1 + 0.3∗x2 + rnorm(100).
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

# Comparar los resultados obtenidos con los del item a):

# ¿Cuales son los coeficientes de regresión?
# Los coeficientes de regresion son, nuevamente, {2, 2, 0.3}.

# Estimar la correlación entre x1 y x2:
# Estimamos la esperanza utilizando el promedio, por lo que, utilizando la sig. formula:
# cov(x,y) = E[xy] - E[x]E[y] calculamos la cov entre x1 y x2:

e_x1 <- mean(x1)
e_x2 <- mean(x2)
e_x1x2 <- mean(x1*x2)
cov_x1x2 <- e_x1x2 - e_x1*e_x2
coef_correlacion <- cov_x1x2/((sd(x1))*(sd(x2)))

coef_correlacion # estimacion del coeficiente de correlacion

# Correlacion utilizando la funcion de R
cor(x1, x2)

# Realizar un scatterplot en el que pueda observarse la relación entre x1 y x2 .
df <- data.frame("x1" = x1, "x2" = x2, "y" = y)

g <- ggplot(data = df, aes(x=x1, y=x2)) +
  geom_point(col= "coral") +
  geom_smooth(col="coral4", formula = y~x, method = loess)
plot(g)

# Utilizando los datos generados, ajustar a un modelo lineal para predecir y en función de
# x1 y x2, utilizando el método de cuadrados minimos, y comparar los valores exactos de β
# con sus valores estimados.

X <- cbind(rep(1,100), x1, x2)
est_lineal <- cuadrados_minimos(X, y)
modelo_lineal <- function(x1, x2) est_lineal[1] + est_lineal[2]*x1 + est_lineal[3]*x2

est_lineal
