# -------------------------------------------------------------------------------------------
# 14. En este ejercicio se crearán datos simulados y se ajustará un modelo de regresión
# lineal simple.
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
# 1. Utilizando la función rnorm, crear un vector x que contenga 100 observaciones provenientes
# de una distribución N (0, 1).
x <- rnorm(100, 0, sqrt(1))

# -------------------------------------------------------------------------------------------
# 2. Utilizando la función rnorm, crear un vector epsilon que contenga 100 observaciones
# provenientes de una distribución N (0, 0.025).
e <- rnorm(100, 0, sqrt(0.025))

# -------------------------------------------------------------------------------------------
# 3. Usando x y epsilon, generar un vector acorde al modelo:
    # y = −1 + 0.5x + epsilon
# ¿Cuál es la longitud del vector y? ¿Cuáles son los valores de β 0 y β 1 en el modelo?

y <- -1 + 0.5*x + e
# Tiene longitud 100, y los valores de B son B0 = -1, B1 = 0.5.
  
# -------------------------------------------------------------------------------------------
# 4. Realizar un scatterplot y observar la relación entre x e y.
df <- data.frame("x" = x, "epsilon" = e, "y" = y)

g <- ggplot(data = df, aes(x= x, y= y)) +
  labs(title = "Modelo inicial (var = var0 = 0.025)") +
  geom_point(col = "gray", alpha= 0.9)

# -------------------------------------------------------------------------------------------
# 5. Ajustar un modelo lineal para predecir y en función de x utilizando el método de cuadrados
# minimos. Comparar los valores exactos de β0 y β1 con sus estimaciones.
X_lineal <- cbind(rep(1,100), x)
est_lineal <- cuadrados_minimos(X_lineal, y)

est_lineal

# -------------------------------------------------------------------------------------------
#6. Graficar la recta de cuadrados minimos sobre el gráfico realizado en (d). En otro color graficar
# la recta Y = −1 + 0.5X.
g <- g +
  geom_abline(intercept = est_lineal[1], slope = est_lineal[2], col="firebrick") +
  geom_abline(intercept = -1, slope = 0.5, col="yellow")

# -------------------------------------------------------------------------------------------
# 7. Ajustar un modelo polinomial que prediga y usando x y x^2 . ¿Encuentra alguna evidencia de
# que el término cuadrático mejora el ajuste del modelo?
X_pol <- cbind(rep(1,100), x, x^2)
est_pol <- cuadrados_minimos(X_pol, y)

est_pol # Vemos que el termino cuadratico es practicamente 0, por lo que no mejora el ajuste.

plot(g)

# -------------------------------------------------------------------------------------------
# Estimamos la varianza:
modelo_lineal <- function(x) est_lineal[1] + est_lineal[2]*x

modelo_pol <- function(x) est_pol[1] + est_pol[2]*x + est_pol[3]*(x^2)
  
R_lineal = y - modelo_lineal(x)
est_varianza_lineal <- sum(R_lineal^2) /(100 - 2)

R_pol = y - modelo_pol(x)
est_varianza_pol <- sum(R_pol^2) /(100 - 3)

est_varianza_lineal
est_varianza_pol

# -------------------------------------------------------------------------------------------
# 8. Repetir los items (a) a (f) modificando los datos generados de manera que haya menos ruido
# en los datos. Una forma de hacerlo es disminuyendo el valor de la varianza de la distribución
# normal usada para general el término del error epsilon.

x <- rnorm(100, 0, sqrt(1))
e <- rnorm(100, 0, sqrt(0.0025))
y <- -1 + 0.5*x + e
df <- data.frame("x" = x, "epsilon" = e, "y" = y)


X_lineal <- cbind(rep(1,100), x)
est_lineal <- cuadrados_minimos(X_lineal, y)
est_lineal
modelo_lineal <- function(x) est_lineal[1] + est_lineal[2]*x

X_pol <- cbind(rep(1,100), x, x^2)
est_pol <- cuadrados_minimos(X_pol, y)
est_pol
modelo_pol <- function(x) est_pol[1] + est_pol[2]*x + est_pol[3]*(x^2)

g <- ggplot(data = df, aes(x= x, y= y)) +
  labs(title = "Modelo con menos ruido (var = var0/10)") + 
  geom_point(col = "gray", alpha= 0.9) +
  geom_abline(intercept = est_lineal[1], slope = est_lineal[2], col="firebrick") +
  geom_abline(intercept = -1, slope = 0.5, col="yellow")
plot(g)

# Estimamos la varianza:

R_lineal = y - modelo_lineal(x)
est_varianza_lineal <- sum(R_lineal^2) /(100 - 2)

R_pol = y - modelo_pol(x)
est_varianza_pol <- sum(R_pol^2) /(100 - 3)

est_varianza_lineal
est_varianza_pol

# -------------------------------------------------------------------------------------------
# 9. Repetir los ı́tems (a) a (f) modificando los datos generados de manera que haya más ruido
# en los datos. Una forma de hacerlo es aumentando el valor de la varianza de la distribución
# normal usada para general el término del error epsilon.

x <- rnorm(100, 0, sqrt(1))
e <- rnorm(100, 0, sqrt(0.25))

y <- -1 + 0.5*x + e
df <- data.frame("x" = x, "epsilon" = e, "y" = y)


X_lineal <- cbind(rep(1,100), x)
est_lineal <- cuadrados_minimos(X_lineal, y)
est_lineal
modelo_lineal <- function(x) est_lineal[1] + est_lineal[2]*x

X_pol <- cbind(rep(1,100), x, x^2)
est_pol <- cuadrados_minimos(X_pol, y)
est_pol
modelo_pol <- function(x) est_pol[1] + est_pol[2]*x + est_pol[3]*(x^2)

g <- ggplot(data = df, aes(x= x, y= y)) +
  labs(title = "Modelo con mas ruido (var = var0*10)") + 
  geom_point(col = "gray", alpha= 0.9) +
  geom_abline(intercept = est_lineal[1], slope = est_lineal[2], col="firebrick") +
  geom_abline(intercept = -1, slope = 0.5, col="yellow")
plot(g)

# Estimamos la varianza:

R_lineal = y - modelo_lineal(x)
est_varianza_lineal <- sum(R_lineal^2) /(100 - 2)

R_pol = y - modelo_pol(x)
est_varianza_pol <- sum(R_pol^2) /(100 - 3)

est_varianza_lineal
est_varianza_pol

# -------------------------------------------------------------------------------------------
#10. En ambos escenarios, hallar un estimador de la varianza.

# Se incluyen las varianzas en los puntos anteriores.
# -------------------------------------------------------------------------------------------