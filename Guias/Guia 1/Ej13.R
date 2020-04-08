# -------------------------------------------------------------------------------------------
# 13. Considere el archivo abalone.txt que contiene información sobre distintas muestras de
# abalones. Los atributos están separados por coma, con los siguientes campos:

# Sexo (categorica): M (masculino), F (femenino) o I (infante).
# Longitud (continua), en milimetros.
# Diametro (continua), en milimetros.
# Altura (continua), en milimetros.
# Peso completo del abalone (continua), en gramos.
# Peso de la carne (continua), en gramos.
# Peso de las visceras (continua), en gramos.
# Peso del caparazon (continua), en gramos.
# Anillos (entera).
# -------------------------------------------------------------------------------------------

# setup de librerias
library(ggplot2)

# cargamos los datos
ruta <- "/home/mp/Facultad/1C 2020/Aprendizaje Estadístico/Guias/Guia 1/"
ruta_archivo <- paste(ruta, sep="", "abalone.txt")
datos <- read.table(ruta_archivo, sep = ",", col.names = c("sexo", "longitud", "diametro", "altura",
                                                           "peso_abalone", "peso_carne",
                                                           "peso_visceras", "peso_caparazon", "anillos"))
n_obs <- length(datos$sexo)

# funciones a utilizar

cuadrados_minimos <- function(X, y) {
  estimador <- solve(t(X) %*% X) %*% t(X) %*% y
  return(estimador)
}

# -------------------------------------------------------------------------------------------
# a. Plantear un modelo de regresión lineal simple para predecir el diámetro en función de la
# longitud.

X_a <- cbind(rep(1, n_obs), datos$longitud)
y_a <- datos$diametro
est_a <- cuadrados_minimos(X_a,y_a)

modelo_a <- function(x) {
  return(est_a[1] + est_a[2]*x)
}

g <- ggplot(data = datos, aes(x=longitud, y=diametro)) +
  geom_point(color = "dodgerblue", alpha = 0.2) +
  labs(title="Diametro en funcion de la longitud", x="Longitud", y="Diametro") +
  geom_abline(intercept = est_a[1], slope = est_a[2], col = "dodgerblue4")
plot(g)


# b. Observe que el conjunto de datos tiene información del peso total de cada espécimen junto
# con un desagregado por partes. Ajustar un modelo de regresión múltiple que explique el peso
# total en función del peso del caparazón, las visceras y la carne.

X_b <- cbind(rep(1, n_obs), datos$peso_caparazon, datos$peso_visceras, datos$peso_carne)
y_b <- datos$peso_abalone
est_b <- cuadrados_minimos(X_b, y_b)

modelo_b <- function(p_caparazon, p_visceras, p_carne) {
  return(est_b[1] + est_b[2]*p_caparazon + est_b[3]*p_visceras + est_b[4]*p_carne)
}


# c. Se trata ahora de establecer una relación entre el peso total y el diámetro del espécimen.
# Empezar dibujando en un scatter plot ambas variables. 

g <- ggplot(data = datos, aes(x=diametro, y=peso_abalone)) +
  labs(title="Peso total en funcion del diametro", x="Diametro", y="Peso total") +
  geom_point(col="gray10", alpha=0.1)

# Si definimos como P al peso total y D al diámetro, se consideran los siguientes modelos:

P <- datos$peso_abalone
D <- datos$diametro

    # Modelo lineal simple, P = b + a*D + e.
X_lineal <- cbind(rep(1, n_obs), D)
est_lineal <- cuadrados_minimos(X_lineal, P)

modelo_lineal <- function(d) {
  return(est_lineal[1] + est_lineal[2]*d)
}

g <- g + stat_function(fun = modelo_lineal, color="dodgerblue1")

    # Modelo cuadrático, P = c + b*D + a*D^2 + e.
X_cuad <- cbind(rep(1, n_obs), D, D^2)
est_cuad <- cuadrados_minimos(X_cuad, P)

modelo_cuad <- function(d) {
  return(est_cuad[1] + est_cuad[2]*d + est_cuad[3]*d^2)
}

g <- g + stat_function(fun = modelo_cuad, color="firebrick4")

    # Modelo cubico sin términos de orden inferior, P = a*D^3 + e.
X_cub <- cbind(D^3)
est_cub <- cuadrados_minimos(X_cub, P)

modelo_cub <- function(d) {
  return(est_cub[1]*d^3)
}

g <- g + stat_function(fun = modelo_cub, color="goldenrod1")

# Efectuar en cada caso una regresión y graficar lascurvas superpuestas sobre el scatter plot.

plot(g)
