# -------------------------------------------------------------------------------------------
# 12. Se tiene en el archivo girasol.txt el rinde de diversas parcelas de girasol (en toneladas)
# según la cantidad de dinero invertida en fertilizantes (en miles de pesos).
# -------------------------------------------------------------------------------------------

# setup de librerias
library(ggplot2)

# cargamos los datos
ruta <- "/home/mp/Facultad/1C 2020/Aprendizaje Estadístico/Guias/Guia 1/"
ruta_archivo <- paste(ruta, sep="", "girasol.txt")
datos <- read.delim(ruta_archivo, sep = " ")

x <- datos$inversion
y <- datos$rinde

# -------------------------------------------------------------------------------------------
# a. Graficar en un diagrama de dispersión inversión vs rinde.

g <- ggplot(data = datos, aes(x=inversion, y=rinde)) +
  geom_point(col="steelblue", size=1.4) +
  labs(title = "Grafico de dispersion", subtitle = "Inversion vs Rinde",
       x="Inversion [miles de pesos]", y="Rinde [toneladas]")  


# b. Plantear un modelo de regresión lineal simple, obtener el estimador de minimos cuadrados.

cuadrados_minimos <- function(X, y) {
  estimador <- solve(t(X) %*% X) %*% t(X) %*% y
  return(estimador)
}

X <- cbind(rep(1,50), x)
B_sombrero <- cuadrados_minimos(X,y)
estimar <- function(x) B_sombrero[1] + B_sombrero[2]*x


# c. Graficar la recta de regresion obtenida, ¿detecta algo sospechoso?

g <- g +
  geom_abline(intercept = B_sombrero[1], slope = B_sombrero[2], col="coral")
plot(g)

# Si, se observa que esta bajada, y se debe a los outliers. Datos sin sentido.
