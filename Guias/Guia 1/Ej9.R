# -------------------------------------------------------------------------------------------
# 9. La idea de este ejercicio es estudiar el comportamiento de las distintas gráficas ante diversas
# situaciones simuladas.
# -------------------------------------------------------------------------------------------

# setup librerias
library(ggplot2)

# -------------------------------------------------------------------------------------------

# 1. Generar, utilizando el comando rnorm(1000), 1000 elementos provenientes de una normal
# estandar, guarde los valores en el objeto x. 
# Realizar un histograma, un boxplot y un Q-Qnorm de x.

x <- data.frame(
  values=c(rnorm(1000))
)

graficar <- function(x) {
  hist <- ggplot(x, aes(x=values)) + 
    geom_histogram(binwidth=0.20, color="#00C083", fill="#00FFAE")
  plot(hist)
  
  boxplot <- ggplot(x, aes(x=values)) + 
    geom_boxplot(color="#00C083", fill="#00FFAE", 
                 outlier.colour="#00855B", outlier.shape=20,
                 outlier.size=1)
  plot(boxplot)
  
  qnorm <- ggplot(x, aes(sample=values)) + 
    stat_qq(color="#00855B")
  plot(qnorm)
}

graficar(x)


# 2. Repetir el analisis anterior para las siguientes situaciones:

# Binomial: rbinom(1000,10,0.4).
x <- data.frame(
  values=c(rbinom(1000,10,0.4))
)
graficar(x)

# Chi-cuadrado: rchisq(1000,50).
x <- data.frame(
  values=c(rchisq(1000,50))
)
graficar(x)

# F de Snedecor: rf(1000,90,40).
x <- data.frame(
  values=c(rf(1000,90,40))
)
graficar(x)

# Gamma: rgamma(1000,0.7)
x <- data.frame(
  values=c(rgamma(1000,0.7))
)
graficar(x)

