# -------------------------------------------------------------------------------------------
# Scratch pad
# -------------------------------------------------------------------------------------------

# setup de librerias
library(ggplot2)

# funciones a utilizar

cuadrados_minimos <- function(X, y) {
  estimador <- solve(t(X) %*% X) %*% t(X) %*% y
  return(estimador)
}

# -------------------------------------------------------------------------------------------
# TESTEANDO CORRELACION

x1 <- runif(1000)

#x2 <- 3 + x1*2 # correlacion 1
#x2 <- runif(1000) # correlacion 0
x2 <- 3 + x1*2 + rnorm(1000, 0, sqrt(0.025)) # correlacion tiende a 1 cuando el ruido tiende a 0x2 <- 3 + x1*2

df <- data.frame("x1" = x1, "x2" = x2)

g <- ggplot(data = df, aes(x= x1, y= x2)) +
  geom_point(col="firebrick3", alpha = 0.5, shape = 21, fill = "firebrick2")
plot(g)

cor(x1, x2)

# -------------------------------------------------------------------------------------------
