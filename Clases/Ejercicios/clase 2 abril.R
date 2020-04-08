datos <- cars

x <- datos$speed      #velocidad
y <- datos$dist       #distancia de frenado

# 2
plot(x,y, pch=20)     ## ggplot2, ver

# 3
x_raya <- mean(x)
s_x <- sd(x)

# 4
X <- matrix(c(rep(1, 50), x), byrow= FALSE, ncol=2)

# Otra forma de hacer esta matriz:
unos <- rep(1,50)
X_alt <- cbind(unos, x) #une columnas


X_t <- t(X)  # traspuesta de X
# X %*% X producto matricial
# solve(X) inversa

beta_sombrero <- ((solve(X_t %*% X)) %*% X_t) %*% y

beta_sombrero

# Graficamos

plot(x,y, pch=20)
lines(x, beta_sombrero[1] + beta_sombrero[2]*x,
      col = "chocolate")
#abline(beta_sombrero[1], beta_sombrero[2],
      #col="Forestgreen")



# ahora la version linda

ajuste <- lm(y~x)

names(ajuste)
summary(ajuste)

# ver ggplot, terminar el ejercicio





