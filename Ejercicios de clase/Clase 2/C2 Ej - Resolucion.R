# Setup
library(ggplot2)

# Loading data
autos <- cars
y <- autos$dist
x <- autos$speed

# Jugando con ggplot

g <- ggplot(data=autos, aes(x=speed, y=dist)) + 
  geom_point(col="steelblue", size=1.4) +
  geom_smooth(method='lm', col="firebrick") +
  coord_cartesian(xlim=c(4,25), ylim=c(0,150)) +
  labs(title="Distancia de frenado vs. Velocidad", subtitle="Ejercicio clase 2", y="Distancia", x="Velocidad") +
  scale_x_continuous(breaks=seq(4, 25, 1)) +
  scale_y_continuous(breaks=seq(0, 150, 15))
#plot(g)


# 2. Grafico de dispersion

g <- ggplot(data=autos, aes(x=speed, y=dist)) + 
  geom_point(col="steelblue", size=1.4) +
  coord_cartesian(xlim=c(4,25), ylim=c(0,150)) +
  labs(title="Distancia de frenado vs. Velocidad", subtitle="Ejercicio clase 2", y="Distancia", x="Velocidad") +
  scale_x_continuous(breaks=seq(4, 25, 1)) +
  scale_y_continuous(breaks=seq(0, 150, 15))


# 3. Estimar la media y el desvio estandar

x_mean = mean(x)
x_std = sd(x)
y_mean = mean(y)
y_std = sd(y)


# 4. Estimador de minimos cuadrados

X = cbind(rep(1, 50), x)
B_sombrero = (solve(t(X) %*% X)) %*% (t(X) %*% y)

g <- g +
  geom_abline(slope = B_sombrero[2], intercept = B_sombrero[1], col="darkgray")


# 5. Superponer sobre el gráfico anterior, en color naranja, los puntos correspondientes a los valores predichos.

predecir <- function(px, p_intercept, p_slope) {
  return(p_intercept + px*p_slope)
}

x_p = predecir(x, B_sombrero[1], B_sombrero[2])

g <- g +
  geom_point(aes(x=x, y=x_p), col="orange")



# 6. ¿Cuánto vale el estimador de σ^2?

s2 <- sum((y-x_p)^2) / (50 - 2)
s2


# 7. Estime la matriz de covarianza de los estimadores obtenidos. ¿Cuánto vale en este caso la matriz X' X?

M_cov = s2*solve(t(X) %*% X)
M_cov

XtX = t(X) %*% X
XtX

# 8. Verifique que sum{n}{i=1} (Yi − Ŷi ) = 0.
R = y - x_p
sum(R) <= 0.0000001 #no sera exactamente cero porque se utilizan numeros en representacion flotante


# 9. Centre las observaciones Xi’s y recalcule los estimadores de los parámetros.
# ¿Cambia el estimador de σ^2?
# Recalcule la estimación de la matriz de covarianza de los estimadores y compárela con la obtenida en (6).

x_centradas = x - mean(x) # centramos

X_c = cbind(rep(1, 50), x_centradas) # recalculamos estimadores
B_sombrero_c = (solve(t(X_c) %*% X_c)) %*% (t(X_c) %*% y)

x_c_p = predecir(x_centradas, B_sombrero_c[1], B_sombrero_c[2])
s2_c <- sum((y-x_c_p)^2) / (50 - 2)
(s2 - s2_c) == 0 # NO CAMBIA el estimador

M_cov_c = s2_c*solve(t(X_c) %*% X_c)

M_cov_c # matriz de covarianza de estimadores centrando observaciones
M_cov # matriz de covarianza de estimadores sin centrar


# 10. Ajustar un modelo polinomial que prediga y usando x y x^2
# ¿Encuentra alguna evidencia de que el término cuadrático mejora el ajuste del modelo?
# Graficar la curva obtenida sobre el gráfico realizado en (2).

x2 = x^2
X_pol = cbind(rep(1, 50), x, x2)
B_sombrero_pol = (solve(t(X_pol) %*% X_pol)) %*% (t(X_pol) %*% y)

predecir_pol <- function(px, estimadores) {
  return(estimadores[1] + px*estimadores[2] + (px^2)*estimadores[3])
}


x_p_pol = predecir_pol(x, B_sombrero_pol)
R_pol = y - x_p_pol

sum(R^2)
sum(R_pol^2)

# Se puede ver que hay menor error en el modelo cuadratico, sin embargo no quiere decir que sea mejor.

f_pol <- function(x) B_sombrero_pol[1] + B_sombrero_pol[2]*x + B_sombrero_pol[3]*(x^2)

g <- g +
  stat_function(fun=f_pol, colour="gray", n = 10000) +
  geom_point(aes(x=x, y=x_p_pol), col="brown")
plot(g)












