# -------------------------------------------------------------------------------------------
# 2. Data: mtcars, en R (no requiere ningun paquete adicional)
# Del set de datos trabajaremos con las variables:
  
  #Y = mpg (millas por galon)
  #X1 = disp (desplazamiento de los cilindros).
  #X2 = wt (peso del motor).
  #X3 = cyl (cantidad de cilindros).
  #X4 = vs (forma del motor).

# -------------------------------------------------------------------------------------------
# setup de librerias
library(ggplot2)
# library(GGally)

# -------------------------------------------------------------------------------------------
# setup de data
data <- mtcars

y <- data$mpg
x1 <- data$disp
x2 <- data$wt
x3 <- data$cyl
x4 <- data$vs

df <- data.frame(cbind(y, x1, x2, x3, x4))

# -------------------------------------------------------------------------------------------
# a) Consideremos s´olo las variables Y y X2 para poder verlo graficamente:
g <- ggplot(data = df, aes(x= x2, y= y)) +
  geom_point(col="steelblue")

# 1) Calcular, para cada punto del diseno, el intervalo de confianza de nivel 0.95 para la respuesta.

alpha <- 0.05
ajuste <- lm(y ~ x2)
summary(ajuste)


s <- summary(ajuste)$sigma
s
X <- model.matrix(ajuste)
n = nrow(X)
p = ncol(X)
A <- solve(t(X) %*% X)


IC <- matrix(0, nrow=n, ncol=2)

for (i in 1:n) {
  IC[i,]<-c(ajuste$fitted.values[i]-qt(0.975, n-p)*s*sqrt((t(X[i, ])) %*% A %*% X[i, ]),
            ajuste$fitted.values[i]+qt(0.975, n-p)*s*sqrt((t(X[i, ])) %*% A %*% X[i, ]) )
}

int<-predict(ajuste, interval = "confidence", level = 0.95) # Otra forma

# 2) Calcular, para cada punto del diseno, el intervalo de prediccion de nivel 0.95 para la respuesta.

ICP <- matrix(0, nrow=n, ncol=2)

for (i in 1:n) {
  ICP[i,]<-c(ajuste$fitted.values[i]-qt(0.975, n-p)*s*sqrt(1 + (t(X[i, ])) %*% A %*% X[i, ]),
            ajuste$fitted.values[i]+qt(0.975, n-p)*s*sqrt(1 + (t(X[i, ])) %*% A %*% X[i, ]) )
}


intP<-predict(ajuste, interval = "confidence", level = 0.95)

# 3) Realizar en un mismo grafico los pares de puntos (x,y), la recta de minimos cuadrados y los
# lımites de los intervalos obtenidos en a) y b) para cada punto del diseno.

# ver codigo jemi

# ----------
# 4) Calcular, para cada punto del diseno, el intervalo de confianza para la respuesta de manera
# que el nivel global de los 32 intervalos obtenidos sea 0.95.

BC <- matrix(0, nrow=n, ncol=2)

for (i in 1:n) {
  BC[i,]<-c(ajuste$fitted.values[i]-sqrt(p*qf(0.975, p, n-p))*s*sqrt((t(X[i, ])) %*% A %*% X[i, ]),
             ajuste$fitted.values[i]+sqrt(p*qf(0.975, p, n-p))*s*sqrt((t(X[i, ])) %*% A %*% X[i, ]) )
}
BC


# 5) Agregar al grafico obtenido en c las bandas de confianza calculadas en d.


plot(g)
