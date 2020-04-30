# -------------------------------------------------------------------------------------------------
# 7. Los datos del archivo cemento.txt fueron tomados en un estudio experimental para relacionar
# el calor generado (Y) al fraguar 14 muestras de cemento con distinta composición. Las variables
# explicativas son los pesos (medidos en porcentajes del peso de cada muestra de cemento) de 5
# componentes del cemento.
# -------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------
# SETUP DE LIBRERIAS Y DATOS
library(ggplot2)

# cargamos los datos
# los leo de un txt porque no sé como leerlos del xls, habría que ver esto
df <- read.table(file = "cemento.txt", sep = ",", header = TRUE)
df <- df[,-1]

# funciones
cuadrados_minimos <- function(X, y) {
  estimador <- solve(t(X) %*% X) %*% t(X) %*% y
  return(estimador)
}

# -------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------
# 7.1. Calcule la matriz de correlación de todas las variables comprendidas en el problema,
# incluyendo a la variable dependiente Y.
matriz_correlacion <- cor(df)

# Inspeccionando esta matriz determine cuáles parecen ser las variables que pueden contribuir
# significativamente a explicar la variación de Y.

  # RTA: En órden de importancia: x3, x4, x2, y un poco menos pero también importante, x1. Por último
  # x5, pero aporta mucho menos.

# -------------------------------------------------------------------------------------------------
# 7.2. Use Y como variable dependiente y todas las variables independientes y una intercept para
# realizar un ajuste lineal.
ajuste <- lm(y ~ x1 + x2 + x3 + x4+ x5, data = df)
summary(ajuste)

# Calcule el estimador de mínimos cuadrados de los parámetros y para cada uno de ellos testee la
# hipótesis de que es 0.

  # RTA: El estimador de mínimos cuadrados se puede ver en ajuste$coefficients:
ajuste$coefficients
  
  # También en ajuste tenemos el test de hipótesis individual de que sean 0.

# ¿Cuáles son significativamente distintos de 0? ¿es la regresión significativa? ¿Observa alguna
# contradicción con el resultado obtenido en los tests individuales anteriores? ¿Vale la pena hacer
# un nuevo intento para seleccionar qué variables entran en la regresión?

  # RTA: la variable con menor p_valor es x2, que tiene un p_v de 0.258, que sigue siendo muy grande.
  # Esto quiere decir que podemos rechazar a un nivel de significación de 74% que el coeficiente
  # que acompaña a x2 en el modelo NO ES CERO. Como vemos, es un nivel de significación muy bajo, por
  # lo que parecería ser que ninguna es significativamente distinta de 0.
  # 
  # La regresión testeando que todos son 0 contra que alguno es distinto de 0 resulta ser significativa,
  # pues el p-valor da practicamente 0. Se observa una contradicción pues esto nos dice a un nivel de significación
  # muy grande que al menos un coeficiente no es 0. Yo creo que sí, debería hacer un ajuste seleccionando
  # las variables que entran en la regresion.

# -------------------------------------------------------------------------------------------------
# 7.3. Calcule la suma de las 5 variables independientes. 

df$sum <- df$x1 + df$x2 + df$x3 + df$x4 + df$x5

# ¿Qué observa? ¿Cómo se justifica este parecido entre los totales?

  # RTA: Vemos que todas las sumas de las 5 variables independientes suman alrededor de 100. Esto
  # puede deberse a que representen porcentajes sobre el total del peso del cemento.
  
# A partir de este resultado, ¿le parece que la matriz de diseño puede estar bien condicionada?
# ¿Puede justificar ésto que eliminemos del modelo la intercept?

  #ver esto

# -------------------------------------------------------------------------------------------------
# 7.4. Realice un nuevo ajuste lineal usando las 5 variables independientes y eliminando la intercept.

ajuste2 <- lm(y ~ 0 + x1 + x2 + x3 + x4+ x5, data = df)
summary(ajuste2)


# ¿Cómo se comparan estos resultados con los obtenidos anteriormente? ¿Cuáles son
# significativamente distintos de 0?

  # RTA: En estos resultados vemos que hay 3 variables significativamente distintas de 0, una que
  # es distinta de 0 con un nivel de significación del 90%, y otra que parece ser 0.

# Plantee un nuevo modelo en el que intervengan aquellas variables independientes que contribuyen
# significativamente y estime los parámetros por mínimos cuadrados.

ajuste3 <- lm(y ~ 0 + x2 + x3 + x4, data = df)
summary(ajuste3)

# ¿Qué modelo eligiría finalmente?

  # ver esto

# -------------------------------------------------------------------------------------------------
