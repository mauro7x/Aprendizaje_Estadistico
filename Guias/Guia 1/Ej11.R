# -------------------------------------------------------------------------------------------
# 11. Implemente una función que dado un vector y de valores de respuesta y una matriz X de
# valores observados, mediante las ecuaciones normales, calcule el estimador de cuadrados
# minimos β sombrero.
# -------------------------------------------------------------------------------------------

cuadrados_minimos <- function(X, y) {
  estimador <- solve(t(X) %*% X) %*% t(X) %*% y
  return(estimador)
}