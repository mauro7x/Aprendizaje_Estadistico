# -------------------------------------------------------------------------------------------
# 10. El archivo graduados.txt contiene los promedios obtenidos en su carrera de grado de 30
# inscriptos en el programa de postgrado del Departamento de Ingenieria Industrial de la Universidad
# de Berkeley.
# -------------------------------------------------------------------------------------------

# setup de librerias
library(ggplot2)

# cargamos los datos
ruta <- "/home/mp/Facultad/1C 2020/Aprendizaje Estadístico/Guias/Guia 1/"
ruta_archivo <- paste(ruta, sep="", "graduados.txt")
datos <- read.table(ruta_archivo)
x <- datos$V1

# -------------------------------------------------------------------------------------------

# a. Calcular la media, la mediana muestral y la media 10 %-podada.
media <- mean(x)
mediana_muestral <- median(x)
media10podada <- mean(x, trim = 0.1)

# b. Calcular el desvio estándar, la distancia intercuartil y la MAD.
desvio_estandar <- sd(x)
distancia_intercuartil <- IQR(x)
median_absolute_deviation <- mad(x)

# c. Realizar un boxplot sobre este conjunto de datos.

boxplot <- ggplot(datos, aes(x=V1)) + 
  labs(title="Boxplot", subtitle = "Notas de graduados") +
  geom_boxplot(color="coral4", fill="coral2", 
               outlier.colour="coral3", outlier.shape=20,
               outlier.size=1)
plot(boxplot)

# ¿Cuáles son las caracteristicas mas sobresalientes?
# ???

# ¿Cómo relaciona lo observado en el boxplot con lo obtenido en los incisos anteriores?
# ??? Se ve la mediana, la distancia intercuartil, etc.

# d. ¿Es razonable suponer normalidad de los datos?
# ¿Con qué gráfico o herramienta lo podrá verificar?

# Para verificarlo, podemos graficar un histograma y ver si su distribucion se parece a una normal.

hist <- ggplot(datos, aes(x=V1)) + 
  labs(title="Histograma", subtitle = "Queremos ver si podemos suponer normalidad") +
  geom_histogram(color="coral4", fill="coral2", bins = 20)
plot(hist)

# Yo diria que para la poca cantidad de datos que tenemos, se parece bastante a una normal.

