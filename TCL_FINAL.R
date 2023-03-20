#Jose Francisco Rivero Gonzalez
#Prueba 1 del taller 1 pregunta 5

#Primero probaremos si el teorema central del límite (TCL) se cumple para las 
#siguiente distribuciones N(-2,3), x^2(15), Exp(3),U(-5,8)

#colocar la libreria

#crear variable que tenga 10,000 observaciones, con media -2 y desv s 3

#primero vamos a realizar un histograma de la distribución de la distribucion normal
#después de eso vamos a sacar la media
 
#lo que nos permite ver esto es que efectivamente tenemos una distribución normal con media -2 
#y le sacamos la media para ver si se acerca al -2. Lo que haremos ahora
#es repetir este ejercicio 1,10,100,1000 y ver si la media de las medias se asemeja
#a -2.

med1 <-c()
med10 <-c()
med100 <-c()
med1000 <-c()
for (i in 1:1) {
  randNorm <- rnorm(10000,-2,3)
  med1[i] <- mean(randNorm)
  med1[i]
}

for (i in 1:10) {
  randNorm <- rnorm(10000,-2,3)
  med10[i] <- mean(randNorm)
  med10[i]
}
for (i in 1:100) {
  randNorm <- rnorm(10000,-2,3)
  med100[i] <- mean(randNorm)
  med100[i]
}
for (i in 1:1000) {
  randNorm <- rnorm(10000,-2,3)
  med1000[i] <- mean(randNorm)
  med1000[i]
}
#aqui lo que ponemos es la función de densidad para las gráficas de los 
#histogramas puestos en una misma gráfica para ver cómo cambia la distribución 
#de las medias de las distribuciones y vemos que se acerca la media
#a-2 y se reduce la desviación conforme se aumenta el número de repeticiones. 
# Graficar los histogramas de las medias muestrales
par(mfrow = c(2,2))
hist(med1,
     main = paste("Histograma de distribución normal"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med10,
     main = paste("Histograma de distribución normal"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med100,
     main = paste("Histograma de distribución normal"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med1000,
     main = paste("Histograma de distribución normal"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 





###
###
#hist(runif(1000,-5,8)) #lo que nos permite ver es cómo se distribuye la función uniforme

med1 <-c()
med10 <-c()
med100 <-c()
med1000 <-c()
for (i in 1:1) {
  randUnif <- runif(10000,-5,8)
  med1[i] <- mean(randUnif)
  med1[i]
}
for (i in 1:10) {
  randUnif <- runif(10000,-5,8)
  med10[i] <- mean(randUnif)
  med10[i]
}
for (i in 1:100) {
  randUnif <- runif(10000,-5,8)
  med100[i] <- mean(randUnif)
  med100[i]
}
for (i in 1:1000) {
  randUnif <- runif(10000,-5,8)
  med1000[i] <- mean(randUnif)
  med1000[i]
}
par(mfrow = c(2,2))
hist(med1,
     main = paste("Histograma de distribución Uniforme"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med10,
     main = paste("Histograma de distribución Uniforme"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med100,
     main = paste("Histograma de distribución Uniforme"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med1000,
     main = paste("Histograma de distribución Uniforme"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 

#lo que queremos ver es si la media se acerca a la media de la distribucion
#unifrme con -5,8 y en este caso vemos que se disminuye la varianza y 
#la media sí se acerca, nuevamente, a la media de la distribución uniforme.
runif(1000,-5,8)
mean(runif(1000,-5,8))

###
###

med1 <-c()
med10 <-c()
med100 <-c()
med1000 <-c()
for (i in 1:1) {
  randExp <- rexp(10000,3)
  med1[i] <- mean(randExp)
  med1[i]
}
for (i in 1:10) {
  randExp <- rexp(10000,3)
  med10[i] <- mean(randExp)
  med10[i]
}
for (i in 1:100) {
  randExp <- rexp(10000,3)
  med100[i] <- mean(randExp)
  med100[i]
}
for (i in 1:1000) {
  randExp <- rexp(10000,3)
  med1000[i] <- mean(randExp)
  med1000[i]
}
par(mfrow = c(2,2))
hist(med1,
     main = paste("Histograma de distribución Exponencial"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med10,
     main = paste("Histograma de distribución Exponencial"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med100,
     main = paste("Histograma de distribución Exponencial"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med1000,
     main = paste("Histograma de distribución Exponencial"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 



#lo que queremos ver es si la media se acerca a la media de la distribucion
#exponencial con 10000 y 3 de media y en este caso vemos que se disminuye la varianza y 
#la media sí se acerca, nuevamente, a la media de la distribución exponencial.
#rexp(1000,3)
#mean(rexp(1000,3))

###
###

med1 <-c()
med10 <-c()
med100 <-c()
med1000 <-c()
for (i in 1:1) {
  randXi <- rchisq(10000,15)
  med1[i] <- mean(randXi)
  med1[i]
}
for (i in 1:10) {
  randXi <- rchisq(10000,15)
  med10[i] <- mean(randXi)
  med10[i]
}
for (i in 1:100) {
  randXi <- rchisq(10000,15)
  med100[i] <- mean(randXi)
  med100[i]
}
for (i in 1:1000) {
  randXi <- rchisq(10000,15)
  med1000[i] <- mean(randXi)
  med1000[i]
}
par(mfrow = c(3,3))
hist(med1,
     main = paste("Histograma de distribución Xi cuadrada"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med10,
     main = paste("Histograma de distribución Xi cuadrada"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med100,
     main = paste("Histograma de distribución Xi cuadrada"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 
hist(med1000,
     main = paste("Histograma de distribución Xi cuadrada"),
     xlab = paste("Valores de la distribución"),
     ylab = paste("Frecuencia")) 


#lo que queremos ver es si la media se acerca a la media de la distribucion
#Xi cuadrada con 10000 observaciones y 15 grados de libertad
#la media sí se acerca, nuevamente, a la media de la distribución Xi cuadrada (15).
rchisq(10000,15)
mean(rchisq(10000,15))


