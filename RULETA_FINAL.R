#Jose Francisco Rivero Gonzalez
#programa del casino
#20/01/2022


#estrategias, tiene distintas estrategias para poder ganar dependiendo
#de la ruleta

## esto solo sirve para la estrategia 1
apuesta<-10000 #el monto de dinero que vas a apostar
veces<-10 #el número de veces que piensas jugar

#estrategia 1: apostar a un numero de los 36 posibles y te pagan 35
#lo que apostaste, 

dinero<-apuesta/veces #el dinero que vas a apostar depende de cuanto traes y las veces que quieres jugar
ganancia<-0 #es para tener una ganancia de cero al princpio

promedio<-c() #para sacar el promedio de ganancias por cada vez que juegas

  for (n in 1:1000) {
  #cuánto dinero tiene?
  apuesta <- 10000
  #veces = el numero de veces que usted puede apostar o jugar
  veces<-100
    for (i in 1:veces) {
    a <-sample(0:36,1)
    b <-sample(0:36,1)
    ifelse (a == b, ganancia[i]<-dinero*35,
            ganancia[i]<- -(dinero))
      
    }
    promedio<-c(promedio,mean(ganancia))
  }
df1 <- data.frame(promedio)
promedio_medio1 <- mean(df1$promedio) #es el promedio de los promedios de ganancias

ggplot(df1, aes(x = promedio))+
  geom_histogram(bins = 20, fill = "gray", color = "white") +
  ggtitle(paste("Histograma de ganancias promedio (", n, "simulaciones)")) +
  xlab("Ganancia promedio") + ylab("Frecuencia") +
  geom_vline(xintercept = promedio_medio1, color = "red", linetype = "dashed")

##estrategia 2: apostar a los pares o impares,
dev.off()
#según un juego de ruleta, la ruleta tiene 36 casillas en las que la pelota
#puede llegar a caer.
negros <-c(2,4,6,8,10,11,13,15,17,20,22,24,26,28,31,33,35)
rojos <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,29,30,32,34,36)
verdes <- 0
## esto solo sirve para la estrategia 1
apuesta<-10000
veces<-10
dinero<-apuesta/veces
ganancia<-0
promedio<-c()
for (n in 1:1000) {
  #cuánto dinero tiene?
  apuesta <- 10000
  #veces = el numero de veces que usted puede apostar o jugar
  veces<-10
  for (i in 1:veces) {
    a <-sample(0:36,1)
    ifelse (a %% 2, ganancia[i]<-dinero*2,
            ganancia[i]<- -(dinero))
    
  }
  
  promedio<-c(promedio,mean(ganancia))
}
df2 <- data.frame(promedio)
promedio_medio2 <- mean(df2$promedio)
ggplot(df2, aes(x = promedio))+
  geom_histogram(bins = 20, fill = "gray", color = "white") +
  ggtitle(paste("Histograma de ganancias promedio (", n, "simulaciones)")) +
  xlab("Ganancia promedio") + ylab("Frecuencia") +
  geom_vline(xintercept = promedio_medio2, color = "red", linetype = "dashed")

#estrategia 3: apostar a los negro o rojos.
dev.off()
#según un juego de ruleta, la ruleta tiene 36 casillas en las que la pelota
#puede llegar a caer.
negros <-c(2,4,6,8,10,11,13,15,17,20,22,24,26,28,31,33,35)
rojos <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,29,30,32,34,36)
verdes <- 0
## esto solo sirve para la estrategia 1
apuesta<-10000
veces<-10
dinero<-apuesta/veces
ganancia<-0

promedio<-c()

for (n in 1:10000) {
  #cuánto dinero tiene?
  apuesta <- 10000
  #veces = el numero de veces que usted puede apostar o jugar
  veces<-10
  for (i in 1:veces) {
    a <-sample(0:36,1)
    ifelse (a %in% c(negros), ganancia[i]<-dinero*2,
            ganancia[i]<- -(dinero))
    
  }
  
  promedio<-c(promedio,mean(ganancia))
}
df3 <- data.frame(promedio)
promedio_medio3 <- mean(df3$promedio)
ggplot(df3, aes(x = promedio))+
  geom_histogram(bins = 20, fill = "gray", color = "white") +
  ggtitle(paste("Histograma de ganancias promedio (", n, "simulaciones)")) +
  xlab("Ganancia promedio") + ylab("Frecuencia") +
  geom_vline(xintercept = promedio_medio3, color = "red", linetype = "dashed")

