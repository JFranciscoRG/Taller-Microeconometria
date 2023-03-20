##Jose Francisco Rivero Gonzalez
##09/02/2023

#Pregunta GINI Taller 1
#encontrar el archivo que quiero que me lea
#leer un archivo lif
rm(list = ls())
library(readr)
#install.packages("ineq")
library(ineq)

datos<-read.delim("Data Storage/Raw/RUIDO_FINAL_PF_2015.tab")

#después de calcular la variable adecuada tenemos que sacar el coeficiente de GINI
#Tengo que crear una visualización que me ayude a expresar cómo funciona ese coeficiente.

#1

#lo que primero haremos será incluir la libreria de ineq
#ahora tenemos que armar una variable que contenga los ingresos
#de todos los individuos

incomes <- c(datos$I_DEC_IUACNTC1_AA)
gini <-Gini(incomes)
#en caso de que quieras ver la gráfica de la curva de Lorenz que es
#la que sirve para ver la desigualdad de ingresos.


#lorenz<- Lc(ingresos)
#plot(lorenz)


#2
#aqui podemos ver un histograma con los ingresos lo que podemos hacer es ver 
#distintos histogramas dependiendo el decil de los ingresos. 
#entonces lo haremos de la siguiente manera:

#lo que pas es que es medio ineficiente hacerlo de esa manera
deciles<- quantile(incomes, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm = TRUE)
#importante si usted pone na.rm = TRUE lo que te dice es que
#cualquier valor que tenga na o parecido lo descarta
#asi puede sumar todos los valores.
group1 <- incomes[incomes <= deciles[2]]
group2 <- incomes[incomes > deciles[2] & incomes <= deciles[3]]
group3 <- incomes[incomes > deciles[3] & incomes <= deciles[4]]
group4 <- incomes[incomes > deciles[4] & incomes <= deciles[5]]
group5 <- incomes[incomes > deciles[5] & incomes <= deciles[6]]
group6 <- incomes[incomes > deciles[6] & incomes <= deciles[7]]
group7 <- incomes[incomes > deciles[7] & incomes <= deciles[8]]
group8 <- incomes[incomes > deciles[8] & incomes <= deciles[9]]
group9 <- incomes[incomes > deciles[9] & incomes <= deciles[10]]
group10 <- incomes[incomes > deciles[10]]
library(gridExtra)
library(ggplot2)
par(mfrow=c(3,4))

hist(incomes)
hist(group1, main = "Histograma del decil 1")
hist(group2, main = "Histograma del decil 2")
hist(group3, main = "Histograma del decil 3")
hist(group4, main = "Histograma del decil 4")
hist(group5, main = "Histograma del decil 5")
hist(group6, main = "Histograma del decil 6")
hist(group7, main = "Histograma del decil 7")
hist(group8, main = "Histograma del decil 8")
hist(group9, main = "Histograma del decil 9")
hist(group10, main = "Histograma del decil 10")

#3

#qué porcentaje del ingreso total (de la suma de todos los ingresos)
#se queda el 0.01 de la población 

#total es el total de ingresos fiscales en 2015
total<-sum(incomes, na.rm = TRUE)
#tenemos que poner los cuantiles de la población en este caso son 4
q1 <- quantile(incomes, 0.999, na.rm = TRUE)
q2 <- quantile(incomes, 0.99, na.rm = TRUE)
q3 <- quantile(incomes, 0.95, na.rm = TRUE)
q4 <- quantile(incomes, 0.9, na.rm = TRUE)

#después tenemos que ponder los ingresos acumulados que tiene supongamos
#el 10% de la población
income_accumulated_q1 <- sum(incomes[incomes >= q1], na.rm = TRUE)
income_accumulated_q2 <- sum(incomes[incomes >= q2], na.rm = TRUE)
income_accumulated_q3 <- sum(incomes[incomes >= q3], na.rm = TRUE)
income_accumulated_q4 <- sum(incomes[incomes >= q4], na.rm = TRUE)


# Calcular los porcentajes acumulados para cada cuantil
income_percentages <- c(income_accumulated_q1, income_accumulated_q2, income_accumulated_q3, income_accumulated_q4) / sum(incomes, na.rm = TRUE)

# Crear un data frame con los porcentajes y los cuantiles correspondientes
df <- data.frame(percentages = income_percentages, quantiles = c("0.1%", "1%", "5%", "10%"))

# Crear el gráfico utilizando ggplot2
ggplot(df, aes(x = quantiles, y = percentages)) + 
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Porcentaje de ingresos acumulados por cuantil", x = "Cuantil", y = "Porcentaje acumulado") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()
