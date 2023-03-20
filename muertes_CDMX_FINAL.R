#Muertes CDMX Final
#José Francisco Rivero González
#Taller 1 microeconometría
rm(list = ls())
#Paquetes y librerías

library(tidyverse)
library(ggplot2)
library(lubridate)
#install.packages("zoo") #para el inciciso 2 vas a ocupar esta librería
library(zoo)
#Lectura de los datos
datos <- read.csv("Data Storage/Raw/actas-defuncion-registro-civil.csv")
fechas <- as.Date(datos$fec_defuncion, format = "%Y-%m-%d")

fecha_promedio<-as.Date("2020-03-23")
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2022-12-31")

subset_data <- subset(datos, fechas >= start_date & fechas <= end_date)

muertes_diarias<-c()
for (a in unique(fechas)) {
  muertes_diarias <- c(muertes_diarias, sum(a == fechas))
}

tabla <- cbind.data.frame(sort(unique(fechas)), muertes_diarias)

eventos_importantes <- c("2020-02-27", "2020-03-23","2020-12-24","2021-10-15")
fecha_eventos <- as.Date(eventos_importantes, format = "%Y-%m-%d")
muertes_eventos <- c(800, 700, 600, 500)
eventos_data <- data.frame(fecha_eventos, muertes_eventos)

ggplot(data = tabla, aes(x = sort(unique(fechas)), y = muertes_diarias)) + 
  geom_line() + 
  geom_vline(xintercept = fecha_eventos, color = c("red","blue","green","orange"), linetype=1) +
  geom_text(
    data = eventos_data,
    aes(x = fecha_eventos, y = muertes_eventos, label = eventos_importantes),
    hjust = 1.5,
    vjust = -0.5,
    color = "black",
    size = 3
  ) +
  xlab("Fechas") + 
  ylab("Muertes diarias") + 
  ggtitle("Muertes diarias por día")

####INCISO 2####
datos<-read_csv("Data Storage/Raw/actas-defuncion-registro-civil.csv")

filtrado <- datos %>% 
  select(fec_defuncion,num_consecutivo)

agrupados <- filtrado %>% 
  group_by(year = lubridate::floor_date(fec_defuncion,"month")) %>% 
  summarise(NN = n())


año_2017 <- agrupados[1:12,1:2] 
año_2017$month = as.yearmon(año_2017$year)

año_2018 <- agrupados[13:24,1:2] 
año_2018$month = as.yearmon(año_2018$year)

año_2019 <- agrupados[25:36,1:2] 
año_2019$month = as.yearmon(año_2019$year)

año_2020 <- agrupados[37:48,1:2] 
año_2020$month = as.yearmon(año_2020$year)

año_2021 <- agrupados[49:60,1:2] 
año_2021$month = as.yearmon(año_2021$year)

año_2022 <- agrupados[61:67,1:2] 
año_2022$month = as.yearmon(año_2022$year)



ggplot(rbind(año_2017, año_2018, año_2019, año_2020, año_2021, año_2022), aes(month(month, label=TRUE, abbr=TRUE), NN,                                                                    group = factor(year(month)), colour = factor(year(month)))) +
  geom_line(size = 1) + 
  labs(title= "El número de muertes aumentó en 2021 y 2020 comparado don los demás años",x="Month",y = "Deaths", colour="Year")
####Inciso 3####
Fechas_antes_pandemia <- subset(agrupados, year<=fecha_promedio)
Fechas_despues_pandemia <- subset(agrupados, year>=fecha_promedio)
Promedio <- mean(Fechas_antes_pandemia$NN)

Muertes_despues <- c()
for (a in Fechas_despues_pandemia$year) {
  Muertes_despues<-c(Muertes_despues, Fechas_despues_pandemia$NN - Promedio)
}
tabla_dferencia <- data.frame(Fechas_despues_pandemia, Muertes_despues)

##Ahora vamos a graficar los datos

ggplot(tabla_dferencia, aes(x = year, y = Muertes_despues))+
  geom_line()+
  labs(title = "La diferencia de las muertes con respecto al promedio tiene saltos en enero del 21 y 20", x="Fechas después de pandemia", y="Diferencia de muertes")
