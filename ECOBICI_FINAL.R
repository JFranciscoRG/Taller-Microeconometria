#Entrega Taller 1 
#José Francisco Rivero González 181869
#pregunta de ecobici
rm(list = ls())

library(jsonlite)
library(dtplyr)
library(tidyverse)
library(geosphere)
library(ggplot2)
library(lubridate)
library(forcats)

####Recolección de datos####
data <- fromJSON("https://gbfs.mex.lyftbikes.com/gbfs/es/station_information.json")
latitud <- data$data$stations$lat
longitud <- data$data$stations$lon
estaciones <- as.data.frame(latitud)
estaciones$longitud <- c(longitud)
estaciones$station <- c(data$data$stations$station_id)

#para encontrar los datos de los viajes diarios los podemos encontrar en el archivo
#de ECOBICI1.R*

ene_2019<-read.csv("Data Storage/Created/2019-01 corregido.csv")
feb_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-02.csv")
mar_2019<-read.csv("Data Storage/Created/2019-03 corregido.csv")
abr_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-04.csv")
may_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-05.csv")
jun_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-06.csv")
jul_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-07.csv")
ago_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-08.csv")
sep_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-09.csv")
oct_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-10.csv")
nov_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-11.csv")
dic_2019<-read.csv("Data Storage/Raw/Pregunta 2/2019-12.csv")



#2020


ene_2020<-read.csv("Data Storage/Created/2020-01 corregido.csv")
feb_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-02.csv")
mar_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-03.csv")
abr_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-04.csv")
may_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-05.csv")
jun_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-06.csv")
jul_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-07.csv")
ago_2020<-read.csv("Data Storage/Created/2020-08 corregido.csv")
sep_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-09.csv")
oct_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-10.csv")
nov_2020<-read.csv("Data Storage/Raw/Pregunta 2/2020-11.csv")
#corregi los datos porque no me salían los correctos
dic_2020<-read.csv("Data Storage/Created/2020-12 corregido.csv")

#2021

ene_2021<-read.csv("Data Storage/Created/2021-01 corregido.csv")
feb_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-02.csv")
mar_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-03.csv")
abr_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-04.csv")
may_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-05.csv")
jun_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-06.csv")
jul_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-07.csv")
ago_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-08.csv")
sep_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-09.csv")
oct_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-10.csv")
nov_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-11.csv")
dic_2021<-read.csv("Data Storage/Raw/Pregunta 2/2021-12.csv")

#2020
colnames(ene_2020)<-colnames(ene_2019)
colnames(feb_2020)<-colnames(ene_2019)
colnames(mar_2020)<-colnames(ene_2019)
colnames(abr_2020)<-colnames(ene_2019)
colnames(may_2020)<-colnames(ene_2019)
colnames(jun_2020)<-colnames(ene_2019)
colnames(jul_2020)<-colnames(ene_2019)
colnames(ago_2020)<-colnames(ene_2019)
colnames(sep_2020)<-colnames(ene_2019)
colnames(oct_2020)<-colnames(ene_2019)
colnames(nov_2020)<-colnames(ene_2019)
colnames(dic_2020)<-colnames(ene_2019)

#2021
colnames(ene_2021)<-colnames(ene_2019)
colnames(feb_2021)<-colnames(ene_2019)
colnames(mar_2021)<-colnames(ene_2019)
colnames(abr_2021)<-colnames(ene_2019)
colnames(may_2021)<-colnames(ene_2019)
colnames(jun_2021)<-colnames(ene_2019)
colnames(jul_2021)<-colnames(ene_2019)
colnames(ago_2021)<-colnames(ene_2019)
colnames(sep_2021)<-colnames(ene_2019)
colnames(oct_2021)<-colnames(ene_2019)
colnames(nov_2021)<-colnames(ene_2019)
colnames(dic_2021)<-colnames(ene_2019)

año2019<- rbind(ene_2019,feb_2019,mar_2019,abr_2019,may_2019,jun_2019,jul_2019,ago_2019,sep_2019,oct_2019,nov_2019,dic_2019)
año2020<- rbind(ene_2020,feb_2020,mar_2020,abr_2020,may_2020,jun_2020,jul_2020,ago_2020,sep_2020,oct_2020,nov_2020,dic_2020)
año2021<- rbind(ene_2021,feb_2021,mar_2021,abr_2021,may_2021,jun_2021,jul_2021,ago_2021,sep_2021,oct_2021,nov_2021,dic_2021)

#vamos a hacer un data frame con 3 columnas la de posición donde sale y la posición que entrega y la fecha

Estacion_Retiro<-año2019$Ciclo_Estacion_Retiro #lo hice con uno para después solo agregar columnas 
Viaje_2019<-as.data.frame(Estacion_Retiro)
Viaje_2019$Estacion_Arribo<-c(año2019$Ciclo_Estacion_Arribo)
Viaje_2019$Fecha<-c(año2019$Fecha_Retiro)  

Estacion_Retiro<-año2020$Ciclo_Estacion_Retiro #lo hice con uno para después solo agregar columnas 
Viaje_2020<-as.data.frame(Estacion_Retiro)
Viaje_2020$Estacion_Arribo<-c(año2020$Ciclo_Estacion_Arribo)
Viaje_2020$Fecha<-c(año2020$Fecha_Retiro)  

Estacion_Retiro<-año2021$Ciclo_Estacion_Retiro #lo hice con uno para después solo agregar columnas 
Viaje_2021<-as.data.frame(Estacion_Retiro)
Viaje_2021$Estacion_Arribo<-c(año2021$Ciclo_Estacion_Arribo)
Viaje_2021$Fecha<-c(año2021$Fecha_Retiro)  


Viaje_2019 <- subset(Viaje_2019, grepl("2019", Fecha))#esto selescciona solo las fechas que están en 2019
Viaje_2020 <- subset(Viaje_2020, grepl("2020", Fecha))#esto selecciona solo las fechas que están en 2020
Viaje_2021 <- subset(Viaje_2021, grepl("2021", Fecha))#esto selecciona solo las fechas que están en 2021

#tengo que mejorar la eficiencia con determinar para cada viaje las columnas necesarias

Viaje_2019$Fecha <- as.Date(format(as.Date(Viaje_2019$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))#con lo que hice en ECOBICI1 para cambiar el formato de las fechas

Viaje_2020$Fecha <- as.Date(format(as.Date(Viaje_2020$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))#con lo que hice en ECOBICI1 para cambiar el formato de las fechas

Viaje_2021$Fecha <- as.Date(format(as.Date(Viaje_2021$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))#con lo que hice en ECOBICI1 para cambiar el formato de las fechas
## 
año2019 <- subset(año2019, grepl("2019", Fecha_Retiro))
año2020 <- subset(año2020, grepl("2020", Fecha_Retiro))
año2021 <- subset(año2021, grepl("2021", Fecha_Retiro))

bici19<-c()
for (a in unique(año2019$Fecha_Retiro)) {
  bici19 <- c(bici19, sum(a == año2019$Fecha_Retiro))
}
tabla19<- cbind.data.frame(fecha=sort(unique(año2019$Fecha_Retiro)), num_viajes=bici19)

#Filtrar los datos de 2020 

bici20 <- c()
for (a in unique(año2020$Fecha_Retiro)) {
  bici20 <- c(bici20, sum(a == año2020$Fecha_Retiro))
}

tabla20 <- cbind.data.frame(fecha=sort(unique(año2020$Fecha_Retiro)), num_viajes=bici20)

#Filtrar los datos de 2021

bici21<-c()
for (a in unique(año2021$Fecha_Retiro)) {
  bici21 <- c(bici21, sum(a == año2021$Fecha_Retiro))
}

tabla21 <- cbind.data.frame(fecha=sort(unique(año2021$Fecha_Retiro)), num_viajes=bici21)

####respuestas a ejercicio####

####Pregunta 1####
# Unir los datos en una tabla
tabla_completa <- data.frame(
  fecha = c(sort(unique(año2019$Fecha_Retiro)),
            sort(unique(año2020$Fecha_Retiro)),
            sort(unique(año2021$Fecha_Retiro))),
  viajes_diarios = c(bici19, bici20, bici21),
  año = c(rep(2019, length(bici19)),
          rep(2020, length(bici20)),
          rep(2021, length(bici21)))
)


tabla_completa$fecha <- as.Date(format(as.Date(tabla_completa$fecha, "%d/%m/%Y"), "%Y-%m-%d"))




# Crear una gráfica de línea con library(ggplot2)

#install.packages("plotly")
library(plotly)

ggplot(tabla_completa, aes(x = fecha, y = viajes_diarios, group = año, color = as.factor(año))) +
  geom_line() +
  geom_rect(xmin = as.Date("2020-03-23"), xmax = as.Date("2020-05-31"), ymin = -10000, ymax = 10000, 
            fill = "pink", alpha = 0.05) +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = "dashed") +
  labs(x = "Fecha", y = "Viajes diarios", title = "Evolución de los viajes diarios por año") +
  scale_color_discrete(name = "Año") +
  theme(panel.background = element_rect(fill = "lightgray"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))


####Pregunta 2####


tabla_completa$día_semana <- weekdays(tabla_completa$fecha, abbreviate = TRUE)

ggplot(tabla_completa, aes(x = viajes_diarios)) +
  geom_histogram(bins = 30, color = "black", fill = "lightblue") +
  labs(x = "Viajes diarios", y = "Frecuencia", title = "Histograma de viajes diarios") +
  scale_y_continuous(label = scales::comma_format()) +
  theme(panel.background = element_rect(fill = "lightgray"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))

densidad_sem<-ggplot(tabla_completa, aes(x = viajes_diarios, fill = día_semana)) +
  geom_histogram(binwidth = 1000, color = "white") +
  labs(x = "Viajes diarios", y = "Frecuencia", title = "Histograma de los viajes diarios por día de la semana") +
  facet_wrap(~ día_semana, nrow = 2)

####Vamos a hacerlo dinámico
install.packages("GGally")
library(GGally)
densidad_sem<-ggplotly(densidad_sem)
densidad_sem

##Se observa que en los datos de las estaciones que obtenemos de retiro y de arribo
##se tiene estaciones o números que no cuadran con el numero de estaciones que se tienen
##en la variable estaciones

##Entonces tengo que volver a acomodar esos datos que filtrar los datos que se tienen
##para que solo me queden los de las coordenadas.

Viaje_2019 <- Viaje_2019 %>% 
  filter(Estacion_Retiro %in% estaciones$station) %>%
  mutate(Estacion_Retiro = factor(Estacion_Retiro, levels = estaciones$station))
Viaje_2019 <- Viaje_2019 %>% 
  filter(Estacion_Arribo %in% estaciones$station) %>%
  mutate(Estacion_Arribo = factor(Estacion_Arribo, levels = estaciones$station))

Viaje_2020 <- Viaje_2020 %>% 
  filter(Estacion_Retiro %in% estaciones$station) %>%
  mutate(Estacion_Retiro = factor(Estacion_Retiro, levels = estaciones$station))
Viaje_2020 <- Viaje_2020 %>% 
  filter(Estacion_Arribo %in% estaciones$station) %>%
  mutate(Estacion_Arribo = factor(Estacion_Arribo, levels = estaciones$station))

Viaje_2021 <- Viaje_2021 %>% 
  filter(Estacion_Retiro %in% estaciones$station) %>%
  mutate(Estacion_Retiro = factor(Estacion_Retiro, levels = estaciones$station))
Viaje_2021 <- Viaje_2021 %>% 
  filter(Estacion_Arribo %in% estaciones$station) %>%
  mutate(Estacion_Arribo = factor(Estacion_Arribo, levels = estaciones$station))



#tengo que hacer esto para 2020 y 2021 para que me queden las estaciones que tiene
#latitud y longitud



# Crear columnas para latitud y longitud de estación de salida y arribo
# lo que hicismos es agregar con mutate 4 columnas al df de Viaje_2019
#este se llaman lat_Retiro,lonRetiro [...] de ahí cada una la llenamos
#con un if else donde el prier argumento es si Estacion retiro está en 
#las estaciones de Json entonces, el primer argumento es 
#encuentra con match la estacion retiro/arribo y ponle su latitud/longitud,
#de lo contrario pon 0.

Viaje_2019 <- Viaje_2019 %>% 
  mutate(lat_Retiro = ifelse(Estacion_Retiro %in% estaciones$station, estaciones$latitud[match(Estacion_Retiro, estaciones$station)], ""),
         lon_Retiro = ifelse(Estacion_Retiro %in% estaciones$station, estaciones$longitud[match(Estacion_Retiro, estaciones$station)], ""),
         lat_Arribo = ifelse(Estacion_Arribo %in% estaciones$station, estaciones$latitud[match(Estacion_Arribo, estaciones$station)], ""),
         lon_Arribo = ifelse(Estacion_Arribo %in% estaciones$station, estaciones$longitud[match(Estacion_Arribo, estaciones$station)], ""))

Viaje_2020 <- Viaje_2020 %>% 
  mutate(lat_Retiro = ifelse(Estacion_Retiro %in% estaciones$station, estaciones$latitud[match(Estacion_Retiro, estaciones$station)], ""),
         lon_Retiro = ifelse(Estacion_Retiro %in% estaciones$station, estaciones$longitud[match(Estacion_Retiro, estaciones$station)], ""),
         lat_Arribo = ifelse(Estacion_Arribo %in% estaciones$station, estaciones$latitud[match(Estacion_Arribo, estaciones$station)], ""),
         lon_Arribo = ifelse(Estacion_Arribo %in% estaciones$station, estaciones$longitud[match(Estacion_Arribo, estaciones$station)], ""))
Viaje_2021 <- Viaje_2021 %>% 
  mutate(lat_Retiro = ifelse(Estacion_Retiro %in% estaciones$station, estaciones$latitud[match(Estacion_Retiro, estaciones$station)], ""),
         lon_Retiro = ifelse(Estacion_Retiro %in% estaciones$station, estaciones$longitud[match(Estacion_Retiro, estaciones$station)], ""),
         lat_Arribo = ifelse(Estacion_Arribo %in% estaciones$station, estaciones$latitud[match(Estacion_Arribo, estaciones$station)], ""),
         lon_Arribo = ifelse(Estacion_Arribo %in% estaciones$station, estaciones$longitud[match(Estacion_Arribo, estaciones$station)], ""))

##Lo que haremo será agregar las distancias totales para cada fecha
##eso lo haremos con la biblioteca geospher

####Pregunta 3####

Viaje_2019 <- Viaje_2019 %>% 
  filter(between(lat_Retiro, -90,90),between(lat_Arribo,-90,90))#tenemos que filtrar aquellos archivos que tienen latitudes o longitudes que no están
Viaje_2020 <- Viaje_2020 %>% #en valores correctos 
  filter(between(lat_Retiro, -90,90),between(lat_Arribo,-90,90))
Viaje_2021 <- Viaje_2021 %>% 
  filter(between(lat_Retiro, -90,90),between(lat_Arribo,-90,90))


distancia_2019 <- distGeo(cbind(Viaje_2019$lon_Retiro, Viaje_2019$lat_Arribo),#es importante que pongas en la primera la longitud y en la segunda la latitud,
                          cbind(Viaje_2019$lon_Arribo, Viaje_2019$lat_Arribo))
distancia_2020 <- distGeo(cbind(Viaje_2020$lon_Retiro, Viaje_2020$lat_Arribo),#es importante que pongas en la primera la longitud y en la segunda la latitud,
                          cbind(Viaje_2020$lon_Arribo, Viaje_2020$lat_Arribo))
distancia_2021 <- distGeo(cbind(Viaje_2021$lon_Retiro, Viaje_2021$lat_Arribo),#es importante que pongas en la primera la longitud y en la segunda la latitud,
                          cbind(Viaje_2021$lon_Arribo, Viaje_2021$lat_Arribo))

Viaje_2019$distancia <- distancia_2019 #la distancia que obtenemos está en metros
Viaje_2020$distancia <- distancia_2020 #la distancia que obtenemos está en metros
Viaje_2021$distancia <- distancia_2021 #la distancia que obtenemos está en metros

# Agrupar por fecha y calcular la suma de las distancias recorridas en cada día
distancia_Total19 <- Viaje_2019 %>%
  group_by(Fecha) %>%
  summarize(distancia = sum(distancia, na.rm = TRUE))
distancia_Total20 <- Viaje_2020 %>%
  group_by(Fecha) %>%
  summarize(distancia = sum(distancia, na.rm = TRUE))
distancia_Total21 <- Viaje_2021 %>%
  group_by(Fecha) %>%
  summarize(distancia = sum(distancia, na.rm = TRUE))

distancia_Total19$Fecha <- as.Date(format(as.Date(distancia_Total19$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))
distancia_Total20$Fecha <- as.Date(format(as.Date(distancia_Total20$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))
distancia_Total21$Fecha <- as.Date(format(as.Date(distancia_Total21$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))


# Graficar la distancia total recorrida por día

ggplot(data = distancia_Total19, aes(x = Fecha, y = distancia/1000)) +
  geom_point() +
  labs(title = "Distancia total recorrida por día en 2019(en KM)", x = "Fecha", y = "Distancia")
ggplot(data = distancia_Total20, aes(x = Fecha, y = distancia/1000)) +
  geom_point() +
  labs(title = "Distancia total recorrida por día en 2020(en KM) disminuye después de abril", x = "Fecha", y = "Distancia")
ggplot(data = distancia_Total21, aes(x = Fecha, y = distancia/1000)) +
  geom_point() +
  labs(title = "Distancia total recorrida por día en 2021(en KM) tiene tendencia de aumentar, pero no llega a los niveles de 2019", x = "Fecha", y = "Distancia")

##vamos a juntar los 3 años

distancia_completa <- data.frame(
  Fecha = c(distancia_Total19$Fecha, distancia_Total20$Fecha, distancia_Total21$Fecha),
  distancia = c(distancia_Total19$distancia, distancia_Total20$distancia, distancia_Total21$distancia)
)

distancia_completa$Fecha <- as.Date(format(as.Date(distancia_completa$Fecha, "%d/%m/%Y"), "%Y-%m-%d"))

##Graficar todo
ggplot(distancia_completa, aes(x = Fecha, y = distancia/1000)) + 
  geom_point()+
  labs(title = "Distancia recorrida tiene un aumento después del 2020 (km) pero no se llega a los niveles de 2019" , x ="años", y = "distancia")

####Pregunta 4#### 

#año2019<- rbind(ene_2019,feb_2019,mar_2019,abr_2019,may_2019,jun_2019,jul_2019,ago_2019,sep_2019,oct_2019,nov_2019,dic_2019)
#año2020<- rbind(ene_2020,feb_2020,mar_2020,abr_2020,may_2020,jun_2020,jul_2020,ago_2020,sep_2020,oct_2020,nov_2020,dic_2020)
#año2021<- rbind(ene_2021,feb_2021,mar_2021,abr_2021,may_2021,jun_2021,jul_2021,ago_2021,sep_2021,oct_2021,nov_2021,dic_2021)

#Viaje_2019 <- subset(Viaje_2019, grepl("2019", Fecha))
#Viaje_2020 <- subset(Viaje_2019, grepl("2020", Fecha))
#Viaje_2021 <- subset(Viaje_2019, grepl("2021", Fecha))

#Hagan una grafica que muestre cuales son las bicicletas mas rapidas y cuales son las mas usadas. Realice
#una grafica que muestre si hay alguna relacion entre “mas rapida” y “mas usada”

#### Análisis de los datos #####

# Lo que quiero hacer es transformar las variables para después poder calcular las diferencias en tiempo 
#de esa forma podemos después calcular con difftime justo cuanto se tardaron las bicicletas

año2019$Fecha_Retiro <- as.Date(año2019$Fecha_Retiro, format = "%d/%m/%Y")
año2019$Hora_Retiro <- as.POSIXct(año2019$Hora_Retiro, format = "%H:%M:%S")
año2019$Fecha_Arribo <- as.Date(año2019$Fecha_Arribo, format = "%d/%m/%Y")
año2019$Hora_Arribo <- as.POSIXct(año2019$Hora_Arribo, format = "%H:%M:%S")

año2020$Fecha_Retiro <- as.Date(año2020$Fecha_Retiro, format = "%d/%m/%Y")
año2020$Hora_Retiro <- as.POSIXct(año2020$Hora_Retiro, format = "%H:%M:%S")
año2020$Fecha_Arribo <- as.Date(año2020$Fecha_Arribo, format = "%d/%m/%Y")
año2020$Hora_Arribo <- as.POSIXct(año2020$Hora_Arribo, format = "%H:%M:%S")

año2021$Fecha_Retiro <- as.Date(año2021$Fecha_Retiro, format = "%d/%m/%Y")
año2021$Hora_Retiro <- as.POSIXct(año2021$Hora_Retiro, format = "%H:%M:%S")
año2021$Fecha_Arribo <- as.Date(año2021$Fecha_Arribo, format = "%d/%m/%Y")
año2021$Hora_Arribo <- as.POSIXct(año2021$Hora_Arribo, format = "%H:%M:%S")

# Calcular diferencia de hora para los disntintos años después lo voy a juntar con la distancia recorrida
#y ver qué bicicletas usó para checar la relación entre bicicletas y rapidez
año2019$Dif_Horas <- difftime(año2019$Hora_Arribo, año2019$Hora_Retiro, units = "hours")
año2020$Dif_Horas <- difftime(año2020$Hora_Arribo, año2020$Hora_Retiro, units = "hours")
año2021$Dif_Horas <- difftime(año2021$Hora_Arribo, año2021$Hora_Retiro, units = "hours")

#Lo que haré será sacar el promedio de la distancia recorrida y el promedio de tiempo diario

z_2019 <- año2019 %>% 
  group_by(Bici) %>% 
  summarise(mean(Dif_Horas),n=n())
z_2020 <- año2020 %>% 
  group_by(Bici) %>% 
  summarise(mean(Dif_Horas),n=n())
z_2021<- año2021 %>% 
  group_by(Bici) %>% 
  summarise(mean(Dif_Horas),n=n())

z_2019 <- z_2019 %>%
  mutate(sqrt_Dif_Horas = as.difftime(sqrt(as.numeric(`mean(Dif_Horas)`)), units = "hours"))
z_2020 <- z_2020 %>%
  mutate(sqrt_Dif_Horas = as.difftime(sqrt(as.numeric(`mean(Dif_Horas)`)), units = "hours"))
z_2021 <- z_2021 %>%
  mutate(sqrt_Dif_Horas = as.difftime(sqrt(as.numeric(`mean(Dif_Horas)`)), units = "hours"))

z <- rbind(z_2019,z_2020,z_2021)

ggplot(z, aes(x = Bici, y = sqrt_Dif_Horas, color = n)) + 
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Tipo de Bicicleta", y = "Raíz cuadrada de la diferencia en horas", colo = "Cantidad de bicicletas usadas")

z_top1019 <- z_2019 %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
z_top1020 <- z_2020 %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
z_top1021 <- z_2021 %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
z_top100 <- z%>% 
  arrange(desc(n)) %>% 
  slice(1:10)

ggplot(z_top1019, aes(x=fct_reorder(Bici, n), y=sqrt_Dif_Horas, size=n, color=n)) + 
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "La bicilceta más usada en 2019 no es la que menos tiempo hace",
       x="Tipo de Bicicleta", 
       y="Raíz cuadrada de la diferencia en horas", 
       size="Cantidad de bicicletas usadas")
ggplot(z_top1020, aes(x=fct_reorder(Bici, n), y=sqrt_Dif_Horas,color=n,size=n)) + 
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Hay una relación entre la bicicleta más usada y la que menos tiempo hace",
       x="Tipo de Bicicleta", 
       y="Raíz cuadrada de la diferencia en horas", 
       size="Cantidad de bicicletas usadas")
ggplot(z_top1021, aes(x=fct_reorder(Bici, n), y=sqrt_Dif_Horas, color=n, size=n)) + 
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "El numero de bicicletas usadas en 2021 se recupera con respecto a 2020",
       x="Tipo de Bicicleta", 
       y="Raíz cuadrada de la diferencia en horas", 
       size="Cantidad de bicicletas usadas")
ggplot(z_top100, aes(x=fct_reorder(Bici, n), y=sqrt_Dif_Horas, color=n, size=n)) + 
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "La grafica de las bicicletas más usadas y rápidas se parecer a la del 2019",
       x="Tipo de Bicicleta", 
       y="Raíz cuadrada de la diferencia en horas", 
       size="Cantidad de bicicletas usadas")

  