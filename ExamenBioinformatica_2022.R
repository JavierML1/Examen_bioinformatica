

#Ejercicio 2

# a)

tiempo <- nycflights13::weather

dim(tiempo)

# b) 

table(tiempo$origin)

# c)

LGA <- tiempo[tiempo$origin == "LGA" , ]

median(LGA$wind_speed, na.rm = T)
mean(LGA$pressure, na.rm = T)

# d)

library(tidyverse)
library(lubridate)

tiempo_sin_NA <- tiempo[complete.cases(tiempo$wind_gust),]
meses <- tiempo_sin_NA %>%
  group_by(month) %>%
  summarise(media_wind_speed = mean(wind_speed),
            media_wind_gust = mean(wind_gust),
            n_casos = n()
  )

meses


# Ejercicio 3

LGA <- tiempo[tiempo$origin == "LGA" , ]
EWR <- tiempo[tiempo$origin == "EWR" , ]
JFK <- tiempo[tiempo$origin == "JFK" , ]

par(mfrow = c(1,3))
boxplot(EWR$temp ~ EWR$month, main = "EWR", ylab = "ºC", xlab = "Months", col =  "red")
boxplot(JFK$temp ~ JFK$month, main = "JFK", ylab = "ºC", xlab = "Months", col =  "green")
boxplot(LGA$temp ~ LGA$month, main = "LGA", ylab = "ºC", xlab = "Months", col =  "blue")





plot_meteo <- function(data , columna1, columna2, titulo, unidades, color)
{
  dat <- data.frame(data)
  x <- dat[ , columna1]
  y <- dat[ , columna2]
  boxplot(x ~ y , type = "l", main = titulo , xlab = "Hours", ylab = unidades, col = color)
  media <- c(mean(EWR$humid, na.rm = T), mean(JFK$humid, na.rm = T), mean(LGA$humid, na.rm = T))
  media_total <- (mean(media))
  return(media)
  print(paste0("La media total es " , media_total))
}
par(mfrow = c(1,3))
plot_meteo(EWR, columna1 = "humid", columna2 ="month", titulo = "Humedad", unidades = "Relative humidity", color = "red")
plot_meteo(JFK, columna1 = "humid", columna2 ="month", titulo = "Humedad", unidades = "Relative humidity", color = "green")
plot_meteo(LGA, columna1 = "humid", columna2 ="month", titulo = "Humedad", unidades = "Relative humidity", color = "blue")
#Ejercicio 4
# a.
cumple <- tiempo[tiempo$month == 3 & tiempo$day == 13 , ]
cumple_JFK <- tiempo[tiempo$origin == "JFK" & tiempo$month == 3 & tiempo$day == 13 , ]
cumple_EWR <- tiempo[tiempo$origin == "EWR" & tiempo$month == 3 & tiempo$day == 13 , ]
cumple_LGA <- tiempo[tiempo$origin == "LGA" & tiempo$month == 3 & tiempo$day == 13 , ]



cor(cumple_EWR$temp , cumple_EWR$humid)

cor(cumple_JFK$temp , cumple_JFK$humid)

cor(cumple_LGA$temp , cumple_LGA$humid)


ggplot(cumple) + 
  geom_point(mapping = aes(x = temp , y = humid, col = origin, size = origin)) +
  ggtitle("Temperatura vs Humedad") + 
  labs(x = "Temperatura", y = "Humedad")

# b.

par(mfrow = c(1,1))
cumple_JFK_LGA <- cumple <- tiempo[tiempo$origin != "EWR" & tiempo$month == 3 & tiempo$day == 13 , ]

t.test(cumple_JFK_LGA$temp ~ cumple$origin)

boxplot(cumple_JFK_LGA$temp ~ cumple_JFK_LGA$origin, xlab = "Origin", ylab = "Temp")

    
