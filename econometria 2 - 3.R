#Taller 3 - Econmetría 2####
#Raul Torres, Juanita Cortes, David Orozco
#Primer punto####

#Segundo punto####
library(readr); library(urca); library(tseries)
Data_UR<-read.csv(file.choose())
View(Data_UR)
attach(Data_UR)
plot(x1, type = 'l')
plot(x2, type = 'l')
plot(x3, type = 'l')

#Tercer punto####
Data_coin<-read.csv(file.choose())


#Cuarto punto####