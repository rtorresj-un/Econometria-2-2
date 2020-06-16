#Taller 2 - R Avanzado#
install.packages("tseries")
install.packages("urca")
install.packages("fBasics")
library(tseries)
library(urca)
library(stats)
library(lmtest)
library(urca)
library(fBasics)

#1. Series de tiempo####
library(readxl)
IPC <- read_excel(file.choose(), 
                  col_names = FALSE)
IPC
ipc=ts(IPC$...1,start = c(2008,1),frequency = 12)
summary(ipc)  #Estadística descriptiva

#Graficar la serie temporal

plot(ipc,main="IPC")

#incluir línea de tendencia
abline(lm(ipc~time(ipc)))

(ciclo=cycle(ipc))     #Ver el ciclo de la serie
boxplot(ipc~ciclo)


descompos = stl(ipc,s.window = "periodic")
plot(descompos)

acf(ipc)
pacf()
Box.test(y, lag = 13, type = c ("Ljung-Box"))