install.packages("tseries")
install.packages("urca")
install.packages("fBasics")
library(tseries)
library(urca)
library(stats)
library(lmtest)
library(urca)
library(fBasics)

IPC
ipc=ts(IPC$...1,start = c(2008,1),frequency = 12)
class(ipc)  #Ver el tipo de objeto
start(ipc)  #Ver cu?ndo comienza la serie
end(ipc)  #Ver cu?ndo finaliza la serie
frequency(ipc)  #Ver la periodicidad de los datos
summary(ipc)  #Estad?stica descriptiva
View(ipc)  #Ver la serie

#Graficar la serie temporal

plot(ipc,main="IPC")

#incluir l?nea de tendencia
abline(lm(ipc~time(ipc)))

(ciclo=cycle(ipc))     #Ver el ciclo de la serie
boxplot(ipc~ciclo)


descompos = stl(ipc,s.window = "periodic")
plot(descompos)

acf(ipc)
pacf()
Box.test(y, lag = 13, type = c ("Ljung-Box"))