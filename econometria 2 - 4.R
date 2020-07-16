#Taller 4. Grupo 28- Juanita Cortes, David Orozco y Raúl Torres  
#######Paquetes
install.packages("vars")
install.packages("svars")
library(vars)
library(urca)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(dplyr)
library(tidyr)
library(svars)


#########################################################################
#########Punto 1




#############################################################################
#########Punto 2
Datos<-read.csv(file.choose())
Y_1<-ts(Datos$y1)
Y_2<-ts(Datos$y2)
##Descripción de las series
summary(Datos)
autoplot(Y_1)
autoplot(Y_2)
var(Y_1)
var(Y_2)
## vemos que la Y_2 varía mucho más que Y_1
##Las dos series en la misma gráfica 
x11()
plot(merge(as.zoo(Y_1), as.zoo(Y_2)), 
     col=c("red","darkblue"),
     plot.type= "single",
     lty = c(2,1),
     lwd = 1,
     xlab="",
     ylab="serie",
     ylim=c(-5,6),
     main="Series simuladas")
##Pruebas de raíz unitaria
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "trend"))
## La tendencia y deriva son significativas, aunque graficamente parece no tener tendencia 
##por tanto hacemos ambas pruebas 
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "drift"))
##confirmamos que la serie es I(0)
summary(ur.df(Y_2, lags=8, selectlags = "AIC", type = "trend"))
## La tendencia y deriva son significativas, aunque graficamente parece no tener tendencia 
##por tanto hacemos ambas pruebas 
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "drift"))
##confirmamos que la serie es I(0)
##Creamos modelo VAR
Y<-cbind(Y_1,Y_2)
VARselect(Y, lag.max = 8, type = "both", season = NULL)## 3 rezagos
VARselect(Y, lag.max = 8, type = "trend")##3 rezagos
VARselect(Y, lag.max = 8, type= "none")## 3 rezagos
##Utilizaremos 3 rezagos para el VAR
VAR3<-VAR(Y, p=3, type = "both")
summary(VAR3)
##La tendencia y la constante no son significativas
VAR3_1<-VAR(Y, p=3, type = "none")
summary(VAR3_1)
##Modelo SVAR
##unimos las dos series en una matriz
##Creamos la matriz de efectos contemporaneos 
matrix_1=as.matrix(cbind(c(1,NA),c(NA,1)))
matrix_1
SVAR(VAR3_1, Amat = matrix_1, Bmat = NULL )
summary(SVAR3)
help("SVAR")






##########################################################################################################
#########Punto 3