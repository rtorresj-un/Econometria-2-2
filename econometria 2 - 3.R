#Taller 3 - Econmetría 2####
#Raul Torres, Juanita Cortes, David Orozco
#Primer punto####

#Segundo punto####
library(readr); library(urca); library(tseries); library(gridExtra); library(ggfortify)
library(forecast)
Data_UR<-read.csv(file.choose())
View(Data_UR)
attach(Data_UR)
plot(x1, type = 'l')
plot(x2, type = 'l')
plot(x3, type = 'l')
grid.arrange(
ggAcf(x1,lag.max=15,plot=T,lwd=2,xlab='',main='ACF de x1'),
ggPacf(x1,lag.max=15,plot=T,lwd=2,xlab='',main='PACF de x1')
)
ur.df(x1, type = 'none', lags = 15, selectlags = 'AIC')
#Tercer punto####
Data_coin<-read.csv(file.choose())


#Cuarto punto####