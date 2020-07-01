#Taller 3 - Econmetría 2####
#Raul Torres, Juanita Cortes, David Orozco
library(readr); library(urca); library(tseries); library(gridExtra); library(ggfortify)
library(forecast); library(seasonal); library(aTSA)
#Primer punto####
Data_1<-read_delim(file.choose(),";", escape_double = FALSE, trim_ws = TRUE)
IPC_DE<-ts(data.frame(Data_1)$IPC_DE, frequency = 12, start = 1960)

summary(IPC_DE)
start(IPC_DE)
end(IPC_DE)
#Clara tendencia de la serie
monthplot(IPC_DE, col = "midnightblue")
autoplot(IPC_DE,main = "IPC Alemania (enero 1960 - abril 2020)")

# Autocorrelación simple y parcial serie original
grid.arrange(
  ggAcf(IPC_DE,lag.max=30,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(IPC_DE,lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

# Gráfico serie en primera diferencia y diferencia de los log
  #Al aplicar log se logra estabilizar la varianza
grid.arrange(
  autoplot(diff(IPC_DE)),
  autoplot(diff(log(IPC_DE)))
)  
BoxCox.ar(IPC_DE)
# Gráfico de autocorrelación simple y parcial diff log
  #claro componente estacional cada 12 periodo
grid.arrange(
  ggAcf(diff(log(IPC_DE)),lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(diff(log(IPC_DE)),lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

#Pruebas de raiz unitaria
#El tau me dice si la serie tiene o no al menos una raíz unitaria. 
#El phi3 me dice si la tendecia es significativa
#El phi2 me indica si la deriva es significativa
summary(ur.df(IPC_DE,type = 'trend'))
summary(ur.df(IPC_DE,type = 'drift'))
summary(ur.df(IPC_DE,type = 'none'))

#Transformaciones
Diff_log<- diff(log(IPC_DE))

#Diferencia estacional de la serie anterior 
Diff_s<-diff(Diff_log,lag = 12,differences = 1)
autoplot(Diff_s)

# El comportamiento estacional se mantiene en los gráficos de autocorrelación

autoplot(diff(diff(log(IPC_DE)),12))
grid.arrange(
  ggAcf(diff(diff(log(IPC_DE)),12),lag.max=30,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(diff(diff(log(IPC_DE)),12),lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

IPC_x11<-seas(diff(log(IPC_DE)),transform.function="none",x11="")
plot(IPC_x11)
IPC_SeasAdj<-seasadj(IPC_x11)
autoplot(IPC_SeasAdj)
autoplot(stl(diff(log(IPC_DE)), s.window = 'periodic')$time.series[,2])

arima<-auto.arima(Diff_s)
arima1<-arima(diff(log(IPC_DE)),order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12))
arima2<-arima(diff(log(IPC_DE)),order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
arima3<-arima(log(IPC_DE),order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12))
arima4<-arima(diff(log(IPC_DE)),order=c(0,0,2),seasonal=list(order=c(0,1,1),period=12))

AR=3; MA=3
arma_seleccion = function(AR.m, MA.m){
  for (i in 0:AR) {
    for (j in 0:MA)  {
      fitp <- arima(log(IPC_DE), order = c(i, 1, j), seasonal=list(order=c(0,1,2),period=12))
      print( c(i, j, AIC(fitp), BIC(fitp)) )
    }
  }
}

arma_seleccion(AR.m, MA.m)

ts.plot( log(IPC_DE), fitted(arima3), col=c('blue', 'red'))

hist(residuals(arima3))
qqnorm(residuals(arima3))
qqline(residuals(arima3))
jarque.bera.test(residuals(arima3))
shapiro.test(residuals(arima3))

grid.arrange(
  ggAcf(residuals(arima3),lag.max=30,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(residuals(arima3),lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

summary(ur.df(fitted(arima3),type = 'trend'))
summary(ur.df(fitted(arima3),type = 'drift'))
summary(ur.df(fitted(arima3),type = 'none'))

Box.test(residuals(arima3),type='Ljung-Box',lag=length(IPC_DE)/4)
Box.test(residuals(arima3),type='Box-Pierce',lag=length(IPC_DE)/4)
ggtsdiag(arima3)
arch <-arch.test(arima3, output=TRUE)

fore1<-autoplot(forecast::forecast(arima3, level = c(95), h = 7))+
                              scale_x_continuous(limit = c(2010, 2025))+
                              scale_y_continuous(limit = c(4.25, 4.75))
print(fore1)

#Segundo punto####
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