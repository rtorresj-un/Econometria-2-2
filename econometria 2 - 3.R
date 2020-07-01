#Taller 3 - Econmetría 2####
#Raul Torres, Juanita Cortes, David Orozco
library(readr); library(urca); library(tseries); library(gridExtra); library(ggfortify)
library(forecast)
#Primer punto####
Data_1<-read_delim(file.choose(),";", escape_double = FALSE, trim_ws = TRUE)
IPC_DE<-ts(data.frame(Data_1)$IPC_DE, frequency = 12, start = 1960)

summary(IPC_DE)
start(IPC_DE)
end(IPC_DE)
#Clara tendencia de la serie
autoplot(IPC_DE,main = "IPC Alemania (enero 1960- abril 2020)")

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

# Gráfico de autocorrelación simple y parcial diff log
  #claro componente estacional cada 12 periodo
grid.arrange(
  ggAcf(diff(log(IPC_DE)),lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(diff(log(IPC_DE)),lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

#Transformaciones
Diff_log<- diff(log(IPC_DE))

#Diferencia estacional de la serie anterior 
Diff_s<-diff(Diff_log,lag = 12,differences = 1)
autoplot(Diff_s)

grid.arrange(
  ggAcf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC')
)
# El comportamiento estacional se mantiene en los gráficos de autocorrelación

autoplot(diff(diff(log(IPC_DE)),12))
grid.arrange(
  ggAcf(diff(diff(log(IPC_DE)),12),lag.max=30,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(diff(diff(log(IPC_DE)),12),lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

plot(stl(diff(log(IPC_DE)), s.window = 'periodic')$time.series[,2])

arima<-auto.arima(log(IPC_DE))
arima1<-arima(diff(log(IPC_DE)),order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12))
arima2<-arima(diff(log(IPC_DE)),order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
arima3<-arima(diff(log(IPC_DE)),order=c(0,0,1),seasonal=list(order=c(0,1,3),period=12),include.mean=T)
arima4<-arima(diff(log(IPC_DE)),order=c(0,0,2),seasonal=list(order=c(0,1,1),period=12))

grid.arrange(
  ggAcf(residuals(arima3),lag.max=30,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(residuals(arima3),lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

ts.plot( diff(log(IPC_DE)),fitted(arima3), col=c('blue', 'red'))

qqnorm(residuals(arima3))
qqline(residuals(arima3))
jarque.bera.test(residuals(arima3))
shapiro.test(residuals(arima3))
summary(IPC_DE)



forecast()

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