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


# intentos ##########################

# serie M2 #######

M2SL <- read_excel("M2SL.xls")
View(M2SL)
M2<-ts(data.frame(M2SL)$M2SL, frequency = 12, start = 1960)
autoplot(M2)
autoplot(log(M2))
autoplot(diff(log(M2)))
autoplot(diff(M2))
plot(stl(M2,s.window = "periodic"))

#Hacer diagrama de caja por trimestre
(ciclo=cycle(M2))     #Ver el ciclo de la serie
boxplot(M2~ciclo)


#Descomponer la serie (componente tendencial, estacional y aleatorio)
descompos = stl(M2,s.window = "periodic")
plot(descompos)


#fac no decae rapidamente, indicios de tendencia 
acf(M2)
pacf(M2)
Diff_log_m2<-diff(log(M2))



# Vamos a aplicar el test de raíz unitaria Dickey-Fuller, el cual nos dice si 
# una serie tiene raíz unitaria o no. La hipótesis nula es que la serie tiene al menos una 
# raíz unitaria (no es estacionaria), mientras la hipótesis alternativa dice que es estacionaria. 

DF.Test = ur.df(Diff_log_m2, type="none", selectlags = "AIC")
summary(DF.Test) #Rechazo la hipótesis nula, asi que Diff_log_m2 es estacionaria. 

# fac y facp de la serie transformada 
grid.arrange(
  ggAcf(Diff_log_m2,lag.max=20,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(Diff_log_m2,lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

#doble diferencia
grid.arrange(
  ggAcf(diff(Diff_log_m2),lag.max=20,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(diff(Diff_log_m2),lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)

autoplot(diff(diff(M2)))
autoplot(Diff_log_m2)

mar <- 4
mma <- 4
results <- c("p","q","AIC","SBC")
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp <- arima(Diff_log_m2, order = c(i, 0, j), include.mean = TRUE)
    results <- rbind(results,as.numeric(c(i, j, AIC(fitp), BIC(fitp)))) 
  }
}

results
# por criterios de info ####
#ARIMA (1,0,3) de serie lm
mod1<-arima(x = Diff_log_m2,order = c(1,0,3))

#ARIMA (2,1,4) de serie lm
mod2<-arima(x = Diff_log_m2,order = c(1,0,4))

#ARIMA (4,1,2) de serie lm
mod3<-arima(x = Diff_log_m2,order = c(3,0,4))

#ARIMA (4,1,4) de serie lm
mod4<-arima(x = Diff_log_m2,order = c(1,0,2),include.mean = TRUE)

#ARIMA (4,1,4) de serie lm
mod5<-arima(x = Diff_log_m2,order = c(3,0,2))

library(lmtest)
coeftest(mod2)  
#significativos mod 2,4,5


# Validacion del modelo
layout(matrix(1:2, ncol = 2, nrow = 1))
plot(mod5$resid)
acf(mod5$resid, ylim=c(-1,1), main = "FAC de Residuales")

for (i in 1:24) {print( Box.test(resid(mod5), lag=i,  type="Ljung") )}

normalTest(mod5$resid, method="jb")
jarque.bera.test(mod5$residuals)
hist(mod5$residuals)
plot(mod5$residuals)


# parentesis ####
which.max(mod4$resid)
da <- rep(0,length(Diff_log_m2))
da
da[735] <- 1
da
Modelexp1 <- arima(Diff_log_m2, order =c(1,0,2), xreg = da)
for (i in 1:24) {print( Box.test(resid(Modelexp1), lag=i,  type="Ljung") )}


#PRONOSTICO####
pron=predict(mod5,n.ahead = 7)
#Pronosticar 5 a?os futuros
v.futuros=pron$pred #predicciones en log
pred<-exp(v.futuros)

pred=forecast(mod5,h=20)
library(ggplot2)
autoplot(pred)

#REAL vs AJUSTADO
ts.plot(Diff_log_m2,(pred$fitted)[5,10],col=c("black","red"))

autoplot(forecast(auto.arima(Diff_log_m2),h = 12))


Des_ger
names(Des_ger)

# PIB Noruega / Ahora es desempleo alemania gg ####
Pib_nor<- ts(data.frame(Des_ger)$LMUNRRTTDEM156S, frequency =12 , start = 1969)
Pib_nor
autoplot(Pib_nor)
autoplot(log(Pib_nor))
autoplot(diff(log(Pib_nor)))
autoplot(diff(Pib_nor))
plot(stl(Pib_nor,s.window = "periodic"))

#Hacer diagrama de caja por trimestre
(ciclo=cycle(Pib_nor))     #Ver el ciclo de la serie
boxplot(Pib_nor~ciclo)

acf(Pib_nor)
pacf(Pib_nor)
Diff_pib<-diff((Pib_nor))

acf(diff(Pib_nor))
pacf(diff(Pib_nor))
# Vamos a aplicar el test de raíz unitaria Dickey-Fuller, el cual nos dice si 
# una serie tiene raíz unitaria o no. La hipótesis nula es que la serie tiene al menos una 
# raíz unitaria (no es estacionaria), mientras la hipótesis alternativa dice que es estacionaria. 

DF.Test = ur.df(Diff_pib, type="none", selectlags = "AIC")
summary(DF.Test) #Rechazo la hipótesis nula, asi que Diff_log_m2 es estacionaria. 

# fac y facp de la serie transformada 
grid.arrange(
  ggAcf(Diff_pib,lag.max=30,plot=T,lwd=2,xlab='',main='ACF del IPC'),
  ggPacf(Diff_pib,lag.max=30,plot=T,lwd=2,xlab='',main='PACF del IPC')
)
# Seleccion de modelo ####
mar <- 4
mma <- 4
results <- c("p","q","AIC","SBC")
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp <- arima(Diff_pib, order = c(i, 0, j), include.mean = TRUE)
    results <- rbind(results,as.numeric(c(i, j, AIC(fitp), BIC(fitp)))) 
  }
}

results
#ARIMA (2,1,4) de serie lm
mod1<-arima(x = Diff_pib,order = c(1,0,1),include.mean = F)
mod1.1<-arima(x = Diff_pib,order = c(1,0,1),include.mean = TRUE)
#ARIMA (4,1,2) de serie lm
mod2<-arima(x = Diff_pib,order = c(2,0,1),include.mean = F)
mod2.1<-arima(x = Diff_pib,order = c(2,0,1),include.mean = TRUE)

coeftest(mod1)  
coeftest(mod2.1)  

jarqueberaTest(mod1.1$residuals)
plot(mod1$residuals)
qqnorm(mod1.1$residuals)
qqline(mod1.1$residuals)
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