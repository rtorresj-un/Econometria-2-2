#Taller 3 - Econmetría 2####
#Raul Torres, Juanita Cortes, David Orozco
library(readr); library(urca); library(tseries); library(gridExtra); library(ggfortify); library(sandwich)
library(forecast); library(seasonal); library(aTSA); library(readxl); library(timeDate); library(FinTS); library(dynlm)
############################################################################
# This R function helps to interpret the output of the urca::ur.df function.
interp_urdf <- function(urdf, level="5pct") {
  if(class(urdf) != "ur.df") stop('parameter is not of class ur.df from urca package')
  if(!(level %in% c("1pct", "5pct", "10pct") ) ) stop('parameter level is not one of 1pct, 5pct, or 10pct')
  
  cat("========================================================================\n")
  cat( paste("At the", level, "level:\n") )
  if(urdf@model == "none") {
    cat("The model is of type none\n")
    tau1_crit = urdf@cval["tau1",level]
    tau1_teststat = urdf@teststat["statistic","tau1"]
    tau1_teststat_wi_crit = tau1_teststat > tau1_crit
    if(tau1_teststat_wi_crit) {
      cat("tau1: The null hypothesis is not rejected, unit root is present\n")
    } else {
      cat("tau1: The null hypothesis is rejected, unit root is not present\n")
    }
  } else if(urdf@model == "drift") {
    cat("The model is of type drift\n")
    tau2_crit = urdf@cval["tau2",level]
    tau2_teststat = urdf@teststat["statistic","tau2"]
    tau2_teststat_wi_crit = tau2_teststat > tau2_crit
    phi1_crit = urdf@cval["phi1",level]
    phi1_teststat = urdf@teststat["statistic","phi1"]
    phi1_teststat_wi_crit = phi1_teststat < phi1_crit
    if(tau2_teststat_wi_crit) {
      # Unit root present branch
      cat("tau2: The first null hypothesis is not rejected, unit root is present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is drift.\n")
      }
    } else {
      # Unit root not present branch
      cat("tau2: The first null hypothesis is rejected, unit root is not present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
        warning("This is inconsistent with the first null hypothesis.")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there is drift.\n")
      }
    }
  } else if(urdf@model == "trend") {
    cat("The model is of type trend\n")
    tau3_crit = urdf@cval["tau3",level]
    tau3_teststat = urdf@teststat["statistic","tau3"]
    tau3_teststat_wi_crit = tau3_teststat > tau3_crit
    phi2_crit = urdf@cval["phi2",level]
    phi2_teststat = urdf@teststat["statistic","phi2"]
    phi2_teststat_wi_crit = phi2_teststat < phi2_crit
    phi3_crit = urdf@cval["phi3",level]
    phi3_teststat = urdf@teststat["statistic","phi3"]
    phi3_teststat_wi_crit = phi3_teststat < phi3_crit
    if(tau3_teststat_wi_crit) {
      # First null hypothesis is not rejected, Unit root present branch
      cat("tau3: The first null hypothesis is not rejected, unit root is present\n")
      if(phi3_teststat_wi_crit) {
        # Second null hypothesis is not rejected
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is no trend, and there is drift\n")
        }
      }
      else {
        # Second null hypothesis is rejected
        cat("phi3: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is trend, and there may or may not be drift\n")
          warning("Presence of drift is inconclusive.")
        }
      }
    } else {
      # First null hypothesis is rejected, Unit root not present branch
      cat("tau3: The first null hypothesis is rejected, unit root is not present\n")
      if(phi3_teststat_wi_crit) {
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        warning("This is inconsistent with the first null hypothesis.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there is no trend, and there is drift\n")
        }
      } else {
        cat("phi3: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there may or may not be trend\n")
        warning("Presence of trend is inconclusive.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first and second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there may or may not be trend, and there may or may not be drift\n")
          warning("Presence of trend and drift is inconclusive.")
        }
      }
    }
  } else warning('urdf model type is not one of none, drift, or trend')
  cat("========================================================================\n")
}
############################################################################
#Primer punto####
Data_1<-read_delim(file.choose(),";", escape_double = FALSE, trim_ws = TRUE)
IPC_DE<-ts(data.frame(Data_1)$IPC_DE[481:length(IPC_DE)], frequency = 12, start = 2000)

summary(IPC_DE); kurtosis(IPC_DE)
start(IPC_DE); end(IPC_DE)
#Clara tendencia de la serie
autoplot(IPC_DE,main = "IPC Alemania (enero 1960 - abril 2020)")
monthplot(IPC_DE, col = "midnightblue")

# Autocorrelación simple y parcial serie original
grid.arrange(
  ggAcf(IPC_DE,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC', ylim=c(-1,1)),
  ggPacf(IPC_DE,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC', ylim=c(-1,1)),
  ggAcf(diff(IPC_DE),lag.max=64,plot=T,lwd=2,xlab='',main='ACF del difIPC', ylim=c(-1,1)),
  ggPacf(diff(IPC_DE),lag.max=64,plot=T,lwd=2,xlab='',main='PACF del difIPC', ylim=c(-1,1)),
  ggAcf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC diferenciado estacional', ylim=c(-1,1)),
  ggPacf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC diferenciado estacional', ylim=c(-1,1))
)

# Gráfico serie en primera diferencia y diferencia de los log
#Al aplicar log se logra estabilizar la varianza
grid.arrange(
  autoplot(diff(IPC_DE)),
  autoplot(diff(log(IPC_DE)))
)  
monthplot(diff(log(IPC_DE)), col = "midnightblue")
# Gráfico de autocorrelación simple y parcial diff log
#claro componente estacional cada 12 periodo
grid.arrange(
  ggAcf(diff(IPC_DE),lag.max=64,plot=T,lwd=2,xlab='',main='ACF del difIPC', ylim=c(-1,1)),
  ggPacf(diff(IPC_DE),lag.max=64,plot=T,lwd=2,xlab='',main='PACF del difIPC', ylim=c(-1,1)),
  ggAcf(Diff_log,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC diferenciado log', ylim=c(-1,1)),
  ggPacf(Diff_log,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC diferenciado log', ylim=c(-1,1))
  
)

#Pruebas de raiz unitaria
#El tau me dice si la serie tiene o no al menos una raíz unitaria. 
#El phi3 me dice si la tendecia es significativa
#El phi2 me indica si la deriva es significativa
summary(ur.df(IPC_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'trend'),level = "5pct")
summary(ur.df(IPC_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'drift'),level = "5pct")
#summary(ur.df(IPC_DE,type = 'none'));interp_urdf(ur.df(IPC_DE,type = 'none'),level = "5pct")

#Transformaciones
Diff<-diff(IPC_DE)
Diff_log<- diff(log(IPC_DE))

summary(ur.df(Diff,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'trend'),level = "5pct")
summary(ur.df(Diff,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'drift'),level = "5pct")
#summary(ur.df(Diff,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'none'),level = "5pct")


#Diferencia estacional de la serie anterior 
Diff_s<-diff(Diff_log,lag = 12,differences = 1)
autoplot(Diff_s)

# El comportamiento estacional se mantiene en los gráficos de autocorrelación

grid.arrange(
  ggAcf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC diferenciado estacional'),
  ggPacf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC diferenciado estacional')
)

summary(ur.df(Diff_s,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff_s,type = 'trend'),level = "5pct")
summary(ur.df(Diff_s,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff_s,type = 'drift'),level = "5pct")
summary(ur.df(Diff_s,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff_s,type = 'none'),level = "5pct")

IPC_x11<-seas(diff(log(IPC_DE)),transform.function="none",x11="")
plot(IPC_x11)
IPC_SeasAdj<-seasadj(IPC_x11)
autoplot(IPC_SeasAdj)
autoplot(stl(diff(log(IPC_DE)), s.window = 'periodic')$time.series[,2])

arima<-auto.arima(Diff)
arima1<-arima(diff(log(IPC_DE)),order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12), include.mean=T)
arima2<-arima(diff(log(IPC_DE)),order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12), include.mean=T)

arima3<-Arima(Diff,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12), include.drift=T)

arima4<-arima(diff(log(IPC_DE)),order=c(0,0,2),seasonal=list(order=c(0,1,1),period=12), include.mean=T)

AR=3; MA=3
arma_seleccion = function(AR.m, MA.m){
  for (i in 0:AR) {
    for (j in 0:MA)  {
      fitp <- arima(Diff, order = c(i, 0, j), seasonal=list(order=c(1,0,1),period=12))
      print( c(i, j, AIC(fitp), BIC(fitp)) )
    }
  }
}
arma_seleccion(AR.m, MA.m)

autoplot(cbind(Diff, fitted(arima3))) + scale_x_continuous(limit = c(2000, 2025))

hist(residuals(arima3))
qqnorm(residuals(arima3))
qqline(residuals(arima3))
shapiro.test(residuals(arima3))

grid.arrange(
  ggAcf(residuals(arima3),lag.max=60,plot=T,lwd=2,xlab='',main='ACF de residuos SARIMA'),
  ggPacf(residuals(arima3),lag.max=60,plot=T,lwd=2,xlab='',main='PACF de residuos SARIMA')
)

summary(ur.df(residuals(arima3),type = 'trend', selectlags = 'AIC'))
summary(ur.df(residuals(arima3),type = 'drift', selectlags = 'AIC'))
summary(ur.df(residuals(arima3),type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(residuals(arima3),type = 'none'),level = "5pct")

Box.test(residuals(arima3),type='Ljung-Box',lag=length(IPC_DE)/4)
Box.test(residuals(arima3),type='Box-Pierce',lag=length(IPC_DE)/4)
checkresiduals(arima3, test = "BG")
ArchTest(residuals(arima3), lags = length(IPC_DE)/4)

fore1<-autoplot(forecast::forecast(arima3, level = c(95), h = 7))+
  scale_x_continuous(limit = c(2010, 2025))#+
  #scale_y_continuous(limit = c(4.5, 4.7))
print(fore1)

library(tsoutliers)
library(expsmooth)
library(fma)

## Identify Outliers

outlier.1 <- tsoutliers::tso(Diff,types = c("AO","LS","TC"),maxit.iloop=10, tsmethod = "arima", args.tsmethod = list(order = c(1, 0, 1), seasonal = list(order = c(1, 0, 1))))
outlier.1
plot(outlier.1)

n <- length(Diff)

## Create Outliers Regressors for ARIMAX
## Two type of outliers Level Shift (LS) and Temprory Change (TC)

mo.ls <- outliers("LS", 12)
ls <- outliers.effects(mo.ls, n)

mo.tc <- outliers("TC", 20)
tc <- outliers.effects(mo.tc, n)

xreg.outliers <- cbind(ls,tc)


## Create Arimax using Outliers as regressor variables.

arima.model <- Arima(Diff,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12), include.drift=T,xreg=xreg.outliers)
arima.model

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
autoplot(descompos)


#fac no decae rapidamente, indicios de tendencia 
grid.arrange(
  ggAcf(M2,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del M2'),
  ggPacf(M2,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del M2')
)

# Vamos a aplicar el test de raíz unitaria Dickey-Fuller, el cual nos dice si 
# una serie tiene raíz unitaria o no. La hipótesis nula es que la serie tiene al menos una 
# raíz unitaria (no es estacionaria), mientras la hipótesis alternativa dice que es estacionaria. 

summary(ur.df(M2,type = 'trend'));interp_urdf(ur.df(M2,type = 'trend'),level = "5pct")
#summary(ur.df(M2,type = 'drift'));interp_urdf(ur.df(M2,type = 'drift'),level = "5pct")
#summary(ur.df(M2,type = 'none'));interp_urdf(ur.df(M2,type = 'none'),level = "5pct")

Diff_log_m2<-diff(log(M2))
grid.arrange(
  autoplot(diff(M2)),
  autoplot(diff(log(M2)))
)  

# fac y facp de la serie transformada 
grid.arrange(
  ggAcf(Diff_log_m2,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del dif logM2', ylim=c(-1,1)),
  ggPacf(Diff_log_m2,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del dif logM2', ylim=c(-1,1)),
  ggAcf(diff(M2),lag.max=60,plot=T,lwd=2,xlab='',main='ACF del difM2', ylim=c(-1,1)),
  ggPacf(diff(M2),lag.max=60,plot=T,lwd=2,xlab='',main='PACF del difM2', ylim=c(-1,1))
)

summary(ur.df(Diff_log_m2,type = 'trend'));interp_urdf(ur.df(Diff_log_m2,type = 'trend'),level = "5pct")
summary(ur.df(Diff_log_m2,type = 'drift'));interp_urdf(ur.df(Diff_log_m2,type = 'drift'),level = "5pct")
summary(ur.df(Diff_log_m2,type = 'none'));interp_urdf(ur.df(Diff_log_m2,type = 'none'),level = "5pct")

mar <- 3
mma <- 3
results <- c("p","q","AIC","SBC")
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp <- arima(Diff_log_m2, order = c(i, 0, j), include.mean = TRUE)
    results <- rbind(results,as.numeric(c(i, j, AIC(fitp), BIC(fitp)))) 
  }
}

results
# por criterios de info 
#ARIMA (1,0,3) de serie lm
mod1<-arima(x = Diff_log_m2,order = c(1,0,3))

#ARIMA (2,1,4) de serie lm
mod2<-arima(x = Diff_log_m2,order = c(1,0,4))

#ARIMA (4,1,2) de serie lm
mod3<-arima(x = Diff_log_m2,order = c(3,0,4))

#ARIMA (4,1,4) de serie lm
mod4<-Arima(y = Diff_log_m2,order = c(1,0,1), xreg = cbind(da))

#ARIMA (4,1,4) de serie lm
mod5<-arima(x = Diff_log_m2,order = c(3,0,2))

coeftest(mod2)  
#significativos mod 2,4,5

auto.arima(Diff_log_m2)

autoplot(cbind(Diff_log_m2, fitted(mod4)))

# Validacion del modelo
layout(matrix(1:2, ncol = 2, nrow = 1))
plot(mod5$resid)
acf(mod5$resid, ylim=c(-1,1), main = "FAC de Residuales")

for (i in 1:24) {print( Box.test(resid(mod5), lag=i,  type="Ljung") )}

normalTest(mod5$resid, method="jb")
jarque.bera.test(mod5$residuals)
hist(mod5$residuals)
qqnorm(mod4$residuals)

Diff_log_m2_t<-Diff_log_m2
which.max(Diff_log_m2_t)
Diff_log_m2_t[735]<- NA
which.max(Diff_log_m2_t)
Diff_log_m2_t[736]<- NA
which.max(Diff_log_m2_t)
Diff_log_m2_t[734]<- NA
which.max(Diff_log_m2_t)
Diff_log_m2_t[288]<- NA
which.max(Diff_log_m2_t)
Diff_log_m2_t[631]<- NA
which.max(Diff_log_m2_t)
Diff_log_m2_t[599]<- NA
which.max(Diff_log_m2_t)
Diff_log_m2_t[512]<- NA
which.min(Diff_log_m2_t)
Diff_log_m2_t[133]<- NA
which.min(Diff_log_m2_t)
Diff_log_m2_t[536]<- NA
which.min(Diff_log_m2_t)
Diff_log_m2_t[612]<- NA

da <- ts(rep(0,length(Diff_log_m2)), frequency = 12, start = 1960)
da[735]<-1;da[736]<-1;da[734]<-1
db <- ts(rep(0,length(Diff_log_m2)), frequency = 12, start = 1960)
db[288]<-1;db[631]<-1;db[599]<-1; db[512]<-1; db[133]<-1; db[536]<-1; db[612]<-1

# parentesis
which.max(mod4$resid)
da <- rep(0,length(Diff_log_m2))
da
da[735] <- 1
da
Modelexp1 <- arima(Diff_log_m2, order =c(1,0,2), xreg = da)
for (i in 1:24) {print( Box.test(resid(Modelexp1), lag=i,  type="Ljung") )}


#PRONOSTICO####
fore<-autoplot(forecast::forecast(fitted(mod4), level = c(95), h = 7))+
  scale_x_continuous(limit = c(2010, 2025))#+
#scale_y_continuous(limit = c(4.5, 4.7))
print(fore)


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

#----------X1
grid.arrange(
  ggAcf(x1,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x1'),
  ggPacf(x1,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x1')
)
ur.df(x1, type = 'none', lags = 15, selectlags = 'AIC')

#Como evidenciamos un proceso altamente persistente, es evidente que la serie no es I(0).


#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf.trend_x1= ur.df(x1, type="trend", selectlags = "AIC")
summary(adf.trend_x1) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -2.242
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 2.9481 

#El tau me dice si la serie tiene o no al menos una ra?z unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de ra?z unitaria. 
adf.drift_x1= ur.df(x1, type="drift", selectlags = "AIC")
summary(adf.drift_x1) 
#Los resultados indican que la serie tiene al menos una raíz unitaria.  -2.2752
#El phi2, por su parte, indica que la deriva de la serie no es significativa phi=2.6151

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
adf.none_x1= ur.df(x1, type="none", selectlags = "AIC")
summary(adf.none_x1) 
#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto no se rechaza la hipótesis nula. 
#Noten la importancia de determinar si la serie tiene términos detemrminísticos, pues el valor calculado en cada
#especificación de la prueba cambió de forma importante.

# Resultados x1
interp_urdf(adf.trend_x1,level = "5pct")
interp_urdf(adf.drift_x1,level = "5pct")
interp_urdf(adf.none_x1,level = "5pct")

#----------X2
grid.arrange(
  ggAcf(x2,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x2'),
  ggPacf(x2,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x2')
)


#Como evidenciamos un proceso altamente persistente, es evidente que la serie no es I(0).


#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf.trend_x2= ur.df(x2, type="trend", selectlags = "AIC")
summary(adf.trend_x2) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.9054
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 3.1896 

#El tau me dice si la serie tiene o no al menos una ra?z unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de ra?z unitaria. 
adf.drift_x2= ur.df(x2, type="drift", selectlags = "AIC")
summary(adf.drift_x2) 
#Los resultados indican que la serie tiene al menos una raíz unitaria.  0.0549
#El phi2, por su parte, indica que la deriva NO -es significativa phi=0.6102 

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
adf.none_x2= ur.df(x2, type="none", selectlags = "AIC")
summary(adf.none_x2) 

#Resultados x2
interp_urdf(adf.trend_x2,level = "5pct")
interp_urdf(adf.drift_x2,level = "5pct")
interp_urdf(adf.none_x2,level = "5pct")

#----------X3 
grid.arrange(
  ggAcf(x3,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x3'),
  ggPacf(x3,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x3')
)

#Como evidenciamos un proceso altamente persistente, es evidente que la serie no es I(0).


#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf.trend_x3= ur.df(x3, type="trend", selectlags = "AIC")
summary(adf.trend_x3) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.6528
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 1.4172  

#El tau me dice si la serie tiene o no al menos una ra?z unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de ra?z unitaria. 
adf.drift_x3= ur.df(x3, type="drift", selectlags = "AIC")
summary(adf.drift_x3) 
#Los resultados indican que la serie tiene al menos una raíz unitaria.  0.1126
#El phi2, por su parte, indica que la deriva es significativa phi=27.6628 

#Resultados x3
interp_urdf(adf.trend_x3,level = "5pct")
interp_urdf(adf.drift_x3,level = "5pct")

detach(Data_UR)
#Tercer punto####
Data_coin<-read.csv(file.choose())
Data_coin<- data.frame(Data_coin)
attach(Data_coin)
View(Data_coin)
x1<-ts(x1, start = 1, frequency = 1)
x2<-ts(x2, start = 1, frequency = 1)
x3<-ts(x3, start = 1, frequency = 1)
x4<-ts(x4, start = 1, frequency = 1)
x5<-ts(x5, start = 1, frequency = 1)
t<-X
grid.arrange(
  ggplot(Data_coin, aes(t,x1))+geom_line(colour='Midnightblue'),
  ggplot(Data_coin, aes(t,x4))+geom_line(colour='Midnightblue'),
  ggplot(Data_coin, aes(t,x2))+geom_line(colour='Midnightblue'),
  ggplot(Data_coin, aes(t,x5))+geom_line(colour='Midnightblue'),
  ggplot(Data_coin, aes(t,x3))+geom_line(colour='Midnightblue'))

#----------X1
grid.arrange(
  ggAcf(x1,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x1'),
  ggPacf(x1,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x1')
)


c_adf.trend_x1= ur.df(x1, type="trend", selectlags = "AIC")
summary(c_adf.trend_x1) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.6283
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 1.3664 

c_adf.drift_x1= ur.df(x1, type="drift", selectlags = "AIC")
summary(c_adf.drift_x1) 
#Los resultados indican que la serie tiene al menos una raíz unitaria. -1.0429
#El phi2, por su parte, indica que la deriva de la serie no es significativa phi= 0.9399 

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
c_adf.none_x1= ur.df(x1, type="none", selectlags = "AIC")
summary(c_adf.none_x1) 

# Resultados x1 # Existe raiz unitaria 
interp_urdf(c_adf.trend_x1,level = "5pct")
interp_urdf(c_adf.drift_x1,level = "5pct")
interp_urdf(c_adf.none_x1,level = "5pct")

c_adf.none_diffx1= ur.df(diff(x1), type="none", selectlags = "AIC")
summary(c_adf.none_diffx1) 
interp_urdf(c_adf.none_diffx1,level = "5pct")

#----------X2
grid.arrange(
  ggAcf(x2,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x2'),
  ggPacf(x2,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x2')
)


#Como evidenciamos un proceso altamente persistente, es evidente que la serie no es I(0).

c_adf.trend_x2= ur.df(x2, type="trend", selectlags = "AIC")
summary(c_adf.trend_x2) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.6581
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 1.4109  

c_adf.drift_x2= ur.df(x2, type="drift", selectlags = "AIC")
summary(c_adf.drift_x2) 
#Los resultados indican que la serie tiene al menos una raíz unitaria.  -1.0448
#El phi2, por su parte, indica que la deriva NO -es significativa phi=0.9278 

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
c_adf.none_x2= ur.df(x2, type="none", selectlags = "AIC")
summary(c_adf.none_x2) 

#Resultados x2 # Tiene raiz unitaria
interp_urdf(c_adf.trend_x2,level = "5pct")
interp_urdf(c_adf.drift_x2,level = "5pct")
interp_urdf(c_adf.none_x2,level = "5pct")

c_adf.none_diffx2= ur.df(diff(x2), type="none", selectlags = "AIC")
summary(c_adf.none_diffx2) 
interp_urdf(c_adf.none_diffx2,level = "5pct")


#----------X3 
grid.arrange(
  ggAcf(x3,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x3'),
  ggPacf(x3,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x3')
)

#Como evidenciamos un proceso altamente persistente, es evidente que la serie no es I(0).

c_adf.trend_x3= ur.df(x3, type="trend", selectlags = "AIC")
summary(c_adf.trend_x3) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.4631
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 1.7561 

c_adf.drift_x3= ur.df(x3, type="drift", selectlags = "AIC")
summary(c_adf.drift_x3) 
#Los resultados indican que la serie tiene al menos una raíz unitaria. -1.8426
#El phi2, por su parte, indica que la deriva no es significativa phi=1.9464 
c_adf.none_x3= ur.df(x3, type="none", selectlags = "AIC")
summary(c_adf.none_x3) 
#Resultados x3
interp_urdf(c_adf.trend_x3,level = "5pct")
interp_urdf(c_adf.drift_x3,level = "5pct")
interp_urdf(c_adf.none_x3,level = "5pct")

c_adf.none_diffx3= ur.df(diff(x3), type="none", selectlags = "AIC")
summary(c_adf.none_diffx3) 
interp_urdf(c_adf.none_diffx3,level = "5pct")

#----------X4
grid.arrange(
  ggAcf(x4,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x4'),
  ggPacf(x4,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x4')
)


c_adf.trend_x4= ur.df(x4, type="trend", selectlags = "AIC")
summary(c_adf.trend_x4) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.6398
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 1.4098 

c_adf.drift_x4= ur.df(x4, type="drift", selectlags = "AIC")
summary(c_adf.drift_x4) 
#Los resultados indican que la serie tiene al menos una raíz unitaria. -1.2824 
#El phi2, por su parte, indica que la deriva de la serie no es significativa phi= 1.0062 

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
c_adf.none_x4= ur.df(x4, type="none", selectlags = "AIC")
summary(c_adf.none_x4) 

# Resultados x1 # Existe raiz unitaria 
interp_urdf(c_adf.trend_x4,level = "5pct")
interp_urdf(c_adf.drift_x4,level = "5pct")
interp_urdf(c_adf.none_x4,level = "5pct")

c_adf.none_diffx4= ur.df(diff(x4), type="none", selectlags = "AIC")
summary(c_adf.none_diffx4) 
interp_urdf(c_adf.none_diffx4,level = "5pct")

#----------X5
grid.arrange(
  ggAcf(x5,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de x5'),
  ggPacf(x5,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de x5')
)


c_adf.trend_x5= ur.df(x5, type="trend", selectlags = "AIC")
summary(c_adf.trend_x5) 
#El tau nos dice que la serie tiene al menos una raíz unitaria; tau= -1.6398
#mientras el phi3 nos dice que la tendencia no es significativa. phi3 1.4098 

c_adf.drift_x5= ur.df(x5, type="drift", selectlags = "AIC")
summary(c_adf.drift_x5) 
#Los resultados indican que la serie tiene al menos una raíz unitaria. -1.2824 
#El phi2, por su parte, indica que la deriva de la serie no es significativa phi= 1.0062 

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
c_adf.none_x5= ur.df(x5, type="none", selectlags = "AIC")
summary(c_adf.none_x5) 

# Resultados x1 # Existe raiz unitaria 
interp_urdf(c_adf.trend_x5,level = "5pct")
interp_urdf(c_adf.drift_x5,level = "5pct")
interp_urdf(c_adf.none_x5,level = "5pct")

c_adf.none_diffx5= ur.df(diff(x5), type="none", selectlags = "AIC")
summary(c_adf.none_diffx5) 
interp_urdf(c_adf.none_diffx5,level = "5pct")

#### Todas las series son integradas de orden 1 

#Prueba de cointegración; ninguna serie presenta tendencia clara, en la prueba no se incluye tendencia lineal.
ifelse(coint.test(x1, x2, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x1, x3, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x1, x4, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x1, x5, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x2, x3, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x2, x4, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x2, x5, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x3, x4, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x3, x5, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')
ifelse(coint.test(x4, x5, nlag = 1, output = F)["type 1",'EG']<=-3.35, 
       yes = 'Hay evidencia de cointegración al 5%',no = 'No hay evidencia de cointegración al 5%')

#Evidencia de cointegracion entre x1-x2 y x4-x5
# Metodología engle granger x1-x2

#paso1: regresión y estimar errores
#MCO
cointx1_x2<-lm(x2~x1)
summary(cointx1_x2)
residx1_x2<-cointx1_x2$residuals
#prueba de raiz unitaria para los errores
grid.arrange(
  ggAcf(residx1_x2,lag.max=25,plot=T,lwd=2,xlab='',main='ACF de los Residuos'),
  ggPacf(residx1_x2,lag.max=25,plot=T,lwd=2,xlab='',main='PACF de los Residuos')
)

raizx1_x2<-ur.df(residx1_x2,type="none",selectlags = "AIC")
summary(raizx1_x2)
interp_urdf(raizx1_x2,level = "5pct") 
# los residuos son estacionarios se confirma que las series estan cointegradas

# MCOD 
max.lag=20
mcod_seleccion = function(max.lag){
  for (i in 1:max.lag) {
    mcod <- dynlm(x1 ~ x2+ L(d(x2),-i:i))
    print( c(i, -i, AIC(mcod), BIC(mcod)) )
  }  
}
mcod_seleccion(max.lag=20) #el mejor es 1,-1

MCOD_12 <- dynlm(x2 ~ x1 + L(d(x1),-1:1)); summary(MCOD_12)
DF_MCOD12<-(ur.df(residuals(MCOD_12), type = "none", selectlags = "AIC"))
interp_urdf(DF_MCOD12,level = "5pct") # NO HAY RAIZ UNITARIA 

# VERIFICACIÓN DE SUPUESTOS
grid.arrange(
  ggAcf(residuals(MCOD_12),lag.max=25,plot=T,lwd=2,xlab='',main='ACF de los Residuos'),
  ggPacf(residuals(MCOD_12),lag.max=25,plot=T,lwd=2,xlab='',main='PACF de los Residuos')
)
checkresiduals(MCOD_12)

# Test Box-Pierce para autocorrelaci?n en los residuales
Box.test(residuals(MCOD_12),lag=250, type = c("Box-Pierce")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(MCOD_12),type='Box-Pierce',lag=20) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(MCOD_12),type='Box-Pierce',lag=30) #rechazo H0,  no se cumple el supuesto.

# Test  Ljung-Box para autocorrelaci?n en los residuales.
Box.test(residuals(MCOD_12),lag=250, type = c("Ljung-Box")) #rechazo H0, no se cumple el supuesto.
Box.test(residuals(MCOD_12),type='Ljung-Box',lag=20) #rechazo H0, no se cumple el supuesto.
Box.test(residuals(MCOD_12),type='Ljung-Box',lag=30) #rechazo H0, no se cumple el supuesto.

# Usando errores estandar robustos
mvc12=vcovHC(MCOD_12,method = "arellano") # matrix de var-con consistente a Corr
coeftest(MCOD_12,mvc12) 

# Modelo de correccion d errores
beta_12<- coefficients(dynlm(x1 ~ x2))[2] ;beta_12
vecm_seleccion = function(max.lagind, max.lagdep){
  for (i in 0:max.lagind) {
    for (j in 0:max.lagdep)  {
      vecm <- dynlm(d(x1) ~  L(x1-beta_12*x2) + L(d(x2), 0:i) + L(d(x1), 0:j))
      print( c(i, j, AIC(vecm), BIC(vecm)) )
    }
  }  
}
vecm_seleccion(max.lagind=6, max.lagdep=6)

vecm_seleccion2 = function(max.lagind, max.lagdep){
  for (i in 0:max.lagind) {
    for (j in 0:max.lagdep)  {
      vecm <- dynlm(d(x2) ~  L(x1-beta_12*x2)+ L(d(x2), 0:j) + L(d(x1), 0:i))
      print( c(i, j, AIC(vecm), BIC(vecm)) )
    }
  }  
}
vecm_seleccion2(max.lagind=7, max.lagdep=7)

VECM_12_1 <- dynlm(d(x1) ~  L(x1-beta_12*x2) + L(d(x2), 0) + L(d(x1))) ;summary(VECM_12_1) 

VECM_12_2 <- dynlm(d(x2) ~  L(x1-beta_12*x2,1) + L(d(x2)) + L(d(x1), 0)) 
summary(VECM_12_2)


# VERIFICACIÓN DE SUPUESTOS 1
grid.arrange(
  ggAcf(residuals(VECM_12_1),lag.max=25,plot=T,lwd=2,xlab='',main='ACF de los Residuos'),
  ggPacf(residuals(VECM_12_1),lag.max=25,plot=T,lwd=2,xlab='',main='PACF de los Residuos')
)
checkresiduals(VECM_12_1)
jarque.bera.test(residuals(VECM_12_1))

# Test Box-Pierce para autocorrelaci?n en los residuales
Box.test(residuals(VECM_12_1),lag=250, type = c("Box-Pierce")) 
Box.test(residuals(VECM_12_1),type='Box-Pierce',lag=20)  
Box.test(residuals(VECM_12_1),type='Box-Pierce',lag=30) 

# Test  Ljung-Box para autocorrelaci?n en los residuales.
Box.test(residuals(VECM_12_1),lag=250, type = c("Ljung-Box")) 
Box.test(residuals(VECM_12_1),type='Ljung-Box',lag=20) 
Box.test(residuals(VECM_12_1),type='Ljung-Box',lag=30) 

raiz12_1<-ur.df(residuals(VECM_12_1),type="none",selectlags = "AIC")
summary(raiz12_1)
interp_urdf(raiz12_1,level = "5pct") 

ArchTest(residuals(VECM_12_1), lags = 250) 

# VERIFICACIÓN DE SUPUESTOS 2
grid.arrange(
  ggAcf(residuals (VECM_12_2),lag.max=25,plot=T,lwd=2,xlab='',main='ACF de los Residuos'),
  ggPacf(residuals(VECM_12_2),lag.max=25,plot=T,lwd=2,xlab='',main='PACF de los Residuos')
)
checkresiduals(VECM_12_2)
jarque.bera.test(residuals(VECM_12_2))

# Test Box-Pierce para autocorrelaci?n en los residuales
Box.test(residuals(VECM_12_2),lag=250, type = c("Box-Pierce")) 
Box.test(residuals(VECM_12_2),type='Box-Pierce',lag=20) 
Box.test(residuals(VECM_12_2),type='Box-Pierce',lag=30) 

# Test  Ljung-Box para autocorrelaci?n en los residuales.
Box.test(residuals(VECM_12_2),lag=250, type = c("Ljung-Box")) 
Box.test(residuals(VECM_12_2),type='Ljung-Box',lag=20) 
Box.test(residuals(VECM_12_2),type='Ljung-Box',lag=30)

raiz12_2<-ur.df(residuals(VECM_12_2),type="none",selectlags = "AIC")
summary(raiz12_2)
interp_urdf(raiz12_2,level = "5pct") 

ArchTest(residuals(VECM_12_2), lags = 250) 

# Metodología engle granger x4-x5
#MCO
cointx4_x5<-lm(x4~x5)
summary(cointx4_x5)
#prueba de raiz unitaria para los errores
grid.arrange(
  ggAcf(residuals(cointx4_x5),lag.max=25,plot=T,lwd=2,xlab='',main='ACF de los Residuos'),
  ggPacf(residuals(cointx4_x5),lag.max=25,plot=T,lwd=2,xlab='',main='PACF de los Residuos')
)

ur.residx4_x5<-ur.df(residuals(cointx4_x5),type="none",selectlags = "AIC"); summary(ur.residx4_x5)
interp_urdf(ur.residx4_x5,level = "5pct") 
# los residuos son estacionarios se confirma que las series estan cointegradas

#MCOD
mcod_seleccion = function(max.lag){
  for (i in 1:max.lag) {
      mcod <- dynlm(x4 ~ x5+ L(d(x5),-i:i))
      print( c(i, -i, AIC(mcod), BIC(mcod)) )
  }  
}
mcod_seleccion(max.lag=4) #el mejor es 2,-2

MCOD_45 <- dynlm(x4 ~ x5 + L(d(x5),-2:2)); summary(MCOD_45)
spread_45=x4-x5

autoplot(cbind(residuals(MCOD_45), spread_45))
grid.arrange(
  autoplot(residuals(MCOD_45)),
  ggAcf(residuals(MCOD_45),lag.max=25,plot=T,lwd=2,xlab='',main='ACF de residuos MCOD_45'),
  ggPacf(residuals(MCOD_45),lag.max=25,plot=T,lwd=2,xlab='',main='PACF de residuos MCOD_45')
)
# Usando errores estandar robustos
coeftest(MCOD_45, vcov. = vcovHC(MCOD_45))
#Verificando residuos
summary(ur.df(diff(residuals(MCOD_45)), type="none", selectlags = "AIC"))
checkresiduals(MCOD_45, test = 'BG', lag = 250) #Se rechaza no correlación serial.
checkresiduals(MCOD_45, test = 'LB', lag = 250, plot = F) #Se rechaza no correlación serial.
jarque.bera.test(residuals(MCOD_45)) #No normalidad de los residuos.
ArchTest(residuals(MCOD_45),lags = 25) #Se rechaza homoscedasticidad.
#Necesario corregir por errores.

#Corrección de errores:
beta_45<- coefficients(dynlm(x4 ~ x5))[2] 

vecm_seleccion1 = function(max.lagind, max.lagdep){
  for (i in 0:max.lagind) {
    for (j in 0:max.lagdep)  {
    vecm <- dynlm(d(x4) ~  L(x4-beta_45*x5,1) + L(d(x5), 0:i) + L(d(x4), 0:j))
    print( c(i, j, AIC(vecm), BIC(vecm)) )
    }
  }  
}
vecm_seleccion1(max.lagind=4, max.lagdep=4) #El mejor es con 1 y 1 rezagos en independiente y dependiente

vecm_seleccion2 = function(max.lagind, max.lagdep){
  for (i in 0:max.lagind) {
    for (j in 0:max.lagdep)  {
      vecm <- dynlm(d(x5) ~  L(x4-beta_45*x5,1)+ L(d(x5), 0:j) + L(d(x4), 0:i))
      print( c(i, j, AIC(vecm), BIC(vecm)) )
    }
  }  
}
vecm_seleccion2(max.lagind=4, max.lagdep=4)#El mejor es con 1 y 3 rezagos en independiente y dependiente

VECM_451 <- dynlm(d(x4) ~  L(x4-beta_45*x5) + L(d(x4), 1:4) + L(d(x5),1:2)) ;summary(VECM_451)
VECM_452 <- dynlm(d(x5) ~  L(x4-beta_45*x5)+ L(d(x5), 1:3) + L(d(x4),1)) ;summary(VECM_452) 

# Usando errores estandar robustos
coeftest(VECM_451, vcov. = vcovHC(VECM_451))
# Usando errores estandar robustos
coeftest(VECM_452, vcov. = vcovHC(VECM_452))
#Verificación de residuos
checkresiduals(VECM_451, test = 'BG', lag = 250)
checkresiduals(VECM_451, test = 'LB', lag = 250,plot = F)
jarque.bera.test(residuals(VECM_451))
ArchTest(residuals(VECM_451), lags = 250)
#Se cumplen supuestos de normalidad, homoscedasticidad y autocorrelación: errores estacionarios.
checkresiduals(VECM_452, test = 'BG', lag = 250)
checkresiduals(VECM_452, test = 'LB', lag = 250,plot = F)
jarque.bera.test(residuals(VECM_452))
ArchTest(residuals(VECM_452), lags = 250)
#Se cumplen supuestos de normalidad, homoscedasticidad y autocorrelación: errores estacionarios.
detach(Data_coin)
#Cuarto punto####

