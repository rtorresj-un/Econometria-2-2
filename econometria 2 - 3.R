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
#Consumer Price Index of All Items in Germany, Index 2015=100, Monthly, Not Seasonally Adjusted
Data_1<-read_delim(file.choose(),";", escape_double = FALSE, trim_ws = TRUE)
IPC_DE<-ts(data.frame(Data_1)$IPC_DE[481:724], frequency = 12, start = 2000)
#Descripción de IPC
summary(IPC_DE); kurtosis(IPC_DE)
start(IPC_DE); end(IPC_DE)
#Clara tendencia de la serie
autoplot(IPC_DE,main = "IPC Alemania (enero 1960 - abril 2020)")
monthplot(IPC_DE, col = "midnightblue")
#Serie estacional
descompos = stl(IPC_DE,s.window = "periodic"); autoplot(descompos)

# Autocorrelación simple y parcial serie original.
grid.arrange(
  ggAcf(IPC_DE,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del IPC', ylim=c(-1,1)),
  ggPacf(IPC_DE,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del IPC', ylim=c(-1,1)),
  ggAcf(diff(IPC_DE),lag.max=60,plot=T,lwd=2,xlab='',main='ACF del IPC diferenciado', ylim=c(-1,1)),
  ggPacf(diff(IPC_DE),lag.max=60,plot=T,lwd=2,xlab='',main='PACF del IPC diferenciado', ylim=c(-1,1))
)
#Es necesario diferenciar la serie.
#Transformaciones.
Diff<-diff(IPC_DE)
Diff_log<- diff(log(IPC_DE))
Diff_s<-diff(Diff_log,lag = 12,differences = 1)

# Gráfico serie en primera diferencia y diferencia de los log
#Al aplicar log se logra estabilizar la varianza
grid.arrange(
  autoplot(Diff, col = "midnightblue"),
  autoplot(Diff_log, col = "midnightblue"), 
  autoplot(Diff_s, col = "midnightblue") 
)  
monthplot(Diff, col = "midnightblue")

# Gráfico de autocorrelación simple y parcial diff log
#claro componente estacional cada 12 periodo
grid.arrange(
  ggAcf(Diff,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del difIPC', ylim=c(-1,1)),
  ggPacf(Diff,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del difIPC', ylim=c(-1,1)),
  ggAcf(Diff_log,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC diferenciado log', ylim=c(-1,1)), #el logaritmo no aporta mejoría a la autocorrelación de la serie
  ggPacf(Diff_log,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC diferenciado log', ylim=c(-1,1)),
  ggAcf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del IPC diferenciado estacional', ylim=c(-1,1)), #diferenciar la serie mejora la autocorrelación de la serie pero puede no ser necesario 
  ggPacf(Diff_s,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del IPC diferenciado estacional', ylim=c(-1,1)) #porque el efecto persiste, puede corregirse con un SARIMA(1,0,1)(1,0,1)[12]
)

#Pruebas de raiz unitaria
summary(ur.df(IPC_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'trend'),level = "5pct")
summary(ur.df(IPC_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
#summary(ur.df(IPC_DE,type = 'none'));interp_urdf(ur.df(IPC_DE,type = 'none'),level = "5pct")

summary(ur.df(Diff,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'trend'),level = "5pct")
summary(ur.df(Diff,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
#summary(ur.df(Diff,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'none'),level = "5pct")

summary(ur.df(Diff_s,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff_s,type = 'trend'),level = "5pct")
summary(ur.df(Diff_s,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff_s,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
summary(ur.df(Diff_s,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff_s,type = 'none'),level = "5pct") 

mar <- 3
mma <- 3
results1 <- c("p","q","AIC","BIC")
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp <- Arima(Diff, order = c(i, 0, j), seasonal=list(order=c(1,0,1),period=12), include.drift=T)
    results1 <- rbind(results1,as.numeric(c(i, j, AIC(fitp), BIC(fitp)))) 
  }
}
results1

sarima<-Arima(Diff,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12), include.drift=T) #modelo elegido por AIC y alto BIC
sarima1<-Arima(Diff,order=c(1,0,0),seasonal=list(order=c(1,1,1),period=12), include.drift=T)
sarima2<-Arima(Diff,order=c(0,0,0),seasonal=list(order=c(1,0,1),period=12), include.drift=T)

#Verificación de errores
grid.arrange(
  ggAcf(residuals(sarima),lag.max=60,plot=T,lwd=2,xlab='',main='ACF de residuos SARIMA'),
  ggPacf(residuals(sarima),lag.max=60,plot=T,lwd=2,xlab='',main='PACF de residuos SARIMA')
)
summary(ur.df(residuals(sarima),type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(residuals(arima1),type = 'none'),level = "5pct") #estacionarios

ArchTest(residuals(sarima), lags = length(IPC_DE)/4) #homoscedasticos
Box.test(residuals(sarima),type='Box-Pierce',lag=length(IPC_DE)/4)
checkresiduals(sarima, test = "LB") #sin correlación serial
qqnorm(residuals(sarima)); qqline(residuals(sarima)) 
shapiro.test(residuals(sarima)) #Normalmente distribuidos
jarque.bera.test(residuals(sarima))

#Ajustado vs. Observado
autoplot(cbind(Diff, fitted(sarima)), main='Estimado y observado') + scale_x_continuous(limit = c(2000, 2020))
#Pronóstico
fore1<-autoplot(forecast::forecast(sarima, level = c(95), h = 7), main='Pronóstico hasta noviembre de 2020', ylab='Cambio del IPC', xlab='')+
  scale_x_continuous(limit = c(2015, 2025))
print(fore1)

### Identify Outliers
#outlier.1 <- tsoutliers::tso(Diff,types = c("AO","LS","TC"),maxit.iloop=10, tsmethod = "auto.arima")
#outlier.1
#plot(outlier.1)
#n <- length(Diff)

## Create Outliers Regressors for ARIMAX
## Two type of outliers Level Shift (LS) and Temprory Change (TC)

#mo.ao <- outliers("AO", c(21,36,351))
#ao <- outliers.effects(mo.ao, n)
#xreg.outliers1 <- cbind(ao)

## Create Arimax using Outliers as regressor variables.
#sarimax <- Arima(Diff,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12), include.drift=T,xreg=xreg.outliers1)
#sarimax

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
mcod_seleccion(max.lag=20) #el mejor es 1,-1 por ambos criterios

MCOD_12 <- dynlm(x1 ~ x2 + L(d(x2),-1:1)); summary(MCOD_12)
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
      vecm <- dynlm(d(x1) ~  L(x1-beta_12*x2) + L(d(x1), 1:i) + L(d(x2), 0:j))
      print( c(i, j, AIC(vecm), BIC(vecm)) )
    }
  }  
}
vecm_seleccion(max.lagind=6, max.lagdep=6)

vecm_seleccion2 = function(max.lagind, max.lagdep){
  for (i in 0:max.lagind) {
    for (j in 1:max.lagdep)  {
      vecm <- dynlm(d(x2) ~  L(x1-beta_12*x2,1)+ L(d(x1), 0:i) + L(d(x2), 1:j))
      print( c(i, j, AIC(vecm), BIC(vecm)) )
    }
  }  
}
vecm_seleccion2(max.lagind=7, max.lagdep=7)

VECM_12_1 <- dynlm(d(x1) ~  L(x1-beta_12*x2) + L(d(x2), 0:1)) ; BIC(VECM_12_1)
summary(VECM_12_1) # por criterios de información, empeora al incluir rezagos de x1


VECM_12_2 <- dynlm(d(x2) ~  L(x1-beta_12*x2,1) +L(d(x1),0:4) ) ;BIC(VECM_12_2)
summary(VECM_12_2)


# VERIFICACIÓN DE SUPUESTOS 1 ####
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


# VERIFICACIÓN DE SUPUESTOS 2 ####
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

VECM_451 <- dynlm(d(x4) ~  L(x4-beta_45*x5) + L(d(x4), 1) + L(d(x5),1)) ;summary(VECM_451)
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

