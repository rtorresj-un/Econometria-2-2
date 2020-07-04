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
jarque.bera.test(arima.model2$residuals)
hist(arima.model2$residuals)
qqnorm(arima.model2$residuals)

#PRONOSTICO####
fore<-autoplot(forecast::forecast(fitted(arima.model2), level = c(95), h = 7))+
  scale_x_continuous(limit = c(2010, 2025))#+
#scale_y_continuous(limit = c(4.5, 4.7))
print(fore)


#REAL vs AJUSTADO
autoplot(cbind(Diff_log_m2, fitted(mod4))) + scale_x_continuous(limit = c(1960, 2025))


## Identify Outliers

outlier.2 <- tsoutliers::tso(Diff_log_m2,types = c("AO","LS","TC"),maxit.iloop=10, tsmethod = "arima", args.tsmethod = list(order = c(1, 0, 1)))
outlier.2
plot(outlier.2)

n <- length(Diff_log_m2)

## Create Outliers Regressors for ARIMAX
## Two type of outliers Level Shift (LS) and Temprory Change (TC)

mo.ls <- outliers("LS", 734)
ls <- outliers.effects(mo.ls, n)

mo.tc <- outliers("TC", c(238,536,630,632,735))
tc <- outliers.effects(mo.tc, n)

mo.ao <- outliers("AO", c(133,508,512,599))
ao <- outliers.effects(mo.ao, n)

xreg.outliers <- cbind(ls,tc,ao)


## Create Arimax using Outliers as regressor variables.

arima.model2 <- Arima(Diff_log_m2,order=c(1,0,1), include.drift=T,xreg=xreg.outliers)
arima.model2
