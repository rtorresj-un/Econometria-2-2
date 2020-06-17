#-----------------------------
Universidad Nacional de Colombia
Taller 2 - R Avanzado

Juanita Cortes
Raúl Torres
#----------------------------

install.packages("tseries")
install.packages("urca")
install.packages("fBasics")
library(tseries)
library(urca)
library(stats)
library(lmtest)
library(urca)
library(fBasics)
library(forecast)
library(ggplot2)


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

par(mfrow=c(1,2))
acf(ipc)
pacf(ipc)
Box.test(ipc, lag = 10, type = c ("Ljung-Box")) #Ho: No correlación serial

plot(diff(ipc))

#Prueba Dickey-Fuller
adf.test(diff(ipc, 2),alternative = "stationary")

ru_1 <- ur.df(diff(ipc, 2), type = "drift", selectlags = c("AIC"))
summary(ru_1)

#Prueba Phillip Perron
pptest<-ur.pp(ipc,model=c("trend"), type=c("Z-tau"))
summary(pptest)

pptest2<-ur.pp(diff(ipc),model=c("constant"), type=c("Z-tau"))

summary(pptest2)

#La prueba Kpss tiene las hip?tesis al reves
kpss<-ur.kpss(ipc, type=c("tau"))
summary(kpss)

kpss2<-ur.kpss(diff(ipc), type=c("tau"))
summary(kpss2)

par(mfrow=c(1,2))
acf(diff(ipc))
pacf(diff(ipc))

#ELECCI?N DEL MEJOR MODELO (SEG?N AIC) el menor indce es el mejor modelo

dipc=diff(ipc, 2) 

arima(x = dipc,order = c(1,0,0))

arima(x = dipc,order = c(2,0,4))

arima(x = dipc,order = c(4,0,3))

# Estimaci?n de modelos

mar <- 4
mma <- 4
results <- c("p","q","AIC","SBC")
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp <- arima(dipc, order = c(i, 0, j), include.mean = TRUE)
    results <- rbind(results,as.numeric(c(i, j, AIC(fitp), BIC(fitp)))) 
  }
}

results

#Modelo final ########
ar_ipc<- arima(x = dipc,order = c(4,0,3))
coeftest(ar_ipc)

#validación del modelo

plot(ar_ipc$resid)
par(mfrow=c(1,2))
acf(ar_ipc$residuals,ylim=c(-1,1), main = "FAC de Residuales")
pacf(ar_ipc$residuals,ylim=c(-1,1), main = "FACP de Residuales")

for (i in 1:10) {print( Box.test(resid(ar_ipc), lag=i,  type="Ljung") )}

#Verificacion de residuos, ruido blanco y normalidad
hist(ar_ipc$residuals)
tsdiag(ar_ipc)
normalTest(ar_ipc$resid, method="jb")

#Pronostico ######
pred=forecast(ar_ipc,h=12)
autoplot(pred)

#REAL vs AJUSTADO
ts.plot(dipc,pred$fitted,col=c("black","red"))


#2.Elección binaria ####
library(readxl)
seguro <- read_dta("seguro.dta")
attach(seguro)
summary(seguro)
X <- cbind(retire, age, hstatusg, hhincome, educyear, married, hisp)
summary(ins)

# Modelo de Probabilidad Lineal
olsreg <- lm(ins ~ X)
summary(olsreg)

# Modelo Logit
logit<- glm(ins ~ X, family=binomial (link = "logit"))
summary(logit) 

# Modelo Probit
probit<- glm(ins ~ X, family=binomial (link="probit"))
summary(probit)

stargazer::stargazer(olsreg,logit,probit,type = "text")

# Efectos marginales del Modelo de Probabilidad Lineal
coef(olsreg)

# Efectos marginales promedio del modelo Logit
LogitScalar <- mean(dlogis(predict(logit)))
LogitScalar * coef(logit)

# Efectos marginales promedio del modelo Probit
ProbitScalar <- mean(dnorm(predict(probit)))
ProbitScalar * coef(probit)

#3. Datos panel ####
