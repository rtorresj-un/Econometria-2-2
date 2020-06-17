#-----------------------------
Universidad Nacional de Colombia
Taller 2 - R Avanzado

Juanita Cortes
Raúl Torres
#----------------------------
library(tseries)
library(urca)
library(stats)
library(lmtest)
library(urca)
library(fBasics)
library(forecast)
library(ggplot2)
library(haven)
library(readxl)

#1. Series de tiempo####
IPC <- read_excel(file.choose(), 
                  col_names = FALSE)
IPC
ipc=ts(IPC$...1,start = c(2008,1),frequency = 12)
summary(ipc)  #Estadística descriptiva

#Graficar la serie temporal
plot(ipc,main="IPC")
abline(lm(ipc~time(ipc)))

descompos = stl(ipc,s.window = "periodic")
plot(descompos)

par(mfrow=c(1,2))
acf(ipc)
pacf(ipc)
Box.test(ipc, lag = 10, type = c ("Ljung-Box")) #Ho: No correlación serial
dev.off()
plot(diff(ipc, 2))

#Prueba Dickey-Fuller
adf.test(ipc,alternative = "stationary")

ru_2 <- ur.df(diff(ipc, 2), type = "drift", selectlags = c("AIC"))
summary(ru_2)

#Prueba Phillip Perron
pptest1<-ur.pp(ipc,model=c("trend"), type=c("Z-tau"))
summary(pptest1)

pptest2<-ur.pp(diff(ipc,2),model=c("constant"), type=c("Z-tau"))
summary(pptest2)

#Prueba kpss
kpss1<-ur.kpss(ipc, type=c("tau"))
summary(kpss1)

kpss2<-ur.kpss(diff(ipc,2), type=c("tau"))
summary(kpss2)

par(mfrow=c(1,2))
acf(diff(ipc,2))
pacf(diff(ipc,2))
dev.off()

#ELECCIÓN DEL MEJOR MODELO - AIC

dipc=diff(ipc, 2) 

# Estimación de modelos

mar <- 4
mma <- 4
results <- c("p","q","AIC","SBC")
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp <- arima(dipc, order = c(i, 0, j), include.mean = TRUE)
    results <- rbind(results,as.numeric(c(i, j, AIC(fitp), BIC(fitp)))) 
  }
}; results

#Modelo final
ar_ipc<- arima(x = dipc,order = c(4,0,3))
coeftest(ar_ipc)

#Validación del modelo
par(mfrow=c(1,2))
acf(ar_ipc$residuals,ylim=c(-1,1), main = "FAC de Residuales")
pacf(ar_ipc$residuals,ylim=c(-1,1), main = "FACP de Residuales")
dev.off()

for (i in 1:10) {print( Box.test(resid(ar_ipc), lag=i,  type="Ljung") )}

#Verificación de residuos, ruido blanco y normalidad.
hist(ar_ipc$residuals)
tsdiag(ar_ipc)
normalTest(ar_ipc$resid, method="jb")

#Pronóstico.
pred=forecast(ar_ipc,h=12)
autoplot(pred)

#Real vs. ajustado
ts.plot(dipc,pred$fitted,col=c("black","red"))


#2.Elección binaria ####
seguro <- read_dta(file.choose())
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

stargazer::stargazer(olsreg,logit,probit,type = "html")

# Efectos marginales promedio del Modelo de Probabilidad Lineal
coef(olsreg)

# Efectos marginales promedio del modelo Logit
LogitScalar <- mean(dlogis(predict(logit)))
LogitScalar * coef(logit)

# Efectos marginales promedio del modelo Probit
ProbitScalar <- mean(dnorm(predict(probit)))
ProbitScalar * coef(probit)

#3. Datos panel ####
Crime <- read_excel(file.choose())
Crime<- pdata.frame(Crime)

#Pooled
pool<- plm(crmrte~ prbarr+prbconv+prbpris+polpc+density+taxpc+west+central+urban, model="pooling", data=Crime)
summary(pool)
#Efectos fijos
fixed<- plm(crmrte~ prbarr+prbconv+prbpris+polpc+density+taxpc+west+central+urban, model="within", data=Crime)
summary(fixed)
#Efectos aleatorios
random<- plm(crmrte~ prbarr+prbconv+prbpris+polpc+density+taxpc+west+central+urban, model="random", data=Crime)
summary(random)

stargazer::stargazer(pool, fixed, random, type='html')

#Test de multiplicadores de Lagrange de Breusch - Pagan; RHo: hay efectos temporales, se utiliza efectos aleatorios.
plmtest(pool,type = c("bp"))

#Test de Hausman; Ho: diferencia no significativa, mejor efectos aleatorios.
phtest(fixed, random)



