#Taller 2 - R Avanzado#
install.packages("tseries")
install.packages("urca")
install.packages("fBasics")
library(tseries)
library(urca)
library(stats)
library(lmtest)
library(urca)
library(fBasics)

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

ar_ipc<- arima(x = dipc,order = c(4,0,3))
coeftest(ar_ipc)

plot(ar_ipc$resid)

for (i in 1:10) {print( Box.test(resid(ar_ipc), lag=i,  type="Ljung") )}

hist(ar_ipc$residuals)
tsdiag(ar_ipc)
normalTest(ar_ipc$resid, method="jb")
