#Taller 4. Grupo 28- Juanita Cortes, David Orozco y Raúl Torres  
#######Paquetes
install.packages("vars")
install.packages("svars")
install.packages('tsDyn')
library(vars);library(urca);library(ggplot2); library(tsDyn)
library(ggfortify);library(gridExtra);library(dplyr)
library(tidyverse);library(svars);library(AER); library(ggthemes)
library(dynlm);library(readr);library(tsDyn);library(VAR.etp);library(forecast)

####EJECUTAR ESTAS FUNCIONES################################################
# This R function helps to interpret the output of the urca::ur.df function.
interp_urdf <- function(urdf, level) {
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
#Función para graficar impulso respuesta con bootstraping
irf_ggplot<-function(VAR, impulso, respuesta){
        IRF = irf(VAR, impulse=impulso ,response=respuesta,n.ahead = 15,ci=0.95, boot=T, ortho=T, runs=100) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);  
        data_irf= data.frame(IRF$irf,IRF$Lower,IRF$Upper, c(0:15))
        ggplot(data_irf, aes(x=data_irf[,4], y=data_irf[,1])) +
                geom_line() + 
                geom_ribbon(aes(ymin=data_irf[,2], ymax=data_irf[,3], fill="Bandas al 95% \n de confianza"), alpha=.3) +
                theme_minimal() + scale_color_distiller() + scale_fill_ordinal(name='') +
                ylab("Porcentaje de cambio") +
                xlab("Pasos adelante") + ggtitle(str_c('Respuesta de ',as.character(colnames(data_irf)[1]), ' ante cambios en ', impulso))
}
############################################################################

####Punto 1####
#Harmonised Index of Consumer Prices (HICP)
Data_1 <- read_csv(file.choose())
attach(Data_1)
Date1<-as.Date(Date, format = '%d/%m/%Y')
IPC_DE<-ts(data.frame(Data_1)$IPC_DE, frequency = 12, start = 2000)

#Descripción de IPC
summary(IPC_DE); kurtosis(IPC_DE)
start(IPC_DE); end(IPC_DE)
#Clara tendencia de la serie
autoplot(IPC_DE,main = "IPC Alemania (enero 2000 - mayo 2020)")
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
Diff_ipc<- diff(IPC_DE)
autoplot(Diff_ipc)
monthplot(Diff_ipc, col = "midnightblue")

#Pruebas de raiz unitaria
summary(ur.df(IPC_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'trend'),level = "5pct")
summary(ur.df(IPC_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
summary(ur.df(IPC_DE,type = 'none'));interp_urdf(ur.df(IPC_DE,type = 'none'),level = "5pct")

summary(ur.df(Diff_ipc,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff_ipc,type = 'trend'),level = "5pct")
summary(ur.df(Diff_ipc,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff_ipc,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
summary(ur.df(Diff_ipc,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff_ipc,type = 'none'),level = "5pct")

# Serie tiene tendencia lineal, se realiza la prueba df con tendencia y no hay presencia de raiz unitaria 

# Harmonized Unemployment Rate: Total: All Persons for Germany, Percent, Seasonally Adjusted
D_DE<-ts(data.frame(Data_1)$UN_DE, frequency = 12, start = 2000)
#Descripción de IPC
summary(D_DE); kurtosis(D_DE)
start(D_DE); end(D_DE)

autoplot(D_DE,main = "Desempleo Alemania (enero 1991 - abril 2020)")
monthplot(D_DE, col = "midnightblue")

#Serie estacional
descompos = stl(D_DE,s.window = "periodic"); autoplot(descompos)

# Autocorrelación simple y parcial serie original.
grid.arrange(
        ggAcf(D_DE,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del Desempleo', ylim=c(-1,1)),
        ggPacf(D_DE,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del Desempleo', ylim=c(-1,1)),
        ggAcf(diff(D_DE),lag.max=60,plot=T,lwd=2,xlab='',main='ACF del Desempleo diferenciado', ylim=c(-1,1)),
        ggPacf(diff(D_DE),lag.max=60,plot=T,lwd=2,xlab='',main='PACF del Desempleo diferenciado', ylim=c(-1,1))
)
#Es necesario diferenciar la serie.
#Transformaciones.
Diff_DE<-diff(D_DE)

# Gráfico serie en primera diferencia y diferencia de los log
#Al aplicar log se logra estabilizar la varianza
autoplot(Diff_DE, colour = 'midnightblue')
monthplot(Diff_DE, col = "midnightblue")

#Pruebas de raiz unitaria
summary(ur.df(D_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(D_DE,type = 'trend'),level = "5pct")
summary(ur.df(D_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(D_DE,type = 'drift'),level = "5pct") 
summary(ur.df(D_DE,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(D_DE,type = 'none'),level = "5pct") #caminata aleatoria 

summary(ur.df(Diff_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff_DE,type = 'trend'),level = "5pct")
summary(ur.df(Diff_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff_DE,type = 'drift'),level = "5pct")
summary(ur.df(Diff_DE,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff_DE,type = 'none'),level = "5pct")
mean(Diff)

GER<-cbind(Diff_ipc, Diff_DE)
VARselect(GER, lag.max=20,type = "none", season = NULL) # estimar var 2 o 7 
VARselect(GER, lag.max=20,type = "const", season = NULL)# estimar var 3 o 2

#VAR con sólo intercepto.
VAR.1= VAR(GER, p=3, type="const", season=NULL) 
summary(VAR.1) #El intercepto es significativo en una ecuación.
roots(VAR.1)
Acoef(VAR.1)
#VAR sin términos determinísticos.
VAR.no.1 = VAR(GER, p=3, type="none", season=NULL)  
summary(VAR.no.1)
roots(VAR.no.1)
Acoef(VAR.no.1)

P.75.1=serial.test(VAR.1, lags.pt = 50, type = "PT.asymptotic");P.75.1 #No rechazo, se cumple el supuesto
P.30.1=serial.test(VAR.1, lags.pt = 30, type = "PT.asymptotic");P.30.1 #No rechazo, se cumple el supuesto
P.20.1=serial.test(VAR.1, lags.pt = 20, type = "PT.asymptotic");P.20.1  #No rechazo, se cumple el supuesto

plot(P.20.1, names = "Diff_ipc") #Relativamente Bien comportados, salvo por normalidad
plot(P.20.1, names = "Diff_DE") #Relativamente Bien comportados, salvo por normalidad.

#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VAR.1, lags.multi = 50, multivariate.only = TRUE)
arch.test(VAR.1, lags.multi = 24, multivariate.only = TRUE) #se cumple el supuesto.
arch.test(VAR.1, lags.multi = 12, multivariate.only = TRUE) #se cumple el supuesto

##Test Jarque-Bera multivariado
normality.test(VAR.1) #rechazo, no se cumple el supuesto. 
autoplot(predict(VAR.1, n.ahead = 12)) 

grid.arrange(
        irf_ggplot(VAR.1, 'Diff_DE', 'Diff_DE'),
        irf_ggplot(VAR.1, 'Diff_DE', 'Diff_ipc'), ncol=2
)

causality(VAR.1, 'Diff_DE')
causality(VAR.1, 'Diff_ipc')

A.mat <- diag(2)
A.mat[1,1] <- NA # El elemento α11 es diferente de cero. .
A.mat[1,2] <- NA #El elemento α21 es diferente de cero, de manera que la primera variable afecta de forma contemporánea a la segunda.
A.mat[2,2] <- NA # El elemento α22 es diferente de cero.
print(A.mat) 

SVAR.1<-SVAR(VAR.1,Amat = A.mat,Bmat = NULL, estmethod = "scoring")
summary(SVAR.1)

grid.arrange(
        irf_ggplot(SVAR.1, 'Diff_DE', 'Diff_DE'),
        irf_ggplot(SVAR.1, 'Diff_DE', 'Diff_ipc'), ncol=2
)


####Punto 2####
Datos_28<-read.csv(file.choose())
Y_1<-ts(Datos_28$y1)
Y_2<-ts(Datos_28$y2)
##Descripción de las series
summary(Datos_28)
autoplot(Y_1)
autoplot(Y_2)
var(Y_1)
var(Y_2)
## vemos que la Y_2 varía mucho más que Y_1
##Las dos series en la misma gráfica 
plot_28<-data.frame(time=1:length(Y_1), variable1 = Y_1, variable2=Y_2)
ggplot(plot_28,aes(time,variable1)) + geom_line(aes(color="Y_1")) +
  geom_line(aes(y=variable2, color="Y_2")) +
  theme_minimal() + xlab('') + ylab('') + scale_color_manual(values=c("#AD0202AD", "#030B6EBE")) +
  labs(color='Series') + theme(legend.position="bottom")

##Pruebas de raíz unitaria
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "trend")); interp_urdf(ur.df(Y_1,type = 'trend'),level = "5pct")
## La tendencia y deriva son significativas, aunque graficamente parece no tener tendencia 

##por tanto hacemos ambas pruebas 
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "drift")); interp_urdf(ur.df(Y_1,type = 'drift'),level = "5pct")
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "none")); interp_urdf(ur.df(Y_1,type = 'none'),level = "5pct")
##confirmamos que la serie es I(0)
summary(ur.df(Y_2, lags=8, selectlags = "AIC", type = "trend")); interp_urdf(ur.df(Y_2,type = 'trend'),level = "5pct")
## La tendencia y deriva son significativas, aunque graficamente parece no tener tendencia 
##por tanto hacemos ambas pruebas 
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "drift")); interp_urdf(ur.df(Y_2,type = 'drift'),level = "5pct")
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "none")); interp_urdf(ur.df(Y_2,type = 'none'),level = "5pct")
##confirmamos que la serie es I(0)

##Creamos modelo VAR
Y<-cbind(Y_1,Y_2)
VARselect(Y, lag.max = 8, type = "both", season = NULL)## 3 rezagos
VARselect(Y, lag.max = 8, type = "const")##3 rezagos
VARselect(Y, lag.max = 8, type= "none")## 3 rezagos
##Utilizaremos 3 rezagos para el VAR
VAR.2.tryco<-VAR(Y, p=3, type = "both")
summary(VAR.2.tryco)

VAR.2.co<-VAR(Y, p=3, type = "const")
summary(VAR.2.co)
##La tendencia y la constante no son significativas
VAR.2<-VAR(Y, p=3, type = "none")
summary(VAR.2)
##validación de supuestos 
##Estabilidad del proceso
roots(VAR.2)
##El proceso es estable
##Autocorrelación 
P.75=serial.test(VAR.2, lags.pt = 75, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR.2, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR.2, lags.pt = 20, type = "PT.asymptotic");P.20  #No rechazo, se cumple el supuesto
P.10=serial.test(VAR.2, lags.pt = 10, type = "PT.asymptotic");P.10 #No rechazo, se cumple el supuesto
stargazer(P.75,P.30,P.20,P.10, c("70","30","20","10"))#Vemos las gráficas ACF y PACF

plot(P.30, names = "Y_1") 
plot(P.30, names = "Y_2") 
##Validamos el supuesto de homoscedasticidad 
arch.test(VAR.2, lags.multi = 24, multivariate.only = TRUE) #No rechazo, se cumple el supuesto.
arch.test(VAR.2, lags.multi = 12, multivariate.only = TRUE) #No rechazo, se cumple el supuesto
##Válidamos el supuesto de normalidad
normality.test(VAR.2)
##Pronóstico 
fore<-predict(VAR.2, n.ahead=5, ci=.95)

fore2<-autoplot(fore, ylab='', xlab='')+
  scale_x_continuous(limit = c(450, 520)) + theme_minimal() + scale_color_stata()
print(fore2)

##Modelo SVAR
##unimos las dos series en una matriz
##Creamos la matriz de efectos contemporaneos 
matrix_1=as.matrix(cbind(c(1,NA),c(NA,1)))
matrix_1
SVAR.2<-SVAR(VAR.2, Amat = matrix_1, Bmat = NULL, hessian = TRUE, method="scoring" )
summary(SVAR.2)
##Ya queda definido el modelo con efectos contemporaneos 
##Impulso respuesta 

grid.arrange(
        irf_ggplot(SVAR.2, 'Y_1', 'Y_1'),
        irf_ggplot(SVAR.2, 'Y_1', 'Y_2'), 
        irf_ggplot(SVAR.2, 'Y_2', 'Y_1'),
        irf_ggplot(SVAR.2, 'Y_2', 'Y_2'), ncol=2
)

## Descomposición de la varianza
vardecom.2<-fevd(SVAR.2, n.ahead = 24)
plot(vardecom.2,col=c("red", "green"))
##Obteniendo los parametros etimados del SVAR
##Creamos las matrices con los coeficientes de los rezagos del VAR
#T_0: No hay interceptos
#T_1:
T_1<-as.matrix(cbind(c(-0.18291,-0.44293),c(-0.09498, -0.36287)))
stargazer(T_1)
#T_2
T_2<-as.matrix(cbind(c(-0.20118,0.07185),c(-0.34974,0.050365)))
T_2
#T_3
T_3<-as.matrix(cbind(c(-0.04237,0.02078),c(-0.05186,-0.16564)))
T_3
##Inversa de A 
InvA<-solve(SVAR.2$A)
InvA
##Obtenemos las matrices del VAR en forma reducida
#a_1
a_1<-InvA%*%T_1
a_1
#a_2
a_2<-InvA%*%T_2
a_2
#a_3
a_3<-InvA%*%T_3
a_3
####EL VAR EN FORMA REDUCIDA QUEDA:
Er<-InvA%*%SVAR.2$Sigma.U%*%InvA 
Er
Y~a_1*L(Y, 1)+a_2*L(Y,2)+a_3*L(Y,3)+Er

####Punto 3####
BG<-data.frame(BG_3<- read.csv(file.choose(),sep=";"))
i_3m<-ts(BG$i_3m, start=1996, frequency = 12)
i_1y<-ts(BG$i_1y, start=1996, frequency = 12)
Date_4<-as.Date(BG$Date, format = '%d/%m/%y')

spreadplot<-data.frame(time=Date_4, variable = c(i_1y-i_3m))
ggplot(spreadplot,aes(time,variable)) + geom_line(aes(color="Spread 1Y-3M")) +
        geom_line(data = BG, aes(Date_4, i_3m, color="3M")) +
        geom_line(data= BG, aes(Date_4, i_1y, color="1Y"))+
        geom_ribbon(aes(ymin = i_3m, ymax = i_1y, fill="Spread 1Y-3M"), alpha = .3) + 
        theme_minimal() + scale_color_stata() + xlab('') + ylab('')+
        labs(color='Series') + scale_fill_stata(name='') +
        theme(legend.position="bottom")

datalogdifplot<-data.frame(time=Date_4[2:294], variable1=log(i_1y)[2:294], variable2=log(i_3m)[2:294], variable3=diff(i_1y), variable4=diff(i_3m))
datalogdifplot2<-data.frame(time=Date_4[2:294], variable1=diff(log(i_1y)), variable2=diff(log(i_3m)))

gralogdif1<-ggplot(datalogdifplot,aes(time,variable1)) +
        geom_line(aes(time, variable4, colour="∆3M")) +
        geom_line(aes(time, variable3, colour="∆1Y")) +
        theme_minimal() + xlab('') + ylab('') + scale_color_manual(values=c("#AD0202AD", "#030B6EBE")) +
        labs(color='Series')
gralogdif2<-ggplot(datalogdifplot,aes(time,variable1)) +
        geom_line(aes(time, variable2, colour="log3M")) +
        geom_line(aes(colour="log1Y")) + 
        theme_minimal() + xlab('') + ylab('') + scale_color_manual(values=c("#AD0202AD", "#030B6EBE")) +
        labs(color='Series')
gralogdif3<-ggplot(datalogdifplot2,aes(time,variable1)) +
        geom_line(aes(time, variable2, colour="∆log3M"), ) +
        geom_line(aes(colour="∆log1Y")) + 
        theme_minimal() + xlab('') + ylab('') + scale_color_manual(values=c("#AD0202AD", "#030B6EBE")) +
        labs(color='Series')

grid.arrange(gralogdif2, gralogdif1, gralogdif3, ncol=3)

## Pruebas de raíz unitaria 
grid.arrange(
        ggAcf(i_3m,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del interés a 3 meses', ylim=c(-1,1)),
        ggPacf(i_3m,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del interés a 3 meses', ylim=c(-1,1)),
        ggAcf(i_1y,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del interés a 1 año', ylim=c(-1,1)),
        ggPacf(i_1y,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del interés a 1 año', ylim=c(-1,1)),
        ggAcf(diff(i_3m),lag.max=60,plot=T,lwd=2,xlab='',main='ACF del interés a 3 meses diferenciado', ylim=c(-1,1)),
        ggPacf(diff(i_3m),lag.max=60,plot=T,lwd=2,xlab='',main='PACF del interés a 3 meses diferenciado', ylim=c(-1,1)),
        ggAcf(diff(i_1y),lag.max=60,plot=T,lwd=2,xlab='',main='ACF del interés a 1 año diferenciado', ylim=c(-1,1)),
        ggPacf(diff(i_1y),lag.max=60,plot=T,lwd=2,xlab='',main='PACF del interés a 1 año diferenciado', ylim=c(-1,1)), 
        ncol=2)

summary(ur.df(i_3m,type = 'trend', selectlags = 'AIC')); interp_urdf(ur.df(i_3m,type = 'trend', selectlags = 'AIC'),level = "5pct")
summary(ur.df(i_3m,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(i_3m,type = 'drift', selectlags = 'AIC'),level = "5pct")
summary(ur.df(i_3m,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(i_3m,type = 'none', selectlags = 'AIC'),level = "5pct")
summary(ur.df(diff(i_3m),type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(diff(i_3m),type = 'none', selectlags = 'AIC'),level = "5pct")

summary(ur.df(i_1y,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(i_1y,type = 'trend', selectlags = 'AIC'),level = "5pct")
summary(ur.df(i_1y,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(i_1y,type = 'drift', selectlags = 'AIC'),level = "5pct")
summary(ur.df(i_1y,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(i_1y,type = 'none', selectlags = 'AIC'),level = "5pct")
summary(ur.df(diff(i_1y),type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(diff(i_1y),type = 'none', selectlags = 'AIC'),level = "5pct")

summary(ur.pp(i_3m,model=c("trend"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(i_3m,model=c("constant"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(i_1y,model=c("trend"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(i_1y,model=c("constant"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(diff(i_3m),model=c("trend"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(diff(i_3m),model=c("constant"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(diff(i_1y),model=c("trend"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad
summary(ur.pp(diff(i_1y),model=c("constant"), type=c("Z-tau"), use.lag = 10))#ho: no estacionariedad

summary(ur.kpss(i_3m, type=c("tau"), use.lag = 10))#ho: estacionariedad
summary(ur.kpss(i_3m, type=c("mu"), use.lag = 10))#ho: estacionariedad
summary(ur.kpss(i_1y, type=c("tau"), use.lag = 10))#ho: estacionariedad
summary(ur.kpss(i_1y, type=c("mu"), use.lag = 10))#ho: estacionariedad
summary(ur.kpss(diff(i_3m), type=c("mu"), use.lag = 10))#ho: estacionariedad
summary(ur.kpss(diff(i_1y), type=c("mu"), use.lag = 10))#ho: estacionariedad

summary(ur.ers(i_3m, type = c("DF-GLS"), model = c("trend"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(i_3m, type = c("DF-GLS"), model = c("constant"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(i_1y, type = c("DF-GLS"), model = c("trend"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(i_1y, type = c("DF-GLS"), model = c("constant"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(diff(i_3m), type = c("DF-GLS"), model = c("trend"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(diff(i_3m), type = c("DF-GLS"), model = c("constant"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(diff(i_1y), type = c("DF-GLS"), model = c("trend"), lag.max = 10)) #ho: no estacionariedad
summary(ur.ers(diff(i_1y), type = c("DF-GLS"), model = c("constant"), lag.max = 10)) #ho: no estacionariedad

i.Y<-cbind(i_1y,i_3m)
VARselect(i.Y, lag.max = 10, type = "const")$selection
VARselect(i.Y, lag.max = 10, type = "const")$criteria

## TEST DE JOHANSEN
eigen1 = ca.jo(i.Y, ecdet = "none", type = "eigen", K = 2, spec = "longrun",season = NULL)
summary(eigen1)
##Al 5% hay una relación de cointegración
## CRITERIO DE LA TRAZA
trace1= ca.jo(i.Y, ecdet = "none", type = "trace", K = 2, spec = "longrun",season = NULL)
summary(trace1) 
#Al 5% de confianza las series están cointegradas.
###TEST CON TÉMINOS DETERMINISTICOS
eigen2 = ca.jo(i.Y, ecdet = "trend", type = "eigen", K = 2, spec = "longrun",season = NULL)
summary(eigen2) #Al 5% de confianza las series est?n cointegradas.

trace2 = ca.jo(i.Y, ecdet = "trend", type = "trace", K = 2, spec = "longrun",season = NULL)
summary(trace2) #Al 5% de confianza las series est?n cointegradas.
###Estimación de modelos VEC Sin términos deterministicos 
VECM.3 = cajorls(trace1, r=1) 
VECM.3 <- VECM(i.Y, lag = 1, r = 1, include = c("const"), estim = "ML")
coefB(VECM.3)
coefA(VECM.3)
####Estimación VEC con términos deterministicos 
Vec.tre<-cajorls(eigen2, r=1)
Vec.tre
coefB(Vec.tre)
coefA(Vec.tre)
##Test para saber si incluir la tendencia 
lttest(eigen2, r=1)
# Cómo no se rechaza Ho, no incluimos la tendencia 
###Modelo VAR
VARi<-vec2var(trace1, r=1)
VARi
VAR2 <- VAR(i.Y, p = 2, type="none", season=NULL)
summary(VAR2)
#########Validación de supuestos 
#Correlación serial
P.62=serial.test(VARi, lags.pt = 60, type = "PT.asymptotic");P.62 #No rechazo, se cumple el supuesto
P.50=serial.test(VAR2, lags.pt = 50, type = "PT.asymptotic");P.50 #No rechazo, se cumple el supuesto
P.30=serial.test(VARi, lags.pt = 20, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto

plot(P.30, names = "i_1y") #Bien comportados, salvo por los residuales al cuadrado
plot(P.62, names = "i_3m")
plot(VARi, names = 'i_1y')
plot(VARi, names = 'i_3m')
#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VARi, lags.multi = 60) 
arch.test(VARi, lags.multi = 50) #Rechazo, no se cumple el supuesto
arch.test(VARi, lags.multi = 20) #Rechazo, no se cumple el supuesto

##Test Jarque-Bera multivariado
plot(normality.test(VARi, multivariate.only = T), names = 'i_1y') #Rechazo, no se cumple el supuesto. 
plot(normality.test(VARi), names = 'i_3m')
#Estabilidad del VAR
summary(VAR2)
roots(VAR2)
stability(VAR2); plot(stability(VAR2))

#Impulso respuesta
grid.arrange(
        irf_ggplot(VARi, 'i_3m', 'i_3m'),
        irf_ggplot(VARi, 'i_3m', 'i_1y'), ncol=2
)

autoplot(predict(VARi, n.ahead = 5, ci = 0.95, method='bootstraping'), facets=T)
#Descomposición de la varianza
decomp.3<-fevd(VARi, n.ahead = 20)
var1y<-c(decomp.3$i_1y[1,],decomp.3$i_1y[2,],decomp.3$i_1y[3,],decomp.3$i_1y[4,],decomp.3$i_1y[5,],decomp.3$i_1y[6,]
  ,decomp.3$i_1y[7,],decomp.3$i_1y[8,],decomp.3$i_1y[9,] ,decomp.3$i_1y[10,], decomp.3$i_1y[11,],decomp.3$i_1y[12,],decomp.3$i_1y[13,],decomp.3$i_1y[14,],decomp.3$i_1y[15,],decomp.3$i_1y[16,]
  ,decomp.3$i_1y[17,],decomp.3$i_1y[18,],decomp.3$i_1y[19,] ,decomp.3$i_1y[20,])
decomp.31<-data.frame(time=rep(1:20,each=2), Series=as.factor(rep(c('i_1y','i_3m'),each=1)), variable=var1y)

var3m<-c(decomp.3$i_3m[1,],decomp.3$i_3m[2,],decomp.3$i_3m[3,],decomp.3$i_3m[4,],decomp.3$i_3m[5,],decomp.3$i_3m[6,]
         ,decomp.3$i_3m[7,],decomp.3$i_3m[8,],decomp.3$i_3m[9,] ,decomp.3$i_3m[10,], decomp.3$i_3m[11,],decomp.3$i_3m[12,],decomp.3$i_3m[13,],decomp.3$i_3m[14,],decomp.3$i_3m[15,],decomp.3$i_3m[16,]
         ,decomp.3$i_3m[17,],decomp.3$i_3m[18,],decomp.3$i_3m[19,] ,decomp.3$i_3m[20,])
decomp.32<-data.frame(time=rep(1:20,each=2), Series=as.factor(rep(c('i_1y','i_3m'),each=1)), variable=var3m)

grid.arrange(
ggplot(data=decomp.31, aes(x=time, y=variable, fill=Series)) + 
        geom_bar(position="stack", stat="identity") + ggtitle('Descomposición de varianza para i_1y') +
        theme_minimal() + xlab('') + ylab('') + scale_fill_manual(values = c("#BF2828F1", "#193AA6EE")),
ggplot(data=decomp.32, aes(x=time, y=variable, fill=Series)) + 
        geom_bar(position="stack", stat="identity") + ggtitle('Descomposición de varianza para i_3m') +
        theme_minimal() + xlab('') + ylab('') + scale_fill_manual(values = c("#BF2828F1", "#193AA6EE"))
)
