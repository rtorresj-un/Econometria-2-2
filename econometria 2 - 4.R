#Taller 4. Grupo 28- Juanita Cortes, David Orozco y Raúl Torres  
#######Paquetes
install.packages("vars")
install.packages("svars")
install.packages('tsDyn')
library(vars);library(urca);library(ggplot2); library(tsDyn)
library(ggfortify);library(gridExtra);library(dplyr)
library(tidyverse);library(svars);library(AER); library(ggthemes)
library(dynlm);library(readr);library(tsDyn);library(VAR.etp)

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
        IRF = irf(VAR, impulse=impulso ,response=respuesta,n.ahead = 10,ci = 0.95, boot=T, ortho=T) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);  
        data_irf= data.frame(IRF$irf,IRF$Lower,IRF$Upper, c(0:10))
        ggplot(data_irf, aes(x=data_irf[,4], y=data_irf[,1])) +
                geom_line() + 
                geom_ribbon(aes(ymin=data_irf[,2], ymax=data_irf[,3], fill="Bandas al 95% \n de confianza"), alpha=.3) +
                theme_minimal() + scale_color_distiller() + scale_fill_ordinal(name='') +
                ylab("Porcentaje de cambio") +
                xlab("Pasos adelante") + ggtitle(str_c('Respuesta de ',as.character(colnames(data_irf)[1]), ' ante cambios en ', impulso))
}
############################################################################

####Punto 1####
###############################
VAR INFLACION DESEMPLEO ALEMANIA 1991- 2020 
353 OBSERVACIONES 1991-01 ; 2020-05
###############################

#Harmonised Index of Consumer Prices (HICP) #####
ipc_ajustado <- read_csv("C:/Users/USUARIO/Downloads/ipc_ajustado.csv", 
                         +     col_names = FALSE)
attach(ipc_ajustado)
Date1<-as.Date(Date, format = '%d/%m/%Y')
IPC_DE<-ts(data.frame(ipc_ajustado)$X2[(1:353)], frequency = 12, start = 1991)
View(IPC_DE)

#Descripción de IPC
summary(IPC_DE); kurtosis(IPC_DE)
start(IPC_DE); end(IPC_DE)
#Clara tendencia de la serie
autoplot(IPC_DE,main = "IPC Alemania (enero 1991 - abril 2020)")
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
grid.arrange(
        autoplot(Diff_ipc, col = "midnightblue")
)  
monthplot(Diff, col = "midnightblue")

# Gráfico de autocorrelación simple y parcial diff 
grid.arrange(
        ggAcf(Diff_ipc,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del difIPC', ylim=c(-1,1)),
        ggPacf(Diff_ipc,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del difIPC', ylim=c(-1,1))
)

#Pruebas de raiz unitaria
summary(ur.df(IPC_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'trend'),level = "5pct")
summary(ur.df(IPC_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(IPC_DE,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
summary(ur.df(IPC_DE,type = 'none'));interp_urdf(ur.df(IPC_DE,type = 'none'),level = "5pct")

summary(ur.df(Diff_ipc,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff_ipc,type = 'trend'),level = "5pct")
summary(ur.df(Diff_ipc,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff_ipc,type = 'drift'),level = "5pct") #caminata aleatoria con deriva
summary(ur.df(Diff,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'none'),level = "5pct")

# Serie tiene tendencia lineal, se realiza la prueba df con tendencia y no hay presencia de raiz unitaria 






# Harmonized Unemployment Rate: Total: All Persons for Germany, Percent, Seasonally Adjusted ####
Data_2<-read_excel(file.choose())
attach(Data_2)
D_DE<-ts(data.frame(Data_2)$desempleo, frequency = 12, start = 1991)


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
grid.arrange(
        autoplot(Diff_DE, col = "midnightblue")
)  
monthplot(Diff_DE, col = "midnightblue")

# Gráfico de autocorrelación simple y parcial diff log
#claro componente estacional cada 12 periodo
grid.arrange(
        ggAcf(Diff_DE,lag.max=64,plot=T,lwd=2,xlab='',main='ACF dif Desempleo', ylim=c(-1,1)),
        ggPacf(Diff_DE,lag.max=64,plot=T,lwd=2,xlab='',main='PACF dif Desempleo', ylim=c(-1,1)),
        ggAcf(Diff_log_DE,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del Desempleo diferenciado log', ylim=c(-1,1)), #el logaritmo no aporta mejoría a la autocorrelación de la serie
        ggPacf(Diff_log_DE,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del Desempleo diferenciado log', ylim=c(-1,1)),
        ggAcf(Diff_s_DE,lag.max=64,plot=T,lwd=2,xlab='',main='ACF del Desempleo diferenciado estacional', ylim=c(-1,1)), #diferenciar la serie mejora la autocorrelación de la serie pero puede no ser necesario 
        ggPacf(Diff_s_DE,lag.max=64,plot=T,lwd=2,xlab='',main='PACF del Desempleo diferenciado estacional', ylim=c(-1,1)) #porque el efecto persiste, puede corregirse con un SARIMA(1,0,1)(1,0,1)[12]
)

#Pruebas de raiz unitaria
summary(ur.df(D_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(D_DE,type = 'trend'),level = "5pct")
summary(ur.df(D_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(D_DE,type = 'drift'),level = "5pct") 
summary(ur.df(D_DE,type = 'none'));interp_urdf(ur.df(D_DE,type = 'none'),level = "5pct") #caminata aleatoria 

summary(ur.df(Diff_DE,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(Diff_DE,type = 'trend'),level = "5pct")
summary(ur.df(Diff_DE,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'drift'),level = "5pct")
summary(ur.df(Diff_DE,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(Diff,type = 'none'),level = "5pct")

plot(Diff_DE); mean(Diff)


GER<-cbind(Diff_DE, Diff_ipc)
VARselect(GER, lag.max=40,type = "none", season = NULL) # estimar var 2 o 7 
VARselect(GER, lag.max=20,type = "const", season = NULL)# estimar var 3 o 2

#VAR con sólo intercepto.
V.dr.1= VAR(GER, p=2, type="const", season=NULL) 
summary(V.dr.1) #El intercepto es significativo en una ecuación.

#VAR sin términos determinísticos.
V.no.1 = VAR(GER, p=2, type="none", season=NULL)  
summary(V.no.1)
roots(V.dr.1)
roots(V.no.1)
Acoef(V.dr.1)





####Punto 2####
Datos<-read.csv(file.choose())
Y_1<-ts(Datos$y1)
Y_2<-ts(Datos$y2)
##Descripción de las series
summary(Datos)
autoplot(Y_1)
autoplot(Y_2)
var(Y_1)
var(Y_2)
## vemos que la Y_2 varía mucho más que Y_1
##Las dos series en la misma gráfica 
x11()
plot(merge(as.zoo(Y_1), as.zoo(Y_2)), 
     col=c("red","darkblue"),
     plot.type= "single",
     lty = c(2,1),
     lwd = 1,
     xlab="",
     ylab="serie",
     ylim=c(-5,6),
     main="Series simuladas")
##Pruebas de raíz unitaria
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "trend"))
## La tendencia y deriva son significativas, aunque graficamente parece no tener tendencia 
##por tanto hacemos ambas pruebas 
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "drift"))
##confirmamos que la serie es I(0)
summary(ur.df(Y_2, lags=8, selectlags = "AIC", type = "trend"))
## La tendencia y deriva son significativas, aunque graficamente parece no tener tendencia 
##por tanto hacemos ambas pruebas 
summary(ur.df(Y_1, lags=8, selectlags = "AIC", type = "drift"))
##confirmamos que la serie es I(0)
##Creamos modelo VAR
Y<-cbind(Y_1,Y_2)
VARselect(Y, lag.max = 8, type = "both", season = NULL)## 3 rezagos
VARselect(Y, lag.max = 8, type = "trend")##3 rezagos
VARselect(Y, lag.max = 8, type= "none")## 3 rezagos
##Utilizaremos 3 rezagos para el VAR
VAR3<-VAR(Y, p=3, type = "both")
summary(VAR3)
##La tendencia y la constante no son significativas
VAR3_1<-VAR(Y, p=3, type = "none")
summary(VAR3_1)
##validación de supuestos 
##Estabilidad del proceso
roots(VAR3_1)
##El proceso es estable
##Autocorrelación 
P.75=serial.test(VAR3_1, lags.pt = 75, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR3_1, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR3_1, lags.pt = 20, type = "PT.asymptotic");P.20  #No rechazo, se cumple el supuesto
P.10=serial.test(VAR3_1, lags.pt = 10, type = "PT.asymptotic");P.10 #No rechazo, se cumple el supuesto
#Vemos las gráficas ACF y PACF
x11()
plot(P.30, names = "Series.1") 
plot(P.30, names = "Series.2") 
##Validamos el supuesto de homoscedasticidad 
arch.test(VAR3_1, lags.multi = 24, multivariate.only = TRUE) #No rechazo, se cumple el supuesto.
arch.test(VAR3_1, lags.multi = 12, multivariate.only = TRUE) #No rechazo, se cumple el supuesto
##Válidamos el supuesto de normalidad
normality.test(VAR3_1)
##Pronóstico 
fore<-predict(VAR3_1, n.ahead=5, ci=.95)
x11()
plot(fore)
help("predict")
##Modelo SVAR
##unimos las dos series en una matriz
##Creamos la matriz de efectos contemporaneos 
matrix_1=as.matrix(cbind(c(1,NA),c(NA,1)))
matrix_1
SVAR3_1<-SVAR(VAR3_1, Amat = matrix_1, Bmat = NULL, hessian = TRUE, method="scoring" )
SVAR3_1
help("SVAR")
##Ya queda definido el modelo con efectos contemporaneos 
##Impulso respuesta 
lags=c(0:5)


#IRF de las variables del sistema ante distintos choques exÃ³genos.
IRF1 = irf(SVAR3_1, impulse="Y_1",response="Y_1",n.ahead = 50,ci = 0.95, ortho=F)  
IRF1.1= data.frame(IRF1$irf,IRF1$Lower,IRF1$Upper, lags)
IRF2 = irf(SVAR3_1, impulse="Y_1",response="Y_2",n.ahead = 50,ci=0.95, ortho=F)  
IRF1.2= data.frame(IRF2$irf,IRF2$Lower,IRF2$Upper, lags)
IRF3 = irf(SVAR3_1, impulse="Y_2",response="Y_1",n.ahead = 50,ci=0.95, ortho=F)   
IRF2.1= data.frame(IRF3$irf,IRF3$Lower,IRF3$Upper, lags)
IRF4 = irf(SVAR3_1, impulse="Y_2",response="Y_2",n.ahead = 50,ci=0.95, ortho=F)  ;
IRF2.2= data.frame(IRF4$irf,IRF4$Lower,IRF4$Upper, lags)


y1.y1 <- IRF1.1%>% 
        ggplot(aes(x=IRF1.1[,4], y=IRF1.1[,1], ymin=IRF1.1[,2], ymax=IRF1.1[,3] )) +
        geom_hline(yintercept = 0, color="red") +
        geom_ribbon(fill="grey", alpha=0.2) +
        geom_line() +
        theme_light() +
        ggtitle("Impulso de y1 - respuesta de y1")+
        ylab("")+
        xlab("pasos adelante") +
        theme(plot.title = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size=11))

y1.y2 <- IRF1.2%>% 
        ggplot(aes(x=IRF1.2[,4], y=IRF1.2[,1], ymin=IRF1.2[,2], ymax=IRF1.2[,3] )) +
        geom_hline(yintercept = 0, color="red") +
        geom_ribbon(fill="grey", alpha=0.2) +
        geom_line() +
        theme_light() +
        ggtitle("Impulso de y1 - respuesta de y2")+
        ylab("")+
        xlab("pasos adelante") +
        theme(plot.title = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size=11))

y2.y1 <- IRF2.1%>% 
        ggplot(aes(x=IRF2.1[,4], y=IRF2.1[,1], ymin=IRF2.1[,2], ymax=IRF2.1[,3] )) +
        geom_hline(yintercept = 0, color="red") +
        geom_ribbon(fill="grey", alpha=0.2) +
        geom_line() +
        theme_light() +
        ggtitle("Impulso de y2 - respuesta de y1")+
        ylab("")+
        xlab("pasos adelante") +
        theme(plot.title = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size=11))

y2.y2 <- IRF2.2%>% 
        ggplot(aes(x=IRF2.2[,4], y=IRF2.2[,1], ymin=IRF2.2[,2], ymax=IRF2.2[,3] )) +
        geom_hline(yintercept = 0, color="red") +
        geom_ribbon(fill="grey", alpha=0.2) +
        geom_line() +
        theme_light() +
        ggtitle("Impulso de y2 - respuesta de y2")+
        ylab("")+
        xlab("pasos adelante") +
        theme(plot.title = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size=11))

x11()
grid.arrange(y1.y1,y1.y2,y2.y1,y2.y2,ncol=2)

## Descomposición de la varianza
x11()
fevd(SVAR3_1, n.ahead = 24)
plot(fevd(SVAR3_1, n.ahead = 24),col=c("red", "green"))
##Obteniendo los parametros etimados del SVAR
##Creamos las matrices con los coeficientes de los rezagos del VAR
#T_0: No hay interceptos
#T_1:
T_1<-as.matrix(cbind(c(-0.18291,-0.44293),c(-0.09498, -0.36287)))
T_1
#T_2
T_2<-as.matrix(cbind(c(-0.20118,0.07185),c(-0.34974,0.050365)))
T_2
#T_3
T_3<-as.matrix(cbind(c(-0.04237,0.02078),c(-0.05186,-0.16564)))
T_3
##Inversa de A 
InvA<-solve(SVAR3_1$A)
##Obtenemos las matrices del VAR en forma reducida
#a_1
a_1<-InvA%*%T_1
#a_2
a_2<-InvA%*%T_2
#a_3
a_3<-InvA%*%T_3
####EL VAR EN FORMA REDUCIDA QUEDA:
Er<-InvA%*%SVAR3_1$Sigma.U%*%InvA 

Y~a_1*L(Y, 1)+a_2*L(Y,2)+a_3*L(Y,3)+Er





####Punto 3####
UK<-data.frame(UK_4 <- read.csv(file.choose(),sep=";"))
i_3m<-ts(UK$i_3m, start=2000, frequency = 12)
i_1y<-ts(UK$i_1y, start=2000, frequency = 12)
i_5y<-ts(UK$i_5y, start=2000, frequency = 12)
Date_4<-as.Date(UK$Date4, format = '%d/%m/%y')
autoplot(cbind(i_3m, i_1y, i_5y), facets = F, main="Tasas de intrés UK", xlab="", ylab="", size=1)

## Pruebas de raíz unitaria 
summary(ur.df(i_3m,type = 'trend', selectlags = 'AIC')); interp_urdf(ur.df(i_3m,type = 'trend', selectlags = 'AIC'),level = "1pct")
summary(ur.df(i_3m,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(i_3m,type = 'drift', selectlags = 'AIC'),level = "1pct")
summary(ur.df(i_3m,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(i_3m,type = 'none', selectlags = 'AIC'),level = "1pct")
summary(ur.df(diff(i_3m),type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(diff(i_3m),type = 'none', selectlags = 'AIC'),level = "1pct")

summary(ur.df(i_1y,type = 'trend', selectlags = 'AIC'));interp_urdf(ur.df(i_1y,type = 'trend', selectlags = 'AIC'),level = "1pct")
summary(ur.df(i_1y,type = 'drift', selectlags = 'AIC'));interp_urdf(ur.df(i_1y,type = 'drift', selectlags = 'AIC'),level = "1pct")
summary(ur.df(i_1y,type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(i_1y,type = 'none', selectlags = 'AIC'),level = "1pct")
summary(ur.df(diff(i_1y),type = 'none', selectlags = 'AIC'));interp_urdf(ur.df(diff(i_1y),type = 'none', selectlags = 'AIC'),level = "1pct")

summary(ur.pp(i_3m,model=c("constant"), type=c("Z-tau"), use.lag = 2))#ho: no estacionariedad
summary(ur.pp(i_1y,model=c("constant"), type=c("Z-tau"), use.lag = 2))#ho: no estacionariedad
summary(ur.pp(diff(i_3m),model=c("constant"), type=c("Z-tau"), use.lag = 2))#ho: no estacionariedad
summary(ur.pp(diff(i_1y),model=c("constant"), type=c("Z-tau"), use.lag = 2))#ho: no estacionariedad

summary(ur.kpss(i_3m, type=c("mu"), use.lag = 2))#ho: estacionariedad
summary(ur.kpss(i_1y, type=c("mu"), use.lag = 2))#ho: estacionariedad
summary(ur.kpss(diff(i_3m), type=c("mu"), use.lag = 2))#ho: estacionariedad
summary(ur.kpss(diff(i_1y), type=c("mu"), use.lag = 2))#ho: estacionariedad

summary(ur.ers(i_3m, type = c("DF-GLS"), model = c("constant"), lag.max = 2))
summary(ur.ers(i_1y, type = c("DF-GLS"), model = c("constant"), lag.max = 2))
summary(ur.ers(diff(i_3m), type = c("DF-GLS"), model = c("constant"), lag.max = 2))
summary(ur.ers(diff(i_1y), type = c("DF-GLS"), model = c("constant"), lag.max = 2))

i.Y<-cbind(i_3m,i_1y)
residplot1<-data.frame(time=Date_4, variable = c(i_1y-i_3m))
ggplot(residplot1,aes(time,variable)) + geom_line(aes(color="Spread 1Y-3M")) +
        geom_line(data = UK, aes(Date_4, i_3m, color="3M")) +
        geom_line(data= UK, aes(Date_4, i_1y, color="1Y"))+
        geom_ribbon(aes(ymin = i_3m, ymax = i_1y, fill="Spread 1Y-3M"), alpha = .3) + 
        theme_minimal() + scale_color_stata() + xlab('') + ylab('')+
        labs(color='Series') + scale_fill_stata(name='') +
        theme(legend.position="bottom")

d_0810<-rep(0,247)
d_0810[106:247]<-1
d_exo<-cbind(d_0810, NULL)

VARselect(i.Y, lag.max = 30, type = "both")$selection
VARselect(i.Y, lag.max = 30, type = "both")$criteria

## TEST DE JOHANSEN
eigen1 = ca.jo(i.Y, ecdet = "none", type = "eigen", K = 7, spec = "longrun",season = NULL)
summary(eigen1)
##Al 5% hay una relación de cointegración
## CRITERIO DE LA TRAZA
trace1= ca.jo(i.Y, ecdet = "none", type = "trace", K = 7, spec = "longrun",season = NULL, dumvar = d_exo)
summary(trace1) 
#Al 5% de confianza las series están cointegradas.
####TEST CON TÉMINOS DETERMINISTICOS
#eigen2 = ca.jo(i.Y, ecdet = "const", type = "eigen", K = 7, spec = "longrun",season = NULL)
#summary(eigen2) #Al 5% de confianza las series est?n cointegradas.
#
#trace2 = ca.jo(i.Y, ecdet = "const", type = "trace", K = 7, spec = "longrun",season = NULL)
#summary(trace2) #Al 5% de confianza las series est?n cointegradas.
####Estimación de modelos VEC Sin términos deterministicos 
Vec = cajorls(trace1, r=1) 
Vec
coefB(Vec)
coefA(Vec)
####Estimación VEC con términos deterministicos 
Vec.det<-cajorls(eigen2, r=1)
Vec.det
coefB(Vec.det)
coefA(Vec.det)
##Test para saber si incluir la tendencia 
lttest(eigen2, r=1)
# Cómo no se rechaza Ho, no incluimos la tendencia 
###Modelo VAR
VARi<-vec2var(trace1, r=1)
VARi
#########Validación de supuestos 

P.62=serial.test(VARi, lags.pt = 62, type = "PT.asymptotic");P.62 #No rechazo, se cumple el supuesto
P.50=serial.test(VARi, lags.pt = 50, type = "PT.asymptotic");P.50 #No rechazo, se cumple el supuesto
P.40=serial.test(VARi, lags.pt = 40, type = "PT.asymptotic");P.40  #No rechazo, se cumple el supuesto
P.30=serial.test(VARi, lags.pt = 30, type = "PT.asymptotic");P.30 #rechazo, no se cumple el supuesto

plot(P.40, names = "i_3m") #Bien comportados, salvo por los residuales al cuadrado
plot(P.40, names = "i_1y")

#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VARi, lags.multi = 60) #Rechazo, no se cumple el supuesto.
arch.test(VARi, lags.multi = 40) #Rechazo, no se cumple el supuesto
ArchTest(resid(VARi), lags = 40)
##Test Jarque-Bera multivariado
normality.test(VARi) #Rechazo, no se cumple el supuesto. 
eigen(VARi$A$A1)
VAR2 <- VAR(i.Y, p= 7, type="none", season=NULL, exogen = d_exo)
plot(stability(VAR2))
summary(VAR2)
#Impulso respuesta
grid.arrange(
        irf_ggplot(VARi, 'i_3m', 'i_3m'),
        irf_ggplot(VARi, 'i_3m', 'i_1y'), ncol=2
)


### 
UK<-data.frame(UK_4 <- read_delim(file.choose(),";", escape_double = FALSE, trim_ws = TRUE))
i_3m<-ts(UK$i_3m, start=2000, frequency = 12)
i_1y<-ts(UK$i_1y, start=2000, frequency = 12)
i_5y<-ts(UK$i_5y, start=2000, frequency = 12)
Date_4<-as.Date(UK$Date4, format = '%d/%m/%y')
ggplot(UK, aes(Date_4, i_3m)) + geom_line(color='midnightblue') + xlab('')
ggplot(UK, aes(Date_4, i_1y)) + geom_line(color='midnightblue') + xlab('')
ggplot(UK, aes(Date_4, i_5y)) + geom_line(color='midnightblue') + xlab('')

autoplot(cbind(i_3m, i_1y), facets = F, main="Precios spot Bonos UK", xlab="", ylab="", size=0.5)

# pruebas de raiz unitaria
summary(ur.df(i_3m,type = 'trend', lags=6, selectlags = 'AIC')) # Raiz unitaria, tendencia no sig
summary(ur.df(i_3m,type = 'drift', lags=6,selectlags = 'AIC')) # Raiz unitaria, Deriva no sig 
summary(ur.df(i_3m,type = 'none', lags=6,selectlags = 'AIC')) # No raiz unitaria 

summary(ur.df(i_1y,type = 'trend', lags=6,selectlags = 'AIC')) # Raiz unitaria, tendencia no sig
summary(ur.df(i_1y,type = 'drift', lags=6,selectlags = 'AIC'))# Raiz unitaria, Deriva no sig  
summary(ur.df(i_1y,type = 'none', lags=6,selectlags = 'AIC')) # No raiz unitaria  

summary(ur.pp(i_3m,model=c("constant"), type=c("Z-tau"))) # h0: raiz unitaria 
summary(ur.pp(i_1y,model=c("constant"), type=c("Z-tau")))
summary(ur.pp(diff(i_3m),model=c("constant"), type=c("Z-tau")))
summary(ur.pp(diff(i_1y),model=c("constant"), type=c("Z-tau")))

summary(ur.kpss(i_3m, type=c("mu")))#ho: estacionariedad
summary(ur.kpss(i_1y, type=c("mu")))#ho: estacionariedad
summary(ur.kpss(diff(i_3m), type=c("mu")))#ho: estacionariedad
summary(ur.kpss(diff(i_1y), type=c("mu")))#ho: estacionariedad

Y<- cbind(i_3m,i_1y)
VARselect(Y, lag.max = 6, type="both", season=NULL) #2 y 4 rezagos, elegimos el m?s parsimonioso
VARselect(Y, lag.max = 6, type="const", season=NULL) #4 rezagos y 3 rezagos, elegimos el m?s parsimonioso
VARselect(Y, lag.max = 6, type="none", season=NULL) #3 rezagos y 4 rezagos, elegimos el m?s parsimonioso

# En la mayoria se incluyen 3 y 4 rezagos, por parsimonia 3 
summary(VAR(Y, p=3, type="both", season=NULL)) #tendencia y constante en una pero no en la otra
summary(VAR(Y, p=3, type="const", season=NULL)) #La constante no es significativa
summary(VAR(Y, p=3, type="none", season=NULL)) # tratar in terminos deterministicos 

VAR2 <- VAR(Y, p=2, type="none", season=NULL)

#Vamos a analizar el comportamiento de los residuales, si no se comportan bien incluiremos m?s rezagos
P.70=serial.test(VAR2, lags.pt = 70, type = "PT.asymptotic");P.70 #No rechazo, se cumple el supuesto
P.60=serial.test(VAR2, lags.pt = 60, type = "PT.asymptotic");P.60 #No rechazo, se cumple el supuesto
P.50=serial.test(VAR2, lags.pt = 50, type = "PT.asymptotic");P.50  #No rechazo, se cumple el supuesto
P.40=serial.test(VAR2, lags.pt = 40, type = "PT.asymptotic");P.40 #No rechazo, se cumple el supuesto
plot(P.40, names = "i_3m")
plot(P.40, names = "i_1y")

jarque.bera.test(VAR2$varresult$i_3m$residuals)
jarque.bera.test(VAR2$varresult$i_1y$residuals)
VAR2$varresult$i_1y$residuals

# NO SE DISTRIBUYE NORMAL IUDAAAAAAAAAAAAA
# sin terminos deterministicos#####

eigen1 = ca.jo(Y, ecdet = "none", type = "eigen", K = 3, spec = "longrun",season = NULL)
summary(eigen1) #Al 5% de confianza las series est?n cointegradas.

#Criterio de la traza. Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r>=1, luego H0: r>=1 vs H1: r>=2, y as? sucesivamente. Aqu? k=2

trace1= ca.jo(Y, ecdet = "none", type = "trace", K = , spec = "longrun",season = NULL)
summary(trace1) #Al 5% de confianza las series estan cointegradas.

# las series estan cointegradas en ambas pruebas
# Estimacion vec
VEC1 = cajorls(eigen1, r=1) 
VEC1

#Con esta funci?n obtenemos el vector de cointegraci?n normalizado
coefB(VEC1)

#Con esta funci?n obtenemos los coeficientes de velocidad de ajuste
coefA(VEC1)

# con terminos deterministicos####
#Criterio del valor propio (es la prueba m?s robusta). Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r=1, luego H0: r=1 vs H1: r=2, y as? sucesivamente. Aqu? k=4

eigen2 = ca.jo(Y, ecdet = "const", type = "eigen", K = 3, spec = "longrun",season = NULL)
summary(eigen2) #Al 5% de confianza las series est?n cointegradas.

#Criterio de la traza. Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r>=1, luego H0: r>=1 vs H1: r>=2, y as? sucesivamente. Aqu? k=4

trace2 = ca.jo(Y, ecdet = "const", type = "trace", K = 3, spec = "longrun",season = NULL)
summary(trace2) #Al 5% de confianza las series est?n cointegradas.
#Aqu? estimamos el modelo VEC
VEC2 = cajorls(eigen2, r=1) 
VEC2

#Con esta funci?n obtenemos el vector de cointegraci?n normalizado
coefB(VEC2)

#Con esta funci?n obtenemos los coeficientes de velocidad de ajuste
coefA(VEC2)

#No rechazo la hip?tesis nula, por lo que no se debe incluir constante en el vector de cointegraci?n.
lttest(eigen2, r=1)  

# Convertir el vec a var ####
VAR.oil = vec2var(eigen1, r = 1)
VAR.oil

length(i_3m)/4 #62
# Validar supuestos ####
P.62=serial.test(VAR.oil, lags.pt = 62, type = "PT.asymptotic");P.62 #No rechazo, se cumple el supuesto
P.50=serial.test(VAR.oil, lags.pt = 50, type = "PT.asymptotic");P.50 #No rechazo, se cumple el supuesto
P.40=serial.test(VAR.oil, lags.pt = 40, type = "PT.asymptotic");P.40  #No rechazo, se cumple el supuesto
P.30=serial.test(VAR.oil, lags.pt = 30, type = "PT.asymptotic");P.30 #rechazo, no se cumple el supuesto


plot(P.40, names = "i_3m") #Bien comportados, salvo por los residuales al cuadrado
plot(P.40, names = "i_1y")

#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VAR.oil, lags.multi = 24, multivariate.only = TRUE) #Rechazo, no se cumple el supuesto.
arch.test(VAR.oil, lags.multi = 12, multivariate.only = TRUE) #Rechazo, no se cumple el supuesto
arch.test()
##Test Jarque-Bera multivariado
normality.test(VAR.oil) #Rechazo, no se cumple el supuesto. 
