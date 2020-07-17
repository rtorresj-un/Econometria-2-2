#Taller 4. Grupo 28- Juanita Cortes, David Orozco y Raúl Torres  
#######Paquetes
install.packages("vars")
install.packages("svars")
library(vars)
library(urca)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(dplyr)
library(tidyr)
library(svars)
library(AER)
library(dynlm)
#########################################################################
#########Punto 1




#############################################################################
#########Punto 2
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
IRF1 = irf(SVAR3_1, impulse="Y_1",response="Y_1",n.ahead = 5,ci = 0.95, ortho=F)  
IRF1.1= data.frame(IRF1$irf,IRF1$Lower,IRF1$Upper, lags)
IRF2 = irf(SVAR3_1, impulse="Y_1",response="Y_2",n.ahead = 5,ci=0.95, ortho=F)  
IRF1.2= data.frame(IRF2$irf,IRF2$Lower,IRF2$Upper, lags)
IRF3 = irf(SVAR3_1, impulse="Y_2",response="Y_1",n.ahead = 5,ci=0.95, ortho=F)   
IRF2.1= data.frame(IRF3$irf,IRF3$Lower,IRF3$Upper, lags)
IRF4 = irf(SVAR3_1, impulse="Y_2",response="Y_2",n.ahead = 5,ci=0.95, ortho=F)  ;
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





##########################################################################################################
#########Punto 3