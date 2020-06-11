#--------------------------
Universidad Nacional de Colombia 
Econometría 2
Taller 2
Variable instrumental y binaria
Grupo 25

integrantes:
Juanita Cortes
Raul Torres
David Orozco
#---------------------------

# install.packages("AER") #Applied Econometrics with R for Instrumental Variables
# install.packages("foreing") #Para cargar datos con formato Stata
# install.packages("stargazer") #Para una presentaci?n m?s est?tica de los resultados
# install.packages("estimatr") #Para hacer MC2E con errores robustos
# install.packages("arm") #An?lisis de datos utilizando regresiones
# installl.packages("lmtest")
# install.packages('margins')

#Cargamos los paquetes. 

library(AER);library(foreign); library(stargazer); library(margins)
library(arm);library(lmtest);library(estimatr); library(ggplot2)
library(tidyverse); library(broom);library(stargazer);library(wooldridge)

#Variable Instrumental ####  

Datos_instrum<-read.csv(file.choose())
summary(Datos_instrum)
attach(Datos_instrum)

#Sobre la base 
plot(x1,y)
plot(x2,y)


#1. Modelo por MCO ####
vi_mco<-lm(y ~ x1 + x2, data = Datos_instrum)
summary(vi_mco)
stargazer::stargazer(vi_mco)

#2. Instrumentos, ¿Qué condiciones deben cumplir para ser válidos?####
## Relevancia ####
cor(x1,z1) #0.5908596
cor(x1,z2) #0.8198656
cor(x1,z3) #0.4272298
cor(x1,z4) #-0.01204702
cor(x1,z5) #0.407697
#Gráficos
#z1
gridExtra::grid.arrange(
ggplot(Datos_instrum, aes_(x = z1,y = x1))+ geom_point()+
      geom_smooth(method = "lm")+ xlab("z1")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z1,x1"),
#z2
ggplot(Datos_instrum, aes_(x = z2,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z2")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z2,x1"),
#z3
ggplot(Datos_instrum, aes_(x = z3,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z3")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z3,x1"),
#z4 nube
ggplot(Datos_instrum, aes_(x = z4,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z4")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z4,x1"),
#z5
ggplot(Datos_instrum, aes_(x = z5,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z5")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z5,x1"), ncol=2
)
#De lo anterior concluimos que z4 no es relevante para x1

## significancia conjunta 1 etapa ####

# Estadístico f>10 en 1etapa
Etapa1 <-lm(x1~x2+z1+z2+z3+z5, data=Datos_instrum); summary(Etapa1)
Etapa1.1<- lm(x1~x2+z1+z2+z5,data = Datos_instrum);summary(Etapa1.1)
Etapa1.2<- lm(x1~x2+z1+z2+z3,data = Datos_instrum);summary(Etapa1.2)

stargazer(Etapa1,Etapa1.1,Etapa1.2)

# Prueba de significancia de un conjunto de parámetros para las anteriores etapas
linearHypothesis(Etapa1,c("z1=0","z2=0","z3=0","z5=0"))  #f= 510.9902
linearHypothesis(Etapa1.1,c("z1=0","z2=0","z5=0")) # f= 678.5912
linearHypothesis(Etapa1.2,c("z1=0","z2=0","z3=0")) # f= 	682.0047

#Al hacer la prueba de significancia conjunta, z1,z2,z3,z5 son conjuntamente diferentes de 0 


#3. Regresion por VI asumiendo x2 exogena ####

VI_1=ivreg(y~x1+x2|x2+z1+z2+z3+z5, data = Datos_instrum);summary(VI_1,diagnostics = TRUE)

VI_2=ivreg(y~x1+x2|x2+z1+z2+z3, data = Datos_instrum);summary(VI_2,diagnostics = TRUE)

VI_3=ivreg(y~x1+x2|x2+z1+z2+z5, data = Datos_instrum);summary(VI_3,diagnostics = TRUE)

stargazer(VI_1,VI_2,VI_3)

#4. asuma x2 endógena. Suponga exogeneidad del instrumento y estime ####
cor(x2,z1) #-0.02959023
cor(x2,z2) #-0.04093649
cor(x2,z3) #-0.0085756
cor(x2,z4) #0.7129909
cor(x2,z5) #-0.02333571

stargazer(lm(x2~x1+z1+z2+z3+z4+z5))

#Si x2 es endogena, el único instrumento relevante es z4 
VI_x2=ivreg(y~x1+x2|x1+z4, data = Datos_instrum);summary(VI_x2,diagnostics = TRUE)

#las pruebas indican que el instrumento es relevante
# wu-Hausman, inidca que x2 no es endógena, resulta mejor estimar por mco
# No es necesario el test de sargan pues sólo se usa un instrumento

Etapa1_x2<-lm(x2~x1+z4, data=Datos_instrum); summary(Etapa1_x2)
linearHypothesis(Etapa1_x2, "z4=0")

#5.Compare las estimaciones ####

stargazer(vi_mco, VI_2,VI_4,VI_x2,type = "text")

#6. escoja el mejor modelos. haga la regresión robusta y compare ####
#Homoscedasticidad
bptest(VI_1)
bptest(VI_3)
#Correlación serial
bgtest(VI_1, order = 4)
bgtest(VI_3, order = 4)

#Modelo sobreidentificado
VI_Robust_x1 = iv_robust(y ~ x1+x2| x2+z1+z2+z3+z5 , data = Datos_instrum, diagnostics = TRUE)
summary(VI_Robust_x1)

#Buen Modelo al quitar z3
VI_Robust2_x1 = iv_robust(y ~ x1+x2| x2+z1+z2+z5 , data = Datos_instrum, diagnostics = TRUE)
summary(VI_Robust2_x1)

library(xtable)
xtab <- xtable(tidy(summary(VI_Robust2_x1)), digits=c(0,4,4,4,4,4))
print(xtab, floating=FALSE)

xtab1 <- xtable(tidy(coeftest(VI_Robust_x1)), digits=c(0,4,4,4,4,4))
print(xtab1, floating=FALSE)

detach(Datos_instrum)
#Variable binaria####
Datos_binaria<-readr::read_csv(file.choose())
attach(Datos_binaria)
summary(Datos_binaria)
plot(y)
plot(x1)
plot(x2)
plot(I(x2^2))
boxplot(x2)
boxplot(I(x2^2))
#Modelo de prob lineal
mpl_1<-lm(y ~ 1 + x1 + x2 + I(x2^2))
summary(mpl_1)
#Modelo Logit
logit_1<-glm(y ~ 1 + x1 + x2 + I(x2^2), family=binomial (link = "logit"))
summary(logit_1)
#Modelo Probit
probit_1<-glm(y ~ 1 + x1 + x2 + I(x2^2), family=binomial (link = "probit"))
summary(probit_1)

stargazer(mpl_1,logit_1,probit_1, type = 'text')

plot(x = x1+x2+I(x2^2),y = y,pch=20,main = "Modelo de Regresión Lineal",
  xlab = substitute(y),ylab = substitute(x1+x2+I(x2^2)))
  abline(lm(y~x1+x2+I(x2^2)))
  
  ggplot(data=NULL,aes(x=x1+x2+I(x2^2), y=y)) + geom_point() + 
    stat_smooth(method=glm, method.args=list(family=binomial (link = "logit")), se=T)+
    ylab(substitute(dependiente))+xlab(substitute(independiente))+
    ggtitle(label = "Modelo de Regresión Logístico",
            subtitle = paste(substitute(dependiente),"vs",substitute(independiente)))
#Efectos marginales####
  #Efecto marginal en la media
  margins(mpl_1, at=list(x1=mean(x1), x2=mean(x2)))
  margins(logit_1, at=list(x1=mean(x1), x2=mean(x2)))
  margins(probit_1, at=list(x1=mean(x1), x2=mean(x2)))
  
  #Efecto marginal promedio
  APE_mpl<- coef(mpl_1)
  APE_logit<- mean(dlogis(predict(logit_1)))*coef(logit_1)
  APE_probit<- mean(dnorm(predict(probit_1)))*coef(probit_1)
  cbind(APE_mpl, APE_logit, APE_probit)
    
#Efectos marginales 1, 2, 3 cuartil####
  
  #Cálculo usando cuantiles efecto marginal en la media (marginal effects at representative cases)
#1er cuartil
margins(mpl_1, at=list(x1=quantile(x1, probs = 0.25), x2=quantile(x2, probs = 0.25)))
margins(logit_1, at=list(x1=quantile(x1, probs = 0.25), x2=quantile(x2, probs = 0.25)))
margins(probit_1, at=list(x1=quantile(x1, probs = 0.25), x2=quantile(x2, probs = 0.25)))
#2do cuartil
margins(mpl_1, at=list(x1=quantile(x1, probs = 0.5), x2=quantile(x2, probs = 0.5)))
margins(logit_1, at=list(x1=quantile(x1, probs = 0.5), x2=quantile(x2, probs = 0.5)))
margins(probit_1, at=list(x1=quantile(x1, probs = 0.5), x2=quantile(x2, probs = 0.5)))
#3er cuartil
margins(mpl_1, at=list(x1=quantile(x1, probs = 0.75), x2=quantile(x2, probs = 0.75)))
margins(logit_1, at=list(x1=quantile(x1, probs = 0.75), x2=quantile(x2, probs = 0.75)))
margins(probit_1, at=list(x1=quantile(x1, probs = 0.75), x2=quantile(x2, probs = 0.75)))
  
#interpretación de efecto marginal mayor que 1
gridExtra::grid.arrange(
  ggplot(data=NULL,aes(x=x1+x2+I(x2^2), y=y)) + geom_point() + 
    stat_smooth(method=glm, method.args=list(family=binomial (link = "logit")), se=T)+
    geom_abline(slope = 1/(3.70239+0.373206), intercept = 0.8617, colour='red')+
    ylab(substitute(y))+xlab(substitute(x1+x2+I(x2^2)))+
    ggtitle(label = "Modelo de Regresión Logístico",
            subtitle = paste(substitute(y),"vs",substitute(x1+x2+I(x2^2)))),
  
  ggplot(data=NULL,aes(x=x1+x2+I(x2^2), y=y)) + geom_point() + 
    stat_smooth(method=glm, method.args=list(family=binomial (link = "probit")), se=T)+
    geom_abline(slope = 1/(3.450295+0.352486), intercept = 0.8617, colour='red')+
    ylab(substitute(y))+xlab(substitute(x1+x2+I(x2^2)))+
    ggtitle(label = "Modelo de Regresión Normal Estándar",
            subtitle = paste(substitute(y),"vs",substitute(x1+x2+I(x2^2))))
)
#Bondad de ajuste de los modelos####
  #Porcentaje correcto predicho
  table(Observado = y, Predicho = round(fitted(mpl_1))) 
  table(Observado = y, Predicho = round(fitted(logit_1))) 
  table(Observado = y, Predicho = round(fitted(probit_1)))
  
  Porcentaje_correcto_0=0.805; Porcentaje_correcto_1=0.957
  Porcentaje_correcto_general=0.5*Porcentaje_correcto_0+0.5*Porcentaje_correcto_1
  Porcentaje_correcto_general*100
  
  #Pseudo R^2 de McFadden
  L_o_log<-logLik(glm(y~1, family=binomial (link = "logit")))
  L_nr_log<-logLik(logit_1)
  1 - L_nr_log/L_o_log
  
  L_o_pro<-logLik(glm(y~1, family=binomial (link = "probit")))
  L_nr_pro<-logLik(probit_1)
  1 - L_nr_pro/L_o_pro
  
  summary(logit_1)$aic
  summary(probit_1)$aic
  
  detach(Datos_binaria)
  