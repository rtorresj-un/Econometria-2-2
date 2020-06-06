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

#Cargamos los paquetes. 

library(AER);library(foreign); library(stargazer); 
library(arm);library(lmtest);library(estimatr)  
library(tidyverse); library(broom);library(stargazer);library(wooldridge)

#Variable Instrumental ####  

Datos_instrum<-read.csv(file.choose())
summary(Datos_instrum)
attach(Datos_instrum)

#Sobre la base 
plot(x1,y)
plot(x2,y)


#1. Modelo por MCO
vi_mco<-lm(y ~ x1 + x2, data = Datos_instrum)
summary(vi_mco)
stargazer::stargazer(vi_mco)

#2. Instrumentos, ¿Qué condiciones deben cumplir para ser válidos?

cor(x1,z1) #0.5908596
cor(x1,z2) #0.8198656
cor(x1,z3) #0.4272298
cor(x1,z4) #-0.01204702
cor(x1,z5) #0.407697

#Gráficos
#z1
ggplot(Datos_instrum, aes_(x = z1,y = x1))+ geom_point()+
      geom_smooth(method = "lm")+ xlab("z1")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z1,x1")
#z2
ggplot(Datos_instrum, aes_(x = z2,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z2")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z2,x1")
#z3
ggplot(Datos_instrum, aes_(x = z3,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z3")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z3,x1")
#z4 nube
ggplot(Datos_instrum, aes_(x = z4,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z4")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z4,x1")
#z5
ggplot(Datos_instrum, aes_(x = z5,y = x1))+ geom_point()+
  geom_smooth(method = "lm")+ xlab("z5")+ylab("x1")+
  theme (text = element_text(size=8)) + 
  ggtitle ("Correlación z5,x1")

relevancia<-lm(x1~z1+z2+z3+z4+z5, data=Datos_instrum)
summary(relevancia)

relevancia1<-lm(x1~z1+z2+z3+z5, data=Datos_instrum); summary(relevancia1)
relevancia2<-lm(x1~z1+z2+z3+z4, data=Datos_instrum);summary(relevancia2)
relevancia3<-lm(x1~z2+z3, data=Datos_instrum);summary(relevancia3)
relevancia4<-lm(x1~z2, data=Datos_instrum);summary(relevancia4)

car::linear.hypothesis(z1=z2,relevancia1)

cor(z1,z2)
#exogeneidad
# Estadístico f>10 en 1etapa

Etapa1 <-lm(x1~x2+z1+z2+z3+z5, data=Datos_instrum); summary(Etapa1)
Etapa1.1<- lm(x1~x2+z1+z3,data = Datos_instrum);summary(Etapa1.1)
Etapa1.1<- lm(x1~x2+z1+z2+z3,data = Datos_instrum);summary(Etapa1.1)
Etapa1.3 <-lm(x1~x2+z1+z2+z3+z4+z5, data=Datos_instrum); summary(Etapa1.3)


#3. Regresion por VI asumiendo x2 exogena
VI_1=ivreg(y~x1+x2|x2+z1+z2+z3+z4+z5, data = Datos_instrum);summary(VI_1,diagnostics = TRUE)
VI_2=ivreg(y~x1+x2|x2+z1+z2+z3+z5, data = Datos_instrum);summary(VI_2,diagnostics = TRUE)
VI_3=ivreg(y~x1+x2|x2+z2, data = Datos_instrum);summary(VI_3,diagnostics = TRUE)
VI_4=ivreg(y~x1+x2|x2+z1, data = Datos_instrum);summary(VI_4,diagnostics = TRUE)


stargazer(VI_1, vi_mco, type = "text")
#son significativs z1 y z2 por aparte 

stargazer(VI_1,VI_2,VI_3, VI_4,type = "text")


#4. asuma x2 endógena. Suponga exogeneidad del instrumento y estime
#5.Compare las estimaciones
#6. escoja el mejor modelos. haga la regresión robusta y compare
  #R.VI.Robust = iv_robust(log(wage) ~ educ| fatheduc , data=data.VI);summary(R.VI.Robust)

#Variable binaria####
Datos_binaria<-readr::read_csv('binar_grupo25.csv')
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

stargazer::stargazer(mpl_1,logit_1,probit_1, type = 'text')

graf.reglineal(dependiente = y,independiente = x1+x2+I(x2^2))
  require(ggplot2)
  ggplot(data=NULL,aes(x=x1+x2+I(x2^2), y=y)) + geom_point() + 
    stat_smooth(method=glm, method.args=list(family="binomial"), se=T)+
    ylab(substitute(dependiente))+xlab(substitute(independiente))+
    ggtitle(label = "Modelo de Regresión Logístico",
            subtitle = paste(substitute(dependiente),"vs",substitute(independiente)))
#Efectos marginales####
  APE_mpl<-coef(mpl_1)
  
  Predlogit_1 <- mean(dlogis(predict(logit_1)))
  APE_logit<-Predlogit_1 * coef(logit_1)
  
  Predprobit_1 <- mean(dnorm(predict(probit_1)))
  APE_probit<-Predprobit_1 * coef(probit_1)

  cbind(APE_mpl, APE_logit, APE_probit)
    
  #Efectos marginales 1, 2, 3 cuantil####
  APE_mpl_2<-coef(mpl_1)
  
  Predlogit_2 <- quantile(dlogis(predict(logit_1)), probs = 0.25)
  APE_logit_2<-Predlogit_2 * coef(logit_1)
  
  Predprobit_2 <- quantile(dnorm(predict(probit_1)), probs = 0.25)
  APE_probit_2<-Predprobit_2 * coef(probit_1)
  
  cbind(APE_mpl_2, APE_logit_2, APE_probit_2)
  
  APE_mpl_3<-coef(mpl_1)
  
  Predlogit_3 <- quantile(dlogis(predict(logit_1)), probs = 0.50)
  APE_logit_3<-Predlogit_3 * coef(logit_1)
  
  Predprobit_3 <- quantile(dnorm(predict(probit_1)), probs = 0.50)
  APE_probit_3<-Predprobit_3 * coef(probit_1)
  
  cbind(APE_mpl_3, APE_logit_3, APE_probit_3)
  
  APE_mpl_4<-coef(mpl_1)
  
  Predlogit_4 <- quantile(dlogis(predict(logit_1)), probs = 0.75)
  APE_logit_4<-Predlogit_4 * coef(logit_1)
  
  Predprobit_4 <- quantile(dnorm(predict(probit_1)), probs = 0.75)
  APE_probit_4<-Predprobit_4 * coef(probit_1)
  
  cbind(APE_mpl_4, APE_logit_4, APE_probit_4)
  