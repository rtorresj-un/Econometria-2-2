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


#Variable Instrumental ####  

Datos_instrum<-readr::read_csv('VI_grupo25.csv')
summary(Datos_instrum)

#1. Modelo por MCO
vi_mco<-lm(y ~ x1 + x2, data = Datos_instrum)
summary(vi_mco)
stargazer::stargazer(vi_mco)

#2. Instrumentos, ¿Qué condiciones deben cumplir para ser válidos?
#3. Regresion por VI asumiendo x2 exogena
#4. asuma x2 endógena. Suponga exogeneidad del instrumento y estime
#5.Compare las estimaciones
#6. escoja el mejor modelos. haga la regresión robusta y compare


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
  