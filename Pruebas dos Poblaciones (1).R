
library(survival)
############################################################
############## Dos Poblaciones #############################
############################################################

t1<-c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161)
d1<-c(1,1,0,1,1,0,1,1,0,1,0)
t2<-c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45)
d2<-c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

t<-c(t1,t2);d<-c(d1,d2);
grupo<-c( rep(1,length(d1)),rep(2,length(d2)) )
cbind(t,d,grupo)

#KAPLAN-MEIER
y1<-Surv(t1,d1);y2<-Surv(t2,d2);y<-Surv(t,d)
sur<-(survfit(y ~ grupo))


#######Gráficas
plot(sur)
plot(sur,mark.time=T,mark=1,lty=3:1,yscale=100)

plot(sur,mark.time=T,lty=3:1,yscale=100)
plot(sur,mark.time=T,mark=2:1,lty=3:1,yscale=100)
plot(sur,conf.int=T,mark.time=T,mark=4,lty=1:4,yscale=100)

plot(sur,axes=F,xlab="Time (in months)",ylab="Survival",col=c(2,4),lwd=3:3)
axis(1,at=seq(0,170,5))
axis(2,at=seq(0,1,0.1))
title("Time to event")
legend(100,0.8,c("Treatement group","Control group"),lty=1:1,col=c(2,4),lwd=3:3)

plot(sur,conf.int=T,mark=7,col=c(2,4),lwd=c(3,1,1,3,1,1) )
axis(1,at=seq(0,170,20))
axis(2,at=seq(0,1,0.1))
title("Time to event")
legend(100,0.8,c("Treatment group","Control group"),col=c(2,4),lty=1:1)

####### Pruebas de hipótesis (fleming-harrington) #######
## Directamente con instrucciones R ##
# w(Z)=S(t)^rho; esto es q=0, y p=rho el escalar que controla el tipo de prueba
# Prueba de peto-peto
survdiff(y~grupo, rho=1)
# Prueba de log-rang
survdiff(y~grupo, rho=0)

#llamar objetos de la prueba
names(survdiff(y~grupo,rho=1))
survdiff(y~grupo, rho=0)$chisq
survdiff(y~grupo,rho=0)$var #matriz de varianzas y covarianzas de la estad stica de prueba


#### Verificación prueba log-rank ####
ti0=summary(survfit(y~1))$time
Di0=summary(survfit(y~1))$n.event
ni0=summary(survfit(y~1))$n.risk
ni1=c(11 ,11,  11, 10, 10, 8,  7, 6,  5,  5,  4,  4,  3,  3,  2)
data.frame(t=ti0, n=ni0, D=Di0, n1=ni1, n2=ni0-ni1, Ei=ni1/ni0*Di0)
sum(ni1/ni0*Di0)
summary(survfit(y1~1))
sum(summary(survfit(y1~1))$n.event)

LogR=survdiff(y~grupo, rho=0);LogR
Obs=LogR$obs;Esp=LogR$exp;VarLogR=LogR$var

# Estadísticas Zj
Z1=Obs[1] - Esp[1]; Z1
Z2=Obs[2] - Esp[2]; Z2

# Estadística de prueba
ZW=(Obs[1]-Esp[1])^2 / VarLogR[1,1];ZW
ZW=(Obs[2]-Esp[2])^2 / VarLogR[2,2];ZW
1-pchisq(ZW,1)


## Creando una función para la prueba de F-H ##
#Cálculo de vectores para ingresar a la función
sur=summary(survfit(y ~ 1));
S=sur$surv;tI=sur$time;R=sur$n.risk;D=sur$n.event
sur1=summary(survfit(y1 ~ 1));t1I=sur1$time;d1I=sur1$n.event
#sur2=summary(survfit(y2 ~ 1));t2I=sur2$time;d2I=sur2$n.event

fleming = function(S,tI,R,D,Y1I,t1I,d1I,p,q){
  n=length(tI)
  R1=NULL;d1=rep(0,n) ; #R1=rep(NA,n)
  #Cálculo de R1i y d1i para cada ti de la muestra combinada
  for(i in 1:n){
    R1[i]=sum(Y1I>=tI[i])
    for(j in 1:length(t1I)){if(tI[i]==t1I[j]) d1[i]=d1I[j]}
  }
  pesos=c(1,S[1:n-1])^p * (1- c(1,S[1:n-1]))^q
  diffnum = d1 - R1 * D/R
  num=pesos%*%diffnum
  diffden=R1/R*(1-R1/R)*D*(R-D)/ifelse((R-1)>0,R-1,1)
  den=sqrt(pesos^2%*%diffden)
  Z=num/den
  pval=1-pchisq(Z^2,1)
  datos=cbind(tI,R,D,R1,d1)
  cat("\n","Matriz de información","\n")
  print(datos)
  pval= 1-pchisq(Z^2,1)
  cat("\n","Estadística de prueba chi2=",Z^2)
  cat("\n","valor crítico de la prueba=",pval,"\n","\n")
}
ni1


# cálculo de la función
# prueba de Peto:
fleming(S,tI,R,D,t1,t1I,d1I,1,0)
survdiff(y~grupo, rho=1)
#prueba de log-rang
fleming(S,tI,R,D,t1,t1I,d1I,0,0)
survdiff(y~grupo, rho=0)

#otros valores de p y q
fleming(S,tI,R,D,t1,t1I,d1I,0,1) # 0.1048542
fleming(S,tI,R,D,t1,t1I,d1I,1,1)

fleming(S,tI,R,D,t1,t1I,d1I,0,6)







