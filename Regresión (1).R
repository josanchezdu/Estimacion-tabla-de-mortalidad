
rm(list = ls())
######################################
##$# Regresión Paramétrica para T ####
######################################
library(survival);

# Ejemplo Cáncer. Conformación de vectores de análisis
# Grupo Terapia (grupo=1)
t1<-c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161)
d1<-c(1,1,0,1,1,0,1,1,0,1,0)
# Grupo Control (grupo=0)
t2<-c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45)
d2<-c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

t<-c(t1,t2);d<-c(d1,d2);
grupo<-c( rep(1,length(d1)),rep(0,length(d2)) )
y<-Surv(t,d)
y1=Surv(t1,d1);y2=Surv(t2,d2)

cbind(t,d,grupo)

######################
####### Weibul #######
######################

Reg.Wei=survreg(y~grupo,dist="weibull")
summary(Reg.Wei)
muE=as.numeric(Reg.Wei$coefficients[1]);muE
gammaE = as.numeric(Reg.Wei$coefficients[2]);gammaE
sigmaE = Reg.Wei$scale;sigmaE

### Estimación de los parámetros de T~Weibull, pero bajo la reparametrización
## rho = exp(-mu/sigma)  ;  alpha = 1/sigma ; beta=-gamma/sigma 
##  S(t)=exp(-rho*t^alpha * exp(beta*Z)) # Z=grupo
alpha=1/sigmaE;alpha
rho=exp(-muE/sigmaE);rho
beta=-(gammaE/sigmaE);beta


# Estimación usando directamente la distribución de T reparametrizada ####
# S(t)=exp(-rho*t^alpha * e^(beta*Z))  ;  
# h(t)=alpha*rho*t^(alpha-1)* e^(beta*Z) ; beta=-gamma/sigma
# Li=lambda(y_i)^(delta_i) * S(y_i)
ff<-function(b){
  alpha=b[1];rho=b[2];beta=b[3]
  e0=exp(beta*grupo);
  -sum(ifelse(d==1,log( alpha*rho*(t^(alpha-1))*e0 )-rho*(t^alpha)*e0,-rho*(t^alpha)*e0 ))
  #-sum(ifelse(d==1,log( alpha)+log(rho)+(alpha-1)*log(t)+beta*grupo -rho*(t^alpha)*e0,-rho*(t^alpha)*e0 ))
}
b=c(1.3,0.0001,1.17);#b(alpha , rho, beta)
Sol=optim(b,ff,hessian=T,method = "Nelder-Mead");Sol

ParE=as.numeric(Sol$par);ParE	# Parámetros estimados con optim
alpha;rho;beta                # Parámetros estimados con survreg
VarE=diag(solve(Sol$hessian)); VarE		# varianza de alpha, rho y beta


########################
##### LogLogística #####
########################
Reg.logL=survreg(y~grupo,dist="loglogistic")
summary(Reg.logL)


#######################
###### Cox ############
#######################


# Estimación modelo de Cox paquete Survival
Cox0=coxph(y~grupo)
Beta=summary(coxph(y~grupo))$coefficients[1];Beta


# Cálculos básicos para las estimaciones no-paramétricas de h0 y H0
 fit<- survfit(y~1,type="fh2")
 tim<- summary(fit)$time # tiempos
 Di<- summary(fit)$n.event # Casos observados
 Ni<- summary(fit)$n.risk # Casos probables
 N=length(tim)


## Estimación del riego basal y riesgo acumulado basal (Total pob)
l0=NULL
for(i in 1:N){
	eb=0
	for(j in 1:length(grupo)){
		if( t[j] >= tim[i] ) eb=eb+exp(Beta*grupo[j])
	}
	l0[i]=Di[i]/eb
}
l0		# Riesgo basal
cumsum(l0)	# Riesgo basal acumulado

# Cálculo riesgo Cox con base en riesgo basal l0 por estimación breslow
l0.1 = l0*exp(Beta*1); l0.1 	# Riesgo Terapia
l0.0 = l0*exp(Beta*0); l0.0		# Riesgo Tratamiento estandar









