rm(list=ls(all=TRUE))
library(survival);

#=========================
# Ajustes NO-Paramétricos
#=========================


#==============================================
# Cálculos para Grupo Control (sin tratamiento)
#==============================================

# Información muestral ( Y , delta)
Y2<-c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45)
d2<-c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

#####  Procedimientos para T2 #########

# a) Tiempos a Evento
t2=as.numeric(names(table(Y2[d2==1])));t2

# b) Individuos a Riesgo. t: ingresa tiempos a evento. Y: valores observados
nriscT <- function(t,Y){
	val <- rep(0, length(t))
	for(i in 1:length(t))
		val[i] <- length(( Y[Y>=t[i]] ))
	val
}
n2=nriscT(t2,Y2);n2

# c) Número de Muertes
nmuertos = function(t,Y,d){
	Dx = NULL
	for(i in 1:length(t)){
		Dx[i] = sum( (Y==t[i])*(d==1)   )
	}
	Dx
}
D2 = nmuertos(t2, Y2, d2); D2

## kaplan Meier
Skm = function(D,n){
	S = NULL
	S[1] = 1 - D[1] / n[1]
	for(i in 2:length(D)){
		S[i] = S[i-1] * ( 1 - D[i]/n[i])
	}
	S
}
S.km2 = Skm(D2, n2); S.km2
Var.km2 = (S.km2^2) * cumsum(  D2 / (n2*(n2-D2 ) )  ); Var.km2

LI.km2 = S.km2 - 1.96 * sqrt(Var.km2);LI.km2[LI.km2>1] <- 1;LI.km2[LI.km2 < 0] <- 0; LI.km2
LS.km2 = S.km2 + 1.96 * sqrt(Var.km2);LS.km2[LS.km2>1] <- 1;LS.km2[LS.km2 < 0] <- 0; LS.km2
cbind( t2, n2, D2, S.km2, LI.km2 , LS.km2  )

## Verificación de cálculos con base en el paquete Survival 
# Información unificada del tiempo al evento 
T2 = Surv(Y2,d2);T2
# Cálculo de estimaciones con instrucción "survfit"
Sur.km2 = survfit( T2 ~ 1, type="kaplan-meier", error = "g", conf.type="plain" )
summary(Sur.km2)
plot(Sur.km2)


### Nelson AAlen

h.NA2 = D2 / n2; h.NA2
H.NA2 = cumsum(h.NA2); H.NA2
S.NA2 = exp( -H.NA2 ); S.NA2
Var.NA2 = (S.NA2^2) * cumsum(  D2 / (n2^2 )  ); Var.NA2 

LI.NA2 = S.NA2 - 1.96 * sqrt(Var.NA2);LI.NA2[LI.NA2>1] <- 1;LI.NA2[LI.NA2 < 0] <- 0; LI.NA2
LS.NA2 = S.NA2 + 1.96 * sqrt(Var.NA2);LS.NA2[LS.NA2>1] <- 1;LS.NA2[LS.NA2 < 0] <- 0; LS.NA2
cbind( t2, n2, D2, S.NA2, LI.NA2 , LS.NA2  )

# Verificación de cálculos con base en el paquete Survival
Sur.NA2 = survfit( T2 ~ 1, type="fleming-harrington", error = "t", conf.type="plain" )
summary(Sur.NA2)
# Gráfica de la sobrevivencia
plot(Sur.NA2)

# Gráfica del riesgo
plot(h.NA2);lines(h.NA2)


#=============================
# Cálculos para los dos Grupos
#=============================

# Información muestral ( Y , delta)
Y1<-c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161)
d1<-c(1,1,0,1,1,0,1,1,0,1,0)
Y2<-c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45)
d2<-c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

# Grupo combinado
Y=c(Y1,Y2);d=c(d1,d2)
Grupo=c( rep(1,length(Y1)), rep(2,length(Y2)) )
cbind(Y,d,Grupo)

# Kaplan-Meier
T<-Surv(Y,d);T
sur<-survfit(T~Grupo);
summary(sur)
plot(sur, xlab = "Time to event", ylab = "S(t)")


#####################################
######### Ajustes Paramétricos  #####
#####################################

#===================================
# Cálculos para el Grupo Tratamiento
#===================================

# Información muestral ( Y , delta)
Y1<-c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161)
d1<-c(1,1,0,1,1,0,1,1,0,1,0)

# Estimación no paramétrica (para comparar frente a la paramétrica)
T1<-Surv(Y1,d1)
sur1<-survfit(T1~1,type="fleming-harrington")
# Riesgo acumulado
H1.NA<- -log(summary(sur1)$surv);H1.NA
# Tiempos a evento
tim1<-summary(sur1)$time;tim1
# Individuos a riesgo
n1=summary(sur1)$n.risk;n1
# Número de eventos
D1 = summary(sur1)$n.event;D1
# Riesgo
h1.NA = D1/n1 ; h1.NA


## Análisis gráfico para determinar el tipo de distribución que modela a T1
par(mfrow=c(2,2))
  ########Verificando modelo exponencial##############
  plot(tim1,H1.NA, xlab = "Time to event", ylab = "cumulative probability")
  lines(tim1,H1.NA)
  title("Exponencial")
  ########verificación modelo Weibull##########
  llh1<-log(H1.NA)
  lt1<-log(tim1)
  plot(lt1,llh1, xlab = "Ln(Time to event)")
  lines(lt1,llh1)
  title("Weibull")
  ########verificación modelo LogLogístico##########
  th1=log(exp(H1.NA)-1)
  plot(lt1,th1, xlab = "Ln(Time to event)")
  lines(lt1,th1)
  title("Log-Logistica")
  ########verificación modelo LogNormal##########
  th2=pnorm(1-exp(-H1.NA),mean=0,sd=1)
  plot(lt1,th2, xlab = "Ln(Time to event)")
  lines(lt1,th2)
  title("Log-Normal")
par(mfrow=c(1,1))

##### Estimar parámetros de una distribución Weibull
### Maximizar logL de una Weibull. Minimizar -LogL ###
# S(t)=exp(-alpha*t^k) ;  h(t)=k*alpha*t^(k-1)  -   Li=h(y_i)^(delta_i) * S(y_i)
ff<-function(b){
   -sum(ifelse(d1==1,log(b[1]*b[2]*(Y1^(b[1]-1)))-b[2]*(Y1^b[1]),-b[2]*(Y1^b[1]) ))
}
# b(k , alpha)
b=c(1,1)
Par.W  = nlminb(b,ff,lower = c(0,0), upper = c(2,1))$par; Par.W 
# optim(b,ff)$par
k1 = Par.W[1];k1
alphaW1 = Par.W[2];alphaW1

### Gráfica de las funciones Weibull
t.1 = seq(0, 160, 0.05);
hWei1=k1*alphaW1*(t.1^(k1-1))
Swei1=exp(-(alphaW1*(t.1^k1)))
fwei1=hWei1*Swei1
plot(t.1,fwei1)
plot(t.1,hWei1)
plot(T1~1, xlab = "Time to event", ylab = "Proportion Surviving");
lines(t.1,Swei1,col=2);

##### Estimar parámetros de una distribución LogLogística
### Maximizar Log-L. Minimizar -Log L ######
# S(t)= 1/(1+lambda*t^alpha)  h(t)=lambda*alpha*t^(alpha-1)/(1+lambda*t^alpha)  -   Li=h(y_i)^(delta_i) * S(y_i)
ff<-function(b){
   -sum(ifelse(d1==1,log( (b[1]*b[2]*Y1^(b[1]-1))/(1+b[2]*Y1^b[1]) )-log(1+b[2]*Y1^b[1]), -log(1+b[2]*Y1^(b[1])) ))
}
#b(alpha , lambda)
b=c(0.5,0.5);
Par.LogL = nlminb(b,ff,lower = c(0,0), upper = c(100,100))
#optim(b,ff)
alphaL = Par.LogL$par[1];alphaL
lambdaL = Par.LogL$par[2];lambdaL

## Gráfica de las funciones loglogística
hLogL=alphaL*lambdaL*(t.1^(alphaL-1))/( 1+lambdaL*(t.1^alphaL) )
SLogL=1/( 1+lambdaL*(t.1^alphaL) )
fLogL=hLogL*SLogL
plot(t.1,fLogL);
plot(t.1,hLogL)
plot(t.1,SLogL)


### Gráfica conjunta de las sobrevencias estimadas
plot(T1~1, xlab = "Time to event", ylab = "Proportion Surviving");
lines(t.1,Swei1,col=2);
lines(t.1,SLogL,col=3)
legend(80,0.8,c("Kaplan-Meier","Weibul","LogLogística"),col=c(1,2,3),lwd=c(2,2,2,2),lty=c(1,1,1,1))


### Gráfica conjunta de los riesgos estimados
plot(tim1,h1.NA, ylim=c(0,0.5),xlim=c(0,100));lines(tim1,D1/n1)
lines(t.1,hWei1,col="red" , lwd=3)
lines(t.1,hLogL,col="green" ,lwd=3)


## Estimación del riesgo Kernell con base en el paquete "muhaz"
library(muhaz)
h1.R = muhaz(Y1, d1,kern="epanechnikov", bw.smooth=10)
plot(h1.R$haz.est)
plot(tim1,h1.NA,ylim=c(0,0.5)); lines(tim1,h1.NA)
lines( h1.R$haz.est,col=2)

### Gráfica de los riesgos paramétricos con el kernell del paquete "muhaz"
plot(t.1,hLogL,col="green" ,lwd=3 , ylim=c(0,0.08))
lines(t.1,hWei1,col="red" , lwd=3)
lines(seq(1,50,length.out =101) , h1.R$haz.est)













