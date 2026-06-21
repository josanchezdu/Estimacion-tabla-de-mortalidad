# Parcial de sobrevivencia
# Jorge Andres Sanchez

################################################
######## Punto 1 ###############################
################################################

# Datos
Y1<-c(0.5, 1, 0.75, 0.25, 1.25)
d1<-c(1, 1, 1, 0, 0) # Los ceros indican censura en los datos, en este caso, por izquierda

# Función de verosimilitud - distribución 
ff_loglogistic <- function(params){
  rho <- params[1]
  alpha <- params[2]
  
  # Inicializar el vector de log-verosimilitud
  loglik <- numeric(length(d1))
  
  # Evento exacto (d=1): contribución f(y) = (ρ α t^(α-1)) / (1 + ρ t^α)^2
  idx1 <- (d1 == 1)
  if(any(idx1)){
    term <- rho * alpha * Y1[idx1]^(alpha-1)
    denominator <- (1 + rho * Y1[idx1]^alpha)^2
    loglik[idx1] <- log(term) - log(denominator)
  }
  
  # Censura por izquierda (d=0): contribución F(y) = 1 - S(y) = ρ t^α / (1 + ρ t^α)
  idx2 <- (d1 == 0)
  if(any(idx2)){
    numerator <- rho * Y1[idx2]^alpha
    denominator <- 1 + rho * Y1[idx2]^alpha
    loglik[idx2] <- log(numerator) - log(denominator)
  }

  # Retornar el negativo de la suma para minimización
  -sum(loglik)
}

# Valores iniciales para rho y alpha
params_init = c(1, 1)
# Optimización 
result <- nlminb(params_init, ff_loglogistic, lower = c(0,0), upper = c(100,100))

# Mostrar resultados
cat("Parámetro rho estimado:", result$par[1], "\n")
cat("Parámetro alpha estimado:", result$par[2], "\n")

#################################################
#######Punto 3###################################
#################################################

Insolvencia = read.delim("C:/Users/jorge/Downloads/Insolvencia.txt")
 
Y2 = Insolvencia$T
d2 = Insolvencia$d

# Estimación no paramétrica (Nelson Alen, para hallar graficos)
# Información unificada del tiempo al evento 
T2<-Surv(Y2,d2)
sur1<-survfit(T2~1,type="fleming-harrington")
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

t.1 = seq(0, 160, 0.05);
##Observando las graficas, el mejor estimador parametrico, parece estar entre la log-logistica, la weibull, y la log normal.
## En nuestro caso, despues de comparas todas con la esimacion kaplan-Meir, se determino que la mejor era la log-logistica



##### Estimar parámetros de una distribución LogLogística
### Maximizar Log-L. Minimizar -Log L ######
# S(t)= 1/(1+lambda*t^alpha)  h(t)=lambda*alpha*t^(alpha-1)/(1+lambda*t^alpha)  -   Li=h(y_i)^(delta_i) * S(y_i)
ff<-function(b){
  -sum(ifelse(d2==1,log( (b[1]*b[2]*Y2^(b[1]-1))/(1+b[2]*Y2^b[1]) )-log(1+b[2]*Y2^b[1]), -log(1+b[2]*Y2^(b[1])) ))
}
#b(alpha , lambda)
b=c(0.5,0.5);
Par.LogL = nlminb(b,ff,lower = c(0,0), upper = c(100,100))
#optim(b,ff)
alphaL = Par.LogL$par[1];alphaL
lambdaL = Par.LogL$par[2];lambdaL

## Gráfica de las funciones loglogística
SLogL=1/( 1+lambdaL*(t.1^alphaL) )


#Estimador (Kaplan-Meier)
T2 = Surv(Y2,d2);T2
# Haremnos el calculo con "survfit"
Sur.km2 = survfit( T2 ~ 1, type="kaplan-meier", error = "g", conf.type="plain" )
summary(Sur.km2)


### Gráfica conjunta de las sobrevencias estimadas
plot(T2~1, xlab = "Time to event", ylab = "Proportion Surviving");
lines(t.1, SLogL,col=2);

legend(2,0.3,c("Kaplan-Meier", "Loglogistica"),col=c(1,2),lwd=c(2,2,2,2),lty=c(1,1,1,1))




#######################################
#######Punto 4#########################
########################################

# Información muestral ( Y , delta)
Y3 = c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161)
d3 = c(1,  1,  0,  1,  1,  0,  1,  1,  0,  1,   0) # 0: indica censura a derecha

# Estimación no paramétrica (Estimador de Kaplan- Meier)

# Información unificada del tiempo al evento 
T3 = Surv(Y3,d3);T3
# Haremnos el calculo con "survfit"
Sur.km3 = survfit( T3 ~ 1, type="kaplan-meier", error = "g", conf.type="plain" )
summary(Sur.km3)

# Estimacion parametrica 

##### Estimar parámetros de una distribución Weibull
### Maximizar logL de una Weibull. Minimizar -LogL ###
# S(t)=exp(-alpha*t^k) ;  h(t)=k*alpha*t^(k-1)  -   Li=h(y_i)^(delta_i) * S(y_i)
ff<-function(b){
  -sum(ifelse(d3==1,log(b[1]*b[2]*(Y3^(b[1]-1)))-b[2]*(Y3^b[1]),-b[2]*(Y3^b[1]) ))
}
# b(k , alpha)
b=c(1,1)
Par.W  = nlminb(b,ff,lower = c(0,0), upper = c(2,1))$par; Par.W 
# optim(b,ff)$par
k1 = Par.W[1];k1
alphaW1 = Par.W[2];alphaW1

### Gráfica de las funciones Weibull
t.1 = seq(0, 160, 0.05);
Swei1=exp(-(alphaW1*(t.1^k1)))


##### Estimar parámetros de una distribución LogLogística
### Maximizar Log-L. Minimizar -Log L ######
# S(t)= 1/(1+lambda*t^alpha)  h(t)=lambda*alpha*t^(alpha-1)/(1+lambda*t^alpha)  -   Li=h(y_i)^(delta_i) * S(y_i)
ff<-function(b){
  -sum(ifelse(d3==1,log( (b[1]*b[2]*Y3^(b[1]-1))/(1+b[2]*Y3^b[1]) )-log(1+b[2]*Y3^b[1]), -log(1+b[2]*Y3^(b[1])) ))
}
#b(alpha , lambda)
b=c(0.5,0.5);
Par.LogL = nlminb(b,ff,lower = c(0,0), upper = c(100,100))
#optim(b,ff)
alphaL = Par.LogL$par[1];alphaL
lambdaL = Par.LogL$par[2];lambdaL

## Gráfica de las funciones loglogística
SLogL=1/( 1+lambdaL*(t.1^alphaL) )


##### Estimar parámetros de una distribución LogNormal
### Maximizar Log-L. Minimizar -Log L ######
ff_lognormal <- function(params){
  mu <- params[1]
  sigma <- params[2]
  
  # Inicializar el vector de log-verosimilitud
  loglik <- numeric(length(d3))
  
  # Evento exacto (d=1): contribución f(y)
  idx1 <- (d3 == 1)
  if(any(idx1)){
    # f(t) = (1/(σ*t*sqrt(2π))) * exp(-0.5*((log(t)-μ)/σ)^2)
    loglik[idx1] <- -log(sigma * Y3[idx1] * sqrt(2*pi)) - 
      0.5 * ((log(Y3[idx1]) - mu)/sigma)^2
  }
  
  # Censura por derecha (d=0): contribución S(y) = 1 - Φ((log(y)-μ)/σ)
  idx0 <- (d3 == 0)
  if(any(idx0)){
    # S(t) = 1 - pnorm((log(t)-mu)/sigma)
    loglik[idx0] <- log(1 - pnorm((log(Y3[idx0]) - mu)/sigma))
  }
  # Retornar el negativo de la suma para minimización
  -sum(loglik)
}
# Valores iniciales para mu y sigma
params_init <- c(0, 1)

# Optimización usando nlminb con restricciones
result <- nlminb(params_init, ff_lognormal, 
                 lower = c(-Inf, 0.001), 
                 upper = c(Inf, Inf))

# Extraer parámetros estimados
mu <- result$par[1]; cat("Parámetro mu estimado:", mu, "\n")
sigma <- result$par[2]; cat("Parámetro sigma estimado:", sigma, "\n")


## Gráfica de las funciones logNormal
SLogN = 1 - pnorm((log(t.1) - mu)/sigma)



### Gráfica conjunta de las sobrevencias estimadas
plot(T3~1, xlab = "Time to event", ylab = "Proportion Surviving");
lines(t.1,Swei1,col=2);
lines(t.1,SLogL,col=3);
lines(t.1,SLogN,col=4)
legend(80,0.8,c("Kaplan-Meier","Weibul","LogLogística", "Lognormal"),col=c(1,2,3, 4),lwd=c(2,2,2,2),lty=c(1,1,1,1))




