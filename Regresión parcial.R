rm(list = ls())
library(survival);


##################################################################
######################### Punto 1 ################################
##################################################################

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

#LogLogística 

Reg.logL=survreg(y~grupo,dist="loglogistic")
summary(Reg.logL)

#Parametros del modelo
mu = coef(Reg.logL)[1]
gamma = coef(Reg.logL)[2]
sigma = Reg.logL$scale

# parámetros de la distribución log-logística
alpha = 1/sigma
rho = exp(-mu/sigma)
beta = exp(-gamma/sigma)

# Función de supervivencia S(t)= 1 / (1 + rho*t^alpha * exp(beta*t*Z)) # Z = grupo

S_loglog = function(t, rho, alpha, beta, z) {
  1/(1 + rho*(t^alpha)*exp(beta*z) )
}

t0 = 12

# Note que:  E(T|T ≥ t0) = t0 + E(T-t0|T ≥ t0) = t0 + vrm(t0) = t0 + (∫[t0,∞] S(t)dt)/S(t0)

# grupo 1:  z = 0

integral1 = integrate(function(t) S_loglog(t, rho, alpha, beta, 0), 
                      lower = t0, upper = Inf)

vrm1 = integral1[[1]]/S_loglog(t0, rho, alpha, beta, 0)
print("Grupo 1: ")
print(vrm1+12)

#grupo 2: z = 1
integral2 = integrate(function(t) S_loglog(t, rho, alpha, beta, 1), 
                      lower = t0, upper = Inf)

vrm1 = integral1[[1]]/S_loglog(t0, rho, alpha, beta, 1)
print("Grupo 2: ")
print(vrm1+12)


##########################################################################
########################### Punto 2 ######################################
##########################################################################
library(survival)

# 1 hombre
# 0 mujer
Credito  = read.delim("C:/Users/jorge/Downloads/Crédito.txt")

# Objeto Surv
t = Credito$t 
d = Credito$dD
y = Surv(t,d)
plot(y)
# Covariantes
Sex = Credito$sexo
ED = Credito$edad

# Ajuste del modelo
modelo2 = survreg(y~Sex + ED ,dist="loglogistic")
summary(modelo2)

#Parametros del modelo
mu2 = coef(modelo2)[1]
gamma2 = coef(modelo2)[2]
sigma2 = modelo2$scale

# parámetros de la distribución log-logística
alpha2 = 1/sigma2
rho2 = exp(-mu2/sigma2)
beta2 = exp(-gamma2/sigma2)


####Interpretacion########

#Factor de aceleracion:
exp(-gamma2)
1/exp(-gamma2)
# 1.019∗M1 = M0 o 0.9815*M0 = M1
#El tiempo mediano hasta el incumplimiento (default) para un hombre es 0.9815
# veces el de una mujer, manteniendo constante la edad.

# o de otra forma, el tiempo hasta el incumplimiento (default) de las mujeres es 1.019  veces
# el tiempo mediano de incumplimineto de los hombres. Manteniendo constante la edad.


#Odds proporcionales:
exp(gamma2/sigma2)

#Decimos que el chance (odds) de no tener incumpliminento siendo hombre es 0.9727 veces el de las mujeres.

#razón de cambio en el modelo log-lineal:
gamma2
#que una mujer pase a ser hombre (manteniendo constante la edad),
#hace que el tiempo de incumplimineto disminuya en un 1.8%

#Ademas de las interpretaciones, notemos que no se rechaza la hipotesis nula (p-valor =  0.9479).
#Es decir, no hay diferencias significativas de tiempo de default entre hombres y mujeres,


# b) Calcular para un hombre de 40 años: Pr(24 < T <25 | T ≥24) = (s(24)-s(25))/s(24)

# Función de supervivencia S(t)= 1 / (1 + rho*t^alpha * exp(beta*t*Z)) # Z = grupo

S_loglog2 = function(t, rho, alpha, beta, z) {
  1/(1 + rho*(t^alpha)*exp(beta*z) )
}

s24 = S_loglog2(24, rho2, alpha2, beta2, 0)
s25 = S_loglog2(25, rho2, alpha2, beta2, 0)

(s24-s25)/s24

#Para un hombre de 40 años que ha llegado a los 24 meses sin incumplir, 
#la probabilidad de que incumpla entre el mes 24 y el 25 es aproximadamente 3.39%.

################################################################################
##########################Punto 3###############################################
################################################################################

D = d
sex = Sex
edad = ED

m.cox = coxph(y ~ sex + edad )
summary(m.cox)

#Parametros estimados

exp(coefficients(m.cox))

# El sexo no es significativo como variable explicativa. Indica que el riesgo de
# insolvencia en hombres es 1.12 veces el riesgo de insolvencia en mujeres.

# el riesgo de insolvencia en una edad t es 0.96 veces el riesgo de insolvencia 
# para la edadt - 1 (edad totalmente significativa, se rechaza la hipótesis de
# que la edad no implica cambios en la insolvencia)

# Calcular para un hombre de 40 años: Pr(24 < T <25 | T ≥24) = (s(24)-s(25))/s(24)
# predicciones de la sobrevivencia (para calcular la probabilidad)

pred = survfit(m.cox, newdata = data.frame(edad = 40,sex = 1))
plot(pred$surv, type = "l")
sh24 =summary(pred, times = 24)$surv
sh25 = summary(pred, times = 25)$surv

(sh24-sh25)/sh24

# La probabilidad es cero, 

