# Jorge Andre Sanchez
library(data.table)
library(knitr)
library(shiny)
library(performance)
library(car)
library(glmnet)
library(tidyverse)
library(caret)
library(psych)
library(mlbench)
library(kableExtra)
library(lmtest)
library(glmtoolbox)
library(caret)
library(see)

#################################################
###############Primer Punto######################
#################################################

#Datos
datos_bebes = data.frame(
  Sexo = factor(c(rep("Mujer", 12), rep("Hombre", 12))),
  Edad = c(
    40, 36, 40, 38, 42, 39, 40, 37, 36, 38, 39, 40,
    40, 38, 40, 35, 36, 37, 41, 40, 37, 38, 40, 38
  ),
  Peso = c(
    3317, 2729, 2935, 2754, 3210, 2817, 3126, 2539,
    2412, 2991, 2875, 3231, 2968, 2795, 3163, 2925,
    2625, 2847, 3292, 3473, 2628, 3176, 3421, 2975
  )
)

#a)
modelo1 = glm(Peso ~ Edad*Sexo, data = datos_bebes, family = gaussian(link = "identity"))


#b) 
summary(modelo1)

old <- sink(tempfile())
envelope(modelo3.1)
# Dado que el p-valor de la interaccion de Edad:SexoMujer es 0.663893 > 0.05, no tenemos evidencia suficiente para rechazar la Hipótesis Nula.
# Es decir, el efecto de la edad gestacional sobre el peso al nacer es el mismo para niños y niñas

#c)
check_model(modelo1)
#1) Es caro, que los residuos no son normales
#2) Hay serios problemas de multicolinealidad
#3) No se observan valores muchos valores atipicos, a exepcion de un posible valor influyente(que puede alterar la interpretacion)
#4) Parece tener problemas de heterodasticidad
plot(cooks.distance(modelo1))

#d)
#En este caso, solo vamos a interpretar las interacciones significativas

#Edad: Un aumento de una semana en la edad gestacional se asocia con un aumento esperado de 111.98 gramos en el peso al nacer
# manteniendo constante el efecto del 

#e)
nuevo_dato = data.frame(Edad = 40, Sexo = factor("Mujer"))

predict(modelo1, newdata = nuevo_dato,type = "response" )
# Se espera, que un bebe de sexo mujer, y edad 40 semanas tenga un peso promedio de 3074.333 gramos.


################################################################
#########################Punto 2################################
################################################################

#datos

datos_senilidad = data.frame(
  Escala = c(
    9, 13, 6, 8, 10, 4, 14, 8, 11, 7, 9, 7, 5, 14,
    13, 16, 10, 12, 11, 14, 15, 18, 7, 16, 9, 9, 11, 13, 15, 13, 10, 11, 6, 17, 14, 19,
    9, 11, 14, 10, 16, 16, 14, 13, 13, 9, 15, 10, 11, 12, 4, 14, 20, 10
  ),
  Senilidad = c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  )
)

#a)
#funcion de enlace logit
modelo2.1 = glm(Senilidad ~ Escala, data = datos_senilidad, family = binomial(link = "logit"))
#funcion de enlace Probit
modelo2.2 = glm(Senilidad ~ Escala, data = datos_senilidad, family = binomial(link = "probit")) 
#Funcion de enlace cloglog
modelo2.3 = glm(Senilidad ~ Escala, data = datos_senilidad, family = binomial(link = "cloglog")) 


AIC(modelo2.1); AIC(modelo2.2); AIC(modelo2.3)
BIC(modelo2.1); BIC(modelo2.2); BIC(modelo2.3)

#En este caso, elegimos el modelo 2(Funcion de enlace "Probit"), por tener menor Aic y bic
#Pero realmente la diferencia no es mucha


#b)
summary(modelo2.2)
#1. Prueba de Wald 
# Observamos que la prueba arrojo un pvalor de 0.00284 < 0.05. Es decir, 
# decidimos rechazar la hipotesis nula(($\beta_{\text{Wechsler}} = 0$). POr lo que, 
# El puntaje de Wechsler si afecta la probabilidad de tener senilidad

#
#Modelo mas simple.
modelo2.0 = glm(Senilidad ~ 1, data = datos_senilidad, family = binomial(link = "probit")) 

#
lrtest(modelo2.0, modelo2.2)
#Como el test arrojo un pvalor<0.05, decimos rechazar H0. Existen diferencias entre el modelo completo y el simple, 
# Por lo que, La inclusión del puntaje de Wechsler en el modelo mejora significativamente el ajuste 
# del modelo.

#c)

check_model(modelo2.2)
#se ve que, sus residuos no son normales. Pero obsevando el check model, vemos residuios uniformes
# ademas, se ve que la distribucionde los datos observados se acerca muy bien a los datos simulados por el modelo
# Y no se evidencia clara de violacion de linealidad(en el grafico 2)

plot(modelo2.2, which = 4)
#usando la regla de 4/(n+p) = 4/(54+1) =0.07 vemos que es posible tener entre 2 o tres datos influyentes
# pero obvervando el check model, no deberia tener mayor inconveniente

envelope(modelo2.2)
#Viendo el evelope, no vemos mayores problemas




#d) No se como se interpreta el enlace
1-qnorm(0.18801) 

#interpretacion: decimos que, por 

#e)
#prediccion
predict(modelo2.2, newdata = data.frame(Escala = 15), type = "response")

###########################################
##############Punto 3######################
###########################################

#Datos
datos_insectos =  data.frame(
  Dosis = c(0.41, 0.58, 0.71, 0.89, 1.01),
  Expuestos = c(50, 48, 46, 49, 50),
  Muertos = c(6, 16, 24, 42, 44)
)

# plot Tasa de Mortalidad

tasa = datos_insectos$Muertos/datos_insectos$Expuestos
plot(y = tasa, x = datos_insectos$Dosis, main = "Tasa de Mortalidad vs. Concentración de Rotenona")
lines(y = tasa, x = datos_insectos$Dosis)


modelo3.1 = glm(
  cbind(Muertos, Expuestos - Muertos) ~ Dosis,
  data = datos_insectos,
  family = binomial(link = "logit")
)

modelo3.2 = glm(
  cbind(Muertos, Expuestos - Muertos) ~ Dosis,
  data = datos_insectos,
  family = binomial(link = "probit")
)

modelo3.3 = glm(
  cbind(Muertos, Expuestos - Muertos) ~ Dosis,
  data = datos_insectos,
  family = binomial(link = "cloglog")
)

# Graficas

# Crear diferentes dosis de predicion
dosis_pred = seq(min(datos_insectos$Dosis), max(datos_insectos$Dosis), length.out = 100)
nuevos_datos = data.frame(Dosis = dosis_pred)

# Predicciones diferente modelos
pred_logit = predict(modelo3.1, newdata = nuevos_datos, type = "response")
pred_probit = predict(modelo3.2, newdata = nuevos_datos, type = "response")
pred_cloglog = predict(modelo3.3, newdata = nuevos_datos, type = "response")

#grafico
plot(y = tasa, x = datos_insectos$Dosis, main = "Tasa de Mortalidad vs. Concentración de Rotenona", pch = 19, col = "black")
lines(dosis_pred, pred_logit, col = "red", lwd = 3)
lines(dosis_pred, pred_probit, col = "green4", lwd = 3)
lines(dosis_pred, pred_cloglog, col = "blue", lwd = 3)
legend("topleft",
       legend = c("Observado", "Logit", "Probit", "Cloglog"),
       col = c("black", "red", "green4", "blue"),
       lty = c(NA, 1, 1, 1),
       pch = c(19, NA, NA, NA),
       lwd = 2,
       bty = "n"
)
#Al parecer, los que mejores predicen son el probit y el logit


AIC(modelo3.1);AIC(modelo3.2);AIC(modelo3.3)
deviance(modelo3.1); deviance(modelo3.2); deviance(modelo3.3)

#Gama el modelo 1 (en lace logit), en ambas pruebas, tanto aic como dde desvio

summary(modelo3.1)
# La prueba de wald, arroja un p-valor de 1.16e-15 < 0.05, por lo que decidimos rechzar la hipotesisi nula.
# es decir,  la concentracíon del plaguicida afecta la probabilidad que los insectos mueran.
modelo3.0 = glm(cbind(Muertos, Expuestos - Muertos) ~ 1, data= datos_insectos, family = binomial(link = "logit"))

lrtest(modelo3.0, modelo3.1)


# La prueba de razon de verosimilitudes arrojo un pvalor de 2.2e-16 < 0.05, por lo que, deicidmos re chazar la hipotesis nula.
#Por lo tanto, la concentracíon del plaguiciad es un predictor estadísticamente significativo 
#y esencial para modelarla probabilidad que los insectos mueran.


envelope(modelo3.1)# evenlope, se ve bien

check_model(modelo3.1)# los residuos si se distribuyen uniformemente, el modelo parece tener una capacidad predictiva razonable
check_overdispersion(modelo3.1)# sin sobredispersion
plot(modelo3.1, which = 4)# regla empirica 4/(n-p-1) = 4/3=0.75, no se ve valores influyentes


#calculo dosis letal del 50%
beta0 = coef(modelo3.1)[1] # Intercepto
beta1 = coef(modelo3.1)[2] # Coeficiente de Dosis
DL50  = -(beta0 / beta1)
#con una dosis de 0.6845816, se asegura una dosis letal del 50%

#comprobacion
predict(modelo3.1, newdata = data.frame(Dosis = 0.6845816), type = "response")
ic_inf <- DL50 - 1.96 * se_dl50
ic_sup <- DL50 + 1.96 * se_dl50

print(c(ic_inf, ic_sup))

vcov_mat <- vcov(modelo3.1)
# Aplicar método delta
var_dl50 =(1/beta1^2) * (vcov_mat[1,1] - 
                           2*(beta0/beta1)*vcov_mat[1,2] + 
                           (beta0^2/beta1^2)*vcov_mat[2,2])
se_dl50 = sqrt(var_dl50)


###########################################
##############Punto 4######################
###########################################

datos_cancer = data.frame(
  Dosis = c(0.00, 0.30, 0.35, 0.45, 0.60, 0.75, 1.00, 1.50),
  #Higado
  Exp_Higado = c(555, 2014, 1102, 550, 441, 382, 213, 211),
  Enf_Higado = c(6, 34, 20, 15, 13, 17, 19, 24),
  #Vejiga
  Exp_Vejiga = c(101, 443, 200, 103, 66, 75, 31, 11),
  Enf_Vejiga = c(1, 5, 0, 2, 2, 12, 21, 11)
)

#Tasas

Tasa_Higado = datos_cancer$Enf_Higado / datos_cancer$Exp_Higado
Tasa_Vejiga = datos_cancer$Enf_Vejiga / datos_cancer$Exp_Vejiga

####Analisis 1, cancer de higado

# a) Modelo Binomial (Enlace Logit)
mod_bin_higado = glm(cbind(Enf_Higado, Exp_Higado - Enf_Higado) ~ Dosis, 
                     family = binomial(link = "logit"), 
                     data = datos_cancer)


# b) Modelo Poisson (Enlace Log)

mod_pois_higado = glm(Enf_Higado ~ Dosis, 
                       family = poisson(link = "log"), 
                       data = datos_cancer)


AIC(mod_bin_higado)
AIC(mod_pois_higado)
Tasa_Higado
#Teniendo el cuenta que la tasas son bajas, y el numeor de observaiones es alta, y el aic son muy parecidos. 
# Concluimos que ambos modelos pueden elegirse, en este caso, escojemos el binomial.


####Analisis 1, cancer de vejiga

# A) Modelo Binomial (Enlace Logit)

mod_bin_vejiga = glm(cbind(Enf_Vejiga, Exp_Vejiga - Enf_Vejiga) ~ Dosis, 
                      family = binomial(link = "logit"), 
                      data = datos_cancer)

# b) Modelo Poisson (Enlace Log)
mod_pois_vejiga = glm(Enf_Vejiga ~ Dosis , 
                       family = poisson(link = "log"), 
                       data = datos_cancer)


AIC(mod_bin_vejiga)
AIC(mod_pois_vejiga)
Tasa_Vejiga
#Aqui es distinto, respecto al caso anteior, pues no todas las tasas son pequeñas,el numero de expuestos no es tan grande y el aic dio peroes resultadso.
# Por esto se decide escojer el modelo binomial


#test de wald, para ambos modelos
summary(mod_bin_higado)# pvlor de 2e-16, es muy claro que si existen diferenicas significativas entre dosis.
summary(mod_bin_vejiga)# pvalor de 2e-16, nuevamente es evidente las diferencias signifiactivas.

# En ambos casos, como se rechaza $H_0$, se concluye  que la dosis es un factor determinante para el cáncer de vejiga e Higado

#test de razon de verosimilitudes

modelo4.01 = glm(cbind(Enf_Higado, Exp_Higado - Enf_Higado) ~ 1, 
                      family = binomial(link = "logit"), 
                      data = datos_cancer)

modelo4.02 = glm(cbind(Enf_Vejiga, Exp_Vejiga - Enf_Vejiga) ~ 1, 
                 family = binomial(link = "logit"), 
                 data = datos_cancer)

#Higado
lrtest(modelo4.01, mod_bin_higado)# pavalor de 2.2e-16. Existen diferencias significativas
#vejiga
lrtest(modelo4.02, mod_bin_vejiga)# pavolor de 2.2e-16. existen diferencias significativas

#Por lo tanto, la dosis del carcinógeno es un predictor estadísticamente significativo 
#y esencial para modelar la incidencia de cáncer en los ratones.

#Ecaluacion de supuestos

#higado
envelope(mod_bin_higado)
check_model(mod_bin_higado)
check_overdispersion(mod_bin_higado)
plot(mod_bin_higado, which = 4)
# En general, el modelo se comporta bien. se ve evidencia la distribucion uniforme de los resifuos,
# El modelo parece tener una capacidad predictiva razonable, y no detecta problemas de sobredispersion
# Es claro, que se ve un valor influyente ( dosis 1.50) 


#vejiga
envelope(mod_bin_vejiga)
check_overdispersion(mod_bin_vejiga)
check_model(mod_bin_vejiga)
plot(mod_bin_vejiga, which = 4)

# en este caso, se ven problemas mas evidente de cumplimineto de supuestos
# La distribucion de los residuos se ven claramente que son uniformes, la capacidad predictiva no se ve muy confiable, 
# y por ultimo, lo que si se observa son dos valores influyentes(dosis 1.50 y 0.30), 
# estos dos valores influyentes, son la razon por la que el el grafico del envelope, refleja porblmeas
# Por ultimo, no hay sobresipersion


# Interpetacion


c = exp(1.7624)# higado 
# Por cada aumento de una unidad en la dosis del carcinógeno, 
#el chance(odds) de que un ratón desarrolle cáncer de hígado se multiplican por 5.826.

d = exp(7.8752) #Vejiga
#Por cada aumento de una unidad en la dosis del carcinógeno, 
#el chance(odds) de que un ratón desarrolle cáncer de vejiga se multiplican por 2631.2


##################################################################
####################Punto 5#######################################
##################################################################


# Datos 
datos_fallas = data.frame(
  Modo1 = c(33.3, 52.2, 64.7, 137.0, 125.9, 116.3, 131.7, 85.0, 91.9),
  Modo2 = c(25.3, 14.4, 32.5, 20.5, 97.6, 53.6, 56.6, 87.3, 47.8),
  Fallas = c(15, 9, 14, 24, 27, 27, 23, 18, 22)
)
datos_fallas

modelo5.1 = glm(Fallas ~ Modo1 + Modo2, data = datos_fallas, family = poisson(link = "log"))
modelo5.2 = glm(Fallas ~ Modo1 + Modo2, data = datos_fallas, family = poisson(link = "identity"))
modelo5.3 = glm(Fallas ~ Modo1 + Modo2, data = datos_fallas, family = poisson(link = "sqrt"))

#comparacion de modelos

AIC(modelo5.1);AIC(modelo5.2); AIC(modelo5.3)

deviance(modelo5.1); deviance(modelo5.2); deviance(modelo5.3)

# Es claro( tant0 el aic, como la devianza) que el mejor modelo es el de enlace "log"

#test de wald
summary(modelo5.1)# el modo 1, es significativo(pvalor de 0.00387), es decir,existe evidencia significativa
# para concluir que el número de fallas depende del tiempo de funcionamiento en el Modo 1

# el modo 2, NO es significativo(pvalor de 0.36852), es decir, no existe evidencia 
# para concluir que el número de fallas depende del tiempo de funcionamiento en el Modo 2

modelo5.0 = glm(Fallas ~ 1, data= datos_fallas, family = poisson(link = "log"))

lrtest(modelo5.0, modelo5.1)
#Existe evidencia  significativa (pvalor de 0.001509)para concluir que el modelo completo (Modo1 y Modo2) es superior al modelo nulo


####Evaluar supuestos
envelope(modelo5.1)
check_model(modelo5.1)
check_overdispersion(modelo5.1)
#No hay sobresdipersion


#interpretacion
exp(0.007015)
#Por cada unidad adicional de Tiempo en el Modo 1 (manteniendo constante el Modo 2), el número esperado de fallas, 
# aumenta en un 0.704% 

#####################################
########### Punto 6 #################
#####################################

#datos
datos_lesiones = data.frame(
  tiempo = c(0, 15, 30, 45, 60),
  lesiones = c(271, 108, 59, 29, 12)
)

plot(x = datos_lesiones$lesiones, y = datos_lesiones$tiempo)

# Se observa una relacion inversa, pero no se ve muy bien que esta relacion sea inversa.

modelo6.1 = glm(lesiones ~ tiempo, family = poisson(link = "log"), data = datos_lesiones)
modelo6.2 = glm(lesiones ~ tiempo, family = poisson(link = "identity"), data = datos_lesiones)
modelo6.3 = glm(lesiones ~ tiempo, family = poisson(link = "sqrt"), data = datos_lesiones)
print("AIC")
print(c(AIC(modelo6.1),AIC(modelo6.2), AIC(modelo6.3)))
print("desvío")
print(c(deviance(modelo6.1), deviance(modelo6.2), deviance(modelo6.3)))

# Gano el log. Es muy superior a los demas.

summary(modelo6.1)
#Es claro que (p-valor = 2e-16) el tiempo es estadisticamente significativo.

#Veamos la prueba de razon de verosimilitudes

modelo6.0 = glm(lesiones ~ 1, family = poisson(link = "log"), data = datos_lesiones)
lrtest(modelo6.0, modelo6.1)

#El modelo completo (con variable tiempo, es muy superior al modelo nulo. 
#Por lo que, se confirma lo que veniamos diciendo. El tiempo si es una variable importante a cosiderar.
                    

check_model(modelo6.1)
envelope(modelo6.1)
plot(modelo6.1, which = 4)
check_overdispersion(modelo6.1)

#Se ven bastantes problemas. Las predicciones no son las mejores, tiene un problema de homogeneidad, 
#y los  residuos no parecen uniformes. Ademas, parece tener un valor influyente.
#El envelope no presento ningun inconveniente, y se ve claramente el dato influyente 
#en la grafica de la distanica de cook.

# No se detecta sobredispersion. Entonces, puede que el problema del modelo se deba a ese unico dato influyente. 


#Interpretacion


#Como exp(-0.051326) = 0.9499689, decimos que  por cada minuto adicional,
#el número de lesiones disminuye aproximadamente un 5% por minuto.

#prediccion
predict(modelo6.1, data.frame(tiempo = 30), type = "response")

# Despues de 30 minutos de exposicion a los rayos X, se espera tener 56.34955 lesiones causadas por el virus.

#####################################
############ Punto 7 ################
#####################################


datos_piel = data.frame(
  Ciudad = c(
    "StPaul", "StPaul", "StPaul", "StPaul", "StPaul", "StPaul", "StPaul",
    "ForthWorth", "ForthWorth", "ForthWorth", "ForthWorth", "ForthWorth", "ForthWorth", "ForthWorth"
  ),
  Edad = c(
    "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+",
    "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"
  ),
  Casos = c(
    1, 16, 30, 71, 102, 130, 40,
    4, 38, 119, 221, 259, 310, 65
  ),
  Poblacion = c(
    172675, 123065, 96216, 92051, 72159, 54722, 8328,
    181343, 146207, 121374, 111353, 83004, 55932, 7583
  )
)

# En este caso, no nos interesa modelar el offset dentro del modelo, pues, para el estudio es irrelevante
# saber si al aumetar la poblacion la tasa aunmenta. 

modelo7 =  glm(Casos ~ Ciudad +  Edad, family = poisson(), data = datos_piel)

summary(modelo7)

#Todas la varibles son relevantes. Ahora, vemos que si existen diferencias
#significativas entre ciudadades (p-valor 2e-16). Por lo tanto, hay evidencia 
#significativa de que la ciudad influye en la incidencia de cáncer de piel.

modelo7.0 =  glm(Casos ~ Edad, family = poisson(link = "log"), data = datos_piel)
lrtest(modelo7.0, modelo7.1)

#Notemos que, al hacer la prueba de razon de verosimilitudes entre el modelo reducido (solo edad),
#y el completo(edad + ciudad), se ve una diferencia significativa. 
#Es decir, la ciudad si es una variable importante que explica el numero 
#de personas con cancer de piel.

check_model(modelo7.1)

#El modelo no parece predecir muy bien, sus residuos no parecen uniformes 
#y su homogeneidad es dudosa.

#No se evidenica problemas de multicolinealidad, y parece tener problemas
#graves de valores influyentes.

envelope(modelo8.3)
plot(modelo8.3, which = 4)

#El envelope, no parece generar inconvenietes, 
#y el grafico de la distanica de cook se observantres datos posiblemente influyentes.

check_overdispersion(modelo7.1)

#Hay un porblema de sobredispersion. En general el modelo tiene bastantes incovenientes,
#depronto ajustando un modelo quasi poisson o quasibinomial mejore, 
#y solucione los incovenientes presentados. 

#d)

# Si. Observemos que en el modelo, la variable Ciudad está codificada con Fort Worth 
#como categoría de referencia. Entonces, exponenicando el coeficiente 
#exp(-0.95748) = 0.384, se tiene que la tasa esperada de casos en St Paul es un un 61.6% menor que la tasa en Fort Worth.


##############################################
################Punto 8#######################
##############################################

library(faraway)
data(wafer, package = "faraway")
datos = wafer

# Enlace Identidad
modelo8.1 = glm(resist ~ x1 + x2 + x3 + x4, family = Gamma(link = "identity"), data = datos)
modelo8.2 = glm(resist ~ x1 + x2 + x3 + x4, family = Gamma(link = "log"), data = datos)
modelo8.3 = glm(resist ~ x1 + x2 + x3 + x4, family = Gamma(link = "inverse"), data = datos)

AIC(modelo8.1); AIC(modelo8.2); AIC(modelo8.3)

# El mejor, es de funcion de enlace inversa

summary(modelo8.3)
# El factor x2(p-valor de 0.000101 < 0.05) y el factor x3 (p-valor de 0.003560 < 0.05) son significaticos(con un alpha del 5%), 
# Es decir, el factor 2 y 3 afectan la resistencia de las placas de los semiconductores.


envelope(modelo8.3)
check_model(modelo8.3)
