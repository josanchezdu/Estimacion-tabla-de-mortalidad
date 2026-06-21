
rm(list=ls(all=TRUE))


### Densidad Weibull  ###

# Densidad
a=5.958716;b=78.82047				# Parámetros estimados
fx = function(x){(a/b)*(x/b)^(a-1)*exp(- (x/b)^a)}
# Verificación de la densidad
integrate(fx, lower = 0, upper = Inf)
# Gráfica densidad
plot(fx(1:120))

#Función de sobrevivencia
Sx=function(x){ exp( -(x/b)^a ) }
plot(Sx(1:120))
Sx(0);Sx(120)

# Función de riesgo
Mux1 = function(x){fx(x)/Sx(x)}
Mux  = function(x){(a/b)*(x/b)^(a-1)}
Mux1(40);Mux(40)
plot(c(40:120),Mux(0:80))


# Calcular:

# 1) La probabilidad que un recién nacido muera entre 40 y 41 años.
integrate(fx, lower = 40, upper = 41)


# 2) La probabilidad que (40) muera antes de los 41
as.numeric(integrate(fx, lower = 40, upper = 81)[1])/Sx(40)


# 3) La probabilidad que (40) muera entre 50 y 70 años de edad.

# 4) La probabilidad que (40) muera entre 40 y 40,0833:
# a) De forma exacta
# c) Aproximada usando h(x).


# 5) La probabilidad que un recién nacido muera entre 40 y 40,0833:
# a) De forma exacta
# b) Aproximada a través del uso directo de f(x)
# c) Aproximada usando h(x).


# 6) La probabilidad que (40) muera entre 50 y 50,0833:
# a) de forma exacta

# b) aproximada a través del uso directo de fT(t)

# c) aproximada usando mu(x).



# 7) La esperanza de vida a los 0, 20 y 60 años de edad.


