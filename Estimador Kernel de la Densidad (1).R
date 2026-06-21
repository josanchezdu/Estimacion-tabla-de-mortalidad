
remove(list=ls())

###########################################
#### ESTIMADOR KERNELL DE LA DENSIDAD #####
###########################################

### A) Generación de la muestra
b=c(20.9,18.2,20,17.3,19.6,13.6,24.9,26.9,23.5,21.8,17,20.4,24.6,22.6,21.2,19.6,14.6,24.4,21.8,18.4,24.8,28.5,11.9,10,25.7,27.2,24.4,30.1,21.6,26,14.6,26.1,22.1,8.4,16.4,19.6,19.6,21.5,20.2,25.2,26.7,22.3,22.9,19.9,16.5,14.1,20.4,16.6,19.1,25.5,16.2,24.7,20,28.4,24.4,15.8,25.6,22.5,17.2,15.8,15.1,16.2,19.9,27.3,22.3,19.3,11.7,14.4,24.5,21.6,12.4,15.9,23.5,22.8,26.6,31,22.2,21.7,25.1,28.8,22.8,21.3,24.5,13.8,14.3,23.6,13.3,28.6,22.9,13.7,15.4,13.1,28.8,11.2,22.3,21.9,11.2,21.2,18.7,15)
n=length(b)

h=2.5
#Graficar histograma, estimador histograma y polígono de frecuencias
points=seq(min(b)-2*h, max(b)+2*h, length.out = 1000);points
fh=rep(0,length(points))
for (i in 1:length(points)){
    z=(points[i]-b)/h
    k=rep(0.5,length(z));
	for (j in 1:length(z)){if(z[j]<(-1) | z[j]>=1) k[j]=0}
    fh[i]=sum(k)/(h*n)
}
hist(b, prob=TRUE,main = paste("Histograma " ),xlab="edad")$density
lines(points,fh,col=3,lwd=4)

h=4.5
## Ciclo para el cálculo de la densidad. Epanechnicov
fe=NULL
for (i in 1:length(points)){
	z=(points[i]-b)/h
	k=3/4*(1-z*z)
	for (j in 1:length(z)){if(z[j]<(-1) | z[j]>=1) k[j]=0}
	fe[i]=sum(k)/(h*n)
}
hist(b, prob=TRUE,main = paste("Histograma " ),xlab="edad")
lines(points,fh,col=3,lwd=4)
lines(points,fe,col=2,lwd=4)
lines(density(b, bw = 2, kernel ="epanechnikov"), lwd=2)

par(mfrow=c(2,2))
	hist(b, prob=TRUE,main = paste("Histograma " ),xlab="edad")

	hist(b, prob=TRUE,main = paste("Histograma " ),xlab="edad")
	lines(points,fh,col=3,lwd=4)

	hist(b, prob=TRUE,main = paste("Histograma " ),xlab="edad")
	lines(points,fe,col=2,lwd=4)

	hist(b, prob=TRUE,main = paste("Histograma " ),xlab="edad")
	lines(density(b, bw = 2, kernel ="epanechnikov"), lwd=2)

par(mfrow=c(1,1))






###################################
#### Regresión No Paramétrica  ####
###################################


rm(list = ls())

#Datos

x=c(1,	1.2,	1.5,	2,	3,	3.7,	4,	4.5)
y=c(3,	3.4,	5,	2,	4.1,	5,	7,	6.5)


h=0.6						# Ancho de banda
t=seq(min(x), max(x), 0.1);	# Puntos a evaluar
a=rep(0,length(t))			# Guardar estimaciones

for (i in 1:length(t)){
     z=(x-t[i])/h
     #k=3/4*(1-z*z);for (j in 1:length(k)){if(z[j]<(-1) | z[j]>1) k[j]=0}
     k=(1/((2*pi)^0.5))*exp(-0.5*z*z)
     wi=k/sum(k)
     xt=x-t[i]
     xt2=(x-t[i])^2
     xt3=(x-t[i])^3
     lm1=lm(y~xt+xt2+xt3,weights=wi)   #estimador NP, polinomio local grado 3
     a[i]=lm1$coefficients[1]
}

plot(t,a)
lines(t,a,lwd=3)
lines(x,y)


# ksmooth:       R-Básico. Estimador de Nadaraya-Watson (con polinomio grado cero)
# sm.regression: función de la librería sm


