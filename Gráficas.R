#=====================================================
#Gráficas
#=====================================================

#Exponencial
rho=0.02
E=1/rho;Sd=sqrt(1/(rho^2));L=7*Sd
t=seq(0, L, 1);
fx=rho*exp(-rho*t)
Lambda=rho*rep(1, times=(L+1))
S=exp(-rho*t)
F=1-S
plot(t,fx)
plot(t,Lambda)
plot(t,S)
plot(t,F)
curve(dexp(x,1/0.02),add=T)

#Exponencial trasladada
rho=0.02;G=100
E=1/rho;Sd=sqrt(1/(rho^2));L=7*Sd
t=seq(0, L, 1);
fx=rho*exp(-rho*(t))
fx2=rho*exp(-rho*(t-G))
plot(t,fx)
lines(t,fx2)


# Normal
mu=20;sig2=25
t=seq(0, 40, 0.1);
f=dnorm(t,mean=20,sd=5)
F=pnorm(t,mean=20,sd=5)
S=1-F
Lambda=f/S
plot(t,f)
plot(t,Lambda)
plot(t,S)


#Weibull
rho=0.02;k=7#0.8 1.2 1.8  2  4 5 6 7 9
k=1.8
E=1/rho*gamma(1/k+1);Sd=sqrt((1/rho^2)*(gamma(2/k+1)-(gamma(1/k+1))^2));L=8*Sd
t=seq(0, L, 0.05);tR=t
fxR=k*rho*((rho*t)^(k-1))*exp(-((rho*t)^k))
LambdaR=k*rho*((rho*t)^(k-1))
SR=exp(-((rho*t)^k))
plot(t,fxR)
plot(t,LambdaR)
plot(t,SR)


rho=0.02;k=7#0.8 1.2 1.8  2  4 5 6 7 9
k=4
E=1/rho*gamma(1/k+1);Sd=sqrt((1/rho^2)*(gamma(2/k+1)-(gamma(1/k+1))^2));L=7.5*Sd
t=seq(0, L, 0.05);tS=t
fxS=k*rho*((rho*t)^(k-1))*exp(-((rho*t)^k))
LambdaS=k*rho*((rho*t)^(k-1))
SS=exp(-((rho*t)^k))
plot(t,fxS)
plot(t,LambdaS)
plot(t,SS)

rho=0.02;k=7#0.8 1.2 1.8  2  4 5 6 7 9
k=8
E=1/rho*gamma(1/k+1);Sd=sqrt((1/rho^2)*(gamma(2/k+1)-(gamma(1/k+1))^2));L=9*Sd
t=seq(10, L, 0.05);tI=t
fxI=k*rho*((rho*t)^(k-1))*exp(-((rho*t)^k))
LambdaI=k*rho*((rho*t)^(k-1))
SI=exp(-((rho*t)^k))
plot(t,fxI)
plot(t,LambdaI)
plot(t,SI)



plot(0,0,  xlim=c(0,110), ylim=c(0,0.06))
lines(tR,fxR,col=1)
lines(tS,fxS,col=2)
lines(tI,fxI,col=4)

plot(0,0,  xlim=c(0,80), ylim=c(0,0.06))
lines(tR,LambdaR,col=1)
lines(tS,LambdaS,col=2)
lines(tI,LambdaI,col=4)

plot(0,0,  xlim=c(0,100), ylim=c(0,1))
lines(tR,SR,col=1)
lines(tS,SS,col=2)
lines(tI,SI,col=4, lwd=4)



#Weibull Trasladada
G=1
alpha=1.4;k=1;#alpha=1;alpha=1.4;alpha=4;alpha=6.5
E=alpha^(1/k)*gamma(1/k+1);Sd=sqrt((alpha^(2/k))*(gamma(2/k+1)-(gamma(1/k+1))^2));L=3*Sd
t=seq(0, L, 0.001);
t2=seq(G, L+G, 0.001)
fx=k*alpha*(t)^(alpha-1)*exp(-k*(t^alpha))
fx2=k*alpha*(t2-G)^(alpha-1)*exp(-k*((t2-G)^alpha))
Lambda=k*alpha*(t)^(alpha-1); Lambda2=k*alpha*(t2-G)^(alpha-1)
S=exp(-k*(t^alpha));S2=exp(-k*((t2-G)^alpha))

plot(t,fx,xlim=c(0,L+G))
lines(t2,fx2,col=2,lwd=3)
plot(t,Lambda,xlim=c(0,L+G))
lines(t2,Lambda2,col=2,lwd=3)
plot(t,S,xlim=c(0,L+G))
lines(t2,S2,col=2,lwd=3)


# Exponential Power
rho=1;k=0.5 # ;k=1.5  (k: forma)
t=seq(0, 4, 0.01);
S=exp(1-exp((rho*t)^k))
Lambda=k*((rho*t)^(k-1))*rho*(exp((rho*t)^k))
fx=S*Lambda
plot(t,fx)
plot(t,Lambda)
plot(t,S)


#Lognormal
rho=1;k=0.5 # rho: escala
E=exp(-log(rho)+0.5*(k^2));Sd=sqrt((exp(k^2)-1)*(exp(-2*log(rho)+k^2)));L=7*Sd
t=seq(0, L, 0.005);
S=1-pnorm(1/k*log(rho*t),mean=0,sd=1)
f=(1/(k*t*sqrt(2*pi)))*(exp(-((log(rho*t))^2)/(2*(k^2))))
Lambda=f/S
plot(t,f)
plot(t,Lambda)
plot(t,S)


#Log-logística
L=10
alpha=2;lambda=1 #alpha=1
#E=   ;Sd=   ;L=7*Sd
t=seq(0, L, 0.005);
S=1/(1+lambda*(t^alpha))
Lambda=alpha*(t^(alpha-1))*lambda/(1+lambda*(t^alpha))
f=Lambda*S
plot(t,f)
plot(t,Lambda)
plot(t,S)





