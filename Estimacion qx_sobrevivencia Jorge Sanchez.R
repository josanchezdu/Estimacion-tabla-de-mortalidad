
# Jorge Andres Sanchez Duarte

datos = read.table("C:/Users/jorge/OneDrive/Escritorio/Materias/Sobreviviencia/Tabla-Fechas (c).txt", header = TRUE,  stringsAsFactors = FALSE)
options(digits = 4, scipen = 999) 



#Convertimos los datos a tipo date 

datos$FEntrada = as.Date(datos$FEntrada, format = "%d/%m/%Y")
datos$FNacimiento = as.Date(datos$FNacimiento, format = "%d/%m/%Y")
datos$FRetiro = as.Date(datos$FRetiro, format = "%d/%m/%Y")
datos$FMuerte = as.Date(datos$FMuerte, format = "%d/%m/%Y")
datos$FSalida = as.Date(datos$FSalida, format = "%d/%m/%Y")


# hallamos yi y zi (considerando años de 365 días) 
datos$yi = as.numeric(difftime(datos$FEntrada, datos$FNacimiento, units = "days")) / 365.25

# Como estamos en el caso actuarial, tomaremos edades de ingreso aproximadas al entero por 
datos$zi = as.numeric(difftime(datos$FSalida, datos$FNacimiento, units = "days")) / 365.25

# edad de muerte si se presenta
datos$mi = as.numeric(difftime(datos$FMuerte, datos$FNacimiento, units = "days")) / 365.25

# edad de retiro si se presenta
datos$ri = as.numeric(difftime(datos$FRetiro, datos$FNacimiento, units = "days")) / 365.25


qx = data.frame(edad = seq(1, 100), qx1com = 0, qx2com = 0, nx1 = 0, nx2 = 0, dx1 = 0, dx2 = 0,  sum1 = 0, sum2 = 0)

#i: Recorre todas las personas
#j recorre las edades de la persona en el estudio
for (i in 1:length(datos$FEntrada)){
  if (datos$Muerto[i] == 0 ){
    datos$mi[i] = datos$zi[i]
  }
  for (j in floor(datos$yi[i]):ceiling(datos$zi[i])){
    
    if (j+1 <= datos$mi[i] & j+1 <= datos$ri[i]){  # Estamos en el caso donde la persona aporto en ambos decrementos. 
      qx$nx1[j] = qx$nx1[j] + 1
      qx$nx2[j] = qx$nx2[j] + 1
      qx$sum1[j] = qx$sum1[j] + 1
      qx$sum2[j] = qx$sum2[j] + 1 
      
      
    }else if( j > datos$mi[i]  & j > datos$ri[i]) {
      next
    }
    else if( j+1 >= datos$mi[i] & datos$mi[i] == datos$zi[i] & datos$ri[i] == datos$zi[i]){
      # Final del estudio. La persona ni se murio, ni se retiro.
      qx$nx1[j] = qx$nx1[j] + 1                           # Pero solo aporta una fraccion de su vida j, pues,
      qx$nx2[j] = qx$nx2[j] + 1                           # cuando termino el estudio, solo tenia una fraccion de dicha edad.
      qx$sum1[j] = qx$sum1[j] + datos$ri[i] - j
      qx$sum2[j] = qx$sum2[j] + datos$ri[i] - j 
      
      
    } else if (datos$Muerto[i] == 1){    # La persona se murio, entonces solo aporta en el decremento de muerte
      qx$sum1[j] = qx$sum1[j] + datos$mi[i] - j
      qx$nx1[j] = qx$nx1[j] + 1
      qx$dx1[j] = qx$dx1[j] + 1
      
      qx$sum2[j] = qx$sum2[j] + datos$mi[i] - j
      qx$nx2[j] = qx$nx2[j] + 1
      
    } else if (j+1 >= datos$ri[i] & j <= datos$ri[i]){    # La persona se retiro, entonces solo aporta en el decremento de retiro
      qx$sum2[j] = qx$sum2[j] + datos$ri[i] - j
      qx$nx2[j] = qx$nx2[j] + 1
      qx$dx2[j] = qx$dx2[j] + 1
      
      qx$sum1[j] = qx$sum1[j] + datos$ri[i] - j
      qx$nx1[j] = qx$nx1[j] + 1
    }
    else{
      next
    }
    
  }
  
}
qx = qx[18:99, ] # limpiar las filas sin datos

# Hallamos la estimacion de los mx
qx$mx1 =  qx$dx1/qx$sum1
qx$mx2 =  qx$dx2/qx$sum2

# hallamos los qx"(j)
qx$qx1com = qx$mx1/(1+0.5*qx$mx1)
qx$qx2com = qx$mx2/(1+0.5*qx$mx2)

# hallamos la tabla real de qx
qx_final = data.frame(edad = seq(18, 99), qx1com = qx$qx1com, qx2com = qx$qx2com, 
                      px1com = 1-qx$qx1com, px2com = 1-qx$qx2com, pxtao = 0,  qx1real = 0, qx2real = 0, qxtao = 0)

# Añadir lo que hace falta a la tabla 
qx_final$pxtao = qx_final$px1com*qx_final$px2com
qx_final$qxtao = 1 - qx_final$pxtao
qx_final$qx1real = (log(qx_final$px1com)/log(qx_final$pxtao))*qx_final$qxtao
qx_final$qx2real = (log(qx_final$px2com)/log(qx_final$pxtao))*qx_final$qxtao

tabla = qx_final
##########################################################################
#############################Graficas#####################################
##########################################################################

edad = qx_final$edad
b = qx_final$qx1real
b[is.na(b)] = 0

qx_final = qx_final[complete.cases(qx_final), ] # eliminar los nahs, para evitar problmas con el suavisado kernel

y = qx_final$qx1real
x = qx_final$edad

kernel = function(x, y, h){
  t=seq(min(x), max(x), 0.1);	# Puntos a evaluar
  a=rep(0,length(t))	
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
  list(a, t)
}

# diferente tipos de h
s1 = kernel(x, y, h = 2)
s2 = kernel(x, y, h = 10)

# notese, que el mejor suavisador es el h = 10

par(mfcol = c(2,2))
plot(b, xlab = "Edad", ylab = "qx", x = edad, pch = 16, cex = 0.7, col = "blue", main = "Estimación qx")

plot(s1[[2]], s1[[1]], type = "l", add = TRUE, xlab = "Edad", ylab = "qx", main =" Suavisador kernel ( h = 2)")
lines(s1[[2]],s1[[1]],lwd=3)
lines(x,y)

plot(b, xlab = "Edad", ylab = "qx", x = edad, pch = 16, cex = 0.7, col = "blue", type = "l", main = "Estimación qx")

plot(s2[[2]], s2[[1]], type = "l", add = TRUE, xlab = "Edad", ylab = "qx", main =" Suavisador kernel ( h = 10)")
lines(s2[[2]],s2[[1]],lwd=3)
lines(x,y)

#################################################
#################Tabla###########################
#################################################

print(tabla)