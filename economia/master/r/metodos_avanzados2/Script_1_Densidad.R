
#################################################
# SCRIPT  ESTIMACIÓN DE LA DENSIDAD
#################################################

# Usaremos estos paquetes específicos para estimación no paramétrica
library(ks)     
library(sm)
library(KernSmooth)
#library(TDA)  # para kNN


#################################################
# HISTOGRAMA
#################################################

#################################################
# Datos de rentabilidad de acciones
#################################################
load("stockres.rda")
#Información sobre el fichero
str(stockres)

# Se trata de un data.frame con un sola variable numérica.
# Mejor deshacemos la estructura de data.frame para obtener 
# vectores numéricos (en este caso un solo vector)
s <- unlist(stockres, use.names = FALSE)

# Una función simple para crear una partición equiespaciada del rango 
# de una variable X con origen en X0 y ancho de clase h
breaks = function(x,x0,h){
  b <- floor((min(x)-x0)/h) : ceiling((max(x)-x0)/h)
  b <- b*h+x0
  return(b)
}

# Por ejemplo:
( b <- breaks(seq(-1,1,0.1), 0.2,0.3)  )  
rm(b) 

# Histograma Rentabilidad Acciones
x0 <- 0         # Origen
h  <- 0.02      # Ancho de clase  
hist(s,freq=FALSE,breaks=breaks(s,x0,h),
     main="",xlab="Rentabilidad acciones",ylab="Histograma",
     col = "lightblue", border = "pink",cex.lab=1.2)
curve(dnorm(x,mean=mean(s),sd=sd(s)), col="magenta", add=TRUE, lty=1, lwd=2)
lines(density(s),col=3, lwd=2)
summary(s)
# Bondad de ajuste a una normal
library(moments)  # también en library(PerformanceAnalytics)
kurtosis(s)   # restar 3
skewness(s)
library(nortest)
lillie.test(s)  
cvm.test(s) 
ad.test(s) 
shapiro.test(s)


#################################################
# Datos PIB per cápita para 228 países en 2010
#################################################

# Ejemplo 2: PIB per cápita para 228 países en 2010
# Los datos son originarios de la librería Sleuth3 (fichero ex0116)
# Luego pueden cargarse como sigue:
 library(Sleuth3)
# data(ex0116)  

str(ex0116)

# Seleccionamos la variable de interés
GDP <- ex0116$PerCapitaGDP
# Re-escalamos respecto de la media
GDP2 <- GDP/mean(GDP)
# Boxplot:
boxplot(GDP2, col=("gold"), main="", horizontal=T, cex.lab=1.2, 
        xlab="PIB per cápita promedio, 2010")
# Histograma
x0 <- 1
h  <- 0.45
hist(GDP2,freq=FALSE,breaks=breaks(GDP2,x0,h), 
     xlab="PIB ppr c?pita promedio, 2010",ylab="Histograma",main="",
     col = "lightblue", border = "pink", cex.lab=1.2)
# Estadísticos descriptivos
summary(GDP2)
skewness(GDP2)  # muy elevada
kurtosis(GDP2)  # más de 20 por encima del 3
# Pruebas de normalidad
cvm.test(GDP2)     # test Cramer-von Mises
lillie.test(GDP2)  # test Lilliefors
shapiro.test(GDP2) # test de Shapiro-Wilks
# Una línea vertical a la altura del 3er quartil (Q3)
quantile(GDP2)
abline(v=quantile(GDP2)[4], col="magenta", lty=2)
lines(density(GDP2))



#################################################
# Efecto del ancho de clase o ventana
#################################################

par(mfrow=c(2,2))
x0 <- 0
h  <- c(0.007,0.02,0.05,0.1)
for (i in 1:4)
{
hist(s,freq=FALSE,breaks=breaks(s,x0,h[i]),
     main="",xlab="Rentabilidad acciones",ylab="Histograma",
     col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,15), xlim=c(-0.2,0.2) )
legend("topleft",legend=paste("h =",h[i]),inset=0.025,cex=1.2,bty="n")

}   

#################################################
# Efecto del origen
#################################################
# Rentabilidad acciones
par(mfrow=c(2,2))
x0 <- c(0,0.01,0.02,0.003)
h  <- 0.04
for (i in 1:4)
{
  hist(s,freq=FALSE,breaks=breaks(s,x0[i],h),
       main="",xlab="Rentabilidad acciones",ylab="Histograma",
       col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,15), xlim=c(-0.2,0.2) )
       theta <- x0[i]
  legend("topleft",legend=bquote(x[0]== ~ .(theta)),inset=0.025,cex=1.2,bty="n")
}   

# PIB per cápita
x0 <- c(0,-0.5)
h  <- 0.75
par(mfrow=c(1,2))
for (i in 1:2)
{
hist(GDP2,freq=FALSE,breaks=breaks(GDP2,x0[i],h), xlab="PIB per cápita promedio, 2010",
     ylab="Histograma",main="",
     col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,0.85) )
  theta <- x0[i]
  legend("topright",legend=bquote(x[0]== ~ .(theta)),inset=0.025,cex=1.2,bty="n")
}
par(mfrow=c(1,1))

#################################################
# ESTIMACIÓN NÚCLEO DE LA DENSIDAD
#################################################

#################################################
# Usando density() en stats
#################################################
ej1 <- s   # le cambio el nombre al vector con datos de acciones
dens.ej1 <- density(ej1)
plot(dens.ej1,col=3,lwd=2) 
str(dens.ej1)
# Genera una partición de 512 puntos del rango de la variable (dens.ej1$x) donde 
# calcula la estimación de la densidad (dens.ej1$y)
( head ( cbind(dens.ej1$x,dens.ej1$y)  )  )
# Los argumentos "bw" y "kernel" seleccionan ventana y kernel respectivamente
dens.ej1$bw
# Simultáneamente con el histograma
hist(ej1,freq=FALSE,breaks=breaks(ej1,x0=0,h=0.02), 
     main="",xlab="Rentabilidad acciones",ylab="Histograma",
     col = "lightblue", border = "pink",cex.lab=1.2)
curve(dnorm(x,mean=mean(ej1),sd=sd(ej1)), col="magenta", add=TRUE, lty=1, lwd=2)  
lines(density(ej1),col=4,lwd=2)  # ajuste kernel con ventana por defecto


#################################################
# Usando sm.density() en sm
#################################################
sm.ej1 <- sm.density(ej1,eval.points=dens.ej1$x)  
# eval.points no hace falta pero es para comparar después con dens.ej1
# por defecto hace un plot del estimador: para evitarlo: display="none"
str(sm.ej1)
# Comparamos las salidas de density() y sm.density()
plot(dens.ej1,col=3) 
lines(sm.ej1$eval,sm.ej1$est,col=2)
# las diferencias se explican porque usan diferentes ventanas por defecto
# En efecto:
sm2.ej1<-sm.density(ej1,eval.points=dens.ej1$x,h=dens.ej1$bw,display="none")
lines(sm2.ej1$eval,sm2.ej1$est,col=4)

# sm.density() presenta la ventaja de calcular la densidad estimada 
# en un conjunto de valores concretos
sm3.ej1 <- sm.density(ej1,eval.points=c(-0.1,0.04), h=dens.ej1$bw, display="none") 
(  est.eval.points <- sm3.ej1$estimate )
sm2.ej1<-sm.density(ej1,eval.points=dens.ej1$x,h=dens.ej1$bw)
plot(sm2.ej1$eval,sm2.ej1$est,col=4,type="l")
points(c(-0.1,0.04),est.eval.points,pch=19,col=2)

# Otras opciones interesantes: 
# bkde() en la librería KernSmooth
# kde()  en la librería ks

#################################################
# ?Cómo influye la ventana?
#################################################
# Ejemplo 1
h <- c(0.001,0.004,0.015,0.05)
par(mfrow=c(2,2))
for (i in 1:4)
{
  fh <- density(ej1, bw=h[i])
  plot(fh,type="l",lwd=2,main="", ylab=expression(paste("Estimador kernel  ", widehat(f[n]))), 
       cex.lab=1.2, xlab="Rentabilidad acciones", ylim=c(0,17))
  legend("topleft",legend=paste("h=",h[i]), inset=0.025,cex=1.2,bty="n")
}
par(mfrow=c(1,1))

# Ejemplo 2
ej2 <- GDP2
par(mfrow=c(1,1))
# Quitamos los más extremos
ej2.new <- sort(ej2, decreasing=TRUE) 
ej2.new <- ej2.new[-c(1:4)] 
h <- c(0.1,0.25,0.5)
for (i in 1:3) 
{
  abs <- density(ej2.new,bw=h[1])$x
  y1 <-  density(ej2.new,bw=h[1])$y
  y2 <-  density(ej2.new,bw=h[2])$y
  y3 <-  density(ej2.new,bw=h[3])$y
}
plot(abs,y1,type="l",lwd=2,main="", ylab=expression(paste("Estimador kernel  ", widehat(f[n]))), 
     cex.lab=1.2, xlab="PIB per c?pita promedio, 2010", col=2)
lines(abs,y2,col=3,lwd=2)
lines(abs,y3,col=4,lwd=2)
legend("topright",legend=paste("h=",h), inset=0.025,cex=1.2,bty="n",ncol=1,lty=1, lwd=2, col=2:4)


#################################################
# ¿Cómo influye el kernel?
#################################################
# Ejemplo 1
library(KernSmooth)
fh1 <- bkde(ej1, bandwidth=0.018, kernel="biweight")
fh2 <- bkde(ej1, bandwidth=0.018, kernel="box")
fh3 <- bkde(ej1, bandwidth=0.018, kernel="epanech")
ylim=c(0,14)
par(mfrow=c(1,3))
plot(fh1,type="l",lwd=2,main="",
     ylab=expression(paste("Estimador kernel  ", widehat(f[n]), "  con h=0.018")),
     ylim=ylim, xlab="Rentabilidad acciones",cex.lab=1.2)
legend("top",legend="K: Biweight", inset=0.025,cex=1.2,bty="n")
plot(fh2,type="l",lwd=2,main="",
     ylab=expression(paste("Estimador kernel  ", widehat(f[n]), "  con h=0.018")),
     ylim=ylim, xlab="Rentabilidad acciones",cex.lab=1.2)
legend("top",legend="K: Rectangular", inset=0.025,cex=1.2,bty="n")
plot(fh3,type="l",lwd=2,main="",
     ylab=expression(paste("Estimador kernel  ", widehat(f[n]), "  con h=0.018")),
     ylim=ylim, xlab="Rentabilidad acciones",cex.lab=1.2)
legend("top",legend="K: Epanechnikov", inset=0.025,cex=1.2,bty="n")

# Ejemplo 2 (sin extremos)
hist(ej2.new,freq=FALSE,breaks=breaks(ej2,x0=1,h=0.45), 
     xlab="PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador kernel  ", widehat(f[n]), "  con kernel gaussiano")),
     main="",xlim=c(-1,5), col = "lightblue", border = "pink",cex.lab=1.2)
lines(density(ej2.new), lwd = 2)
rug(ej2.new)   # proyecta puntos en el eje OX
h<-density(ej2.new)$bw

hist(ej2.new,freq=FALSE,breaks=breaks(ej2,x0=1,h=0.45),
     xlab="PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador kernel  ", widehat(f[n]), "  con kernel rectangular")),
     main="",xlim=c(-1,5), col = "lightblue", border = "pink",cex.lab=1.2)
lines(density(ej2.new, bw=h, window = "rectangular"), lwd = 2)
rug(ej2.new)

hist(ej2.new,freq=FALSE,breaks=breaks(ej2,x0=1,h=0.45),
     xlab="PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador kernel  ", widehat(f[n]), "  con kernel triangular")),
     main="",xlim=c(-1,5), col = "lightblue", border = "pink",cex.lab=1.2)
lines(density(ej2.new, bw=h, window = "triangular"), lwd = 2)
rug(ej2.new)
par(mfrow=c(1,1))

#########################################################
# Transformando datos para corregir el efecto frontera
#########################################################

# y=t(x); x=t^{-1}(y); para una transformación t(.) monótona
# Entonces:  
# 1. Estimar f_Y vía kernel en base a la muestra transformada Y_i=t(X_i)
# 2. Estimar f_X mediante 
#    f_X(x) = f_X(t^{-1}(y)) =  f_Y(t(x))*t'(x), con x=t^{-1}(y) 

# Ejemplo 2
# Transformación logaritmica
l.ej2.new <- log(ej2.new)
# Estimación ncleo datos transformados
tde <- density(l.ej2.new, window = "gaussian")   # Estimación de f_Y vía kernel
# Plot de la densidad kernel para los datos transformados (t(X)=log(X))
par(mfrow=c(1,2))
hist(l.ej2.new,freq=FALSE,breaks=18,xlab="log PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador kernel  ", widehat(g[n]))),
     main="",col = "lightblue", border = "pink",cex.lab=1.2)
lines(tde, lwd = 2)
legend("topleft",legend="h=0.38",cex=1.2,bty="n")
rug(l.ej2.new)

# Deshacemos la transformación
# Obtenemos los x en cuyos logaritmos se evalúa la densidad
# usando la transformación inversa (exponencial)
tinvde.x <- exp(tde$x)   #  x=t^{-1}(y)
# Obtenemos la densidad usando la regla de la cadena:
# Densidad basada en log(X_i) evaluada en tde$x por la derivada del logaritmo
tinvde.y <- tde$y * (1/tinvde.x)

# Plot simultáneo de histograma y densidad
hist(ej2.new,freq=FALSE,breaks=15,xlab="PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador densidad  ", widehat(f[n]))),
     main="",col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,2))
lines(tinvde.x,tinvde.y, lwd = 2)
rug(ej2.new)

# Usando una ventana más pequeña:
tde <- density(l.ej2.new, window = "gaussian",bw=0.18)
hist(l.ej2.new,freq=FALSE,breaks=18,xlab="log PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador kernel  ", widehat(g[n]))),
     main="",col = "lightblue", border = "pink",cex.lab=1.2)
lines(tde, lwd = 2)
legend("topleft",legend="h=0.18",cex=1.2,bty="n")
rug(l.ej2.new)

tinvde.x <- exp(tde$x)
tinvde.y <- tde$y * (1 / tinvde.x)
hist(ej2.new,freq=FALSE,breaks=15,xlab="PIB per c?pita promedio, 2010",
     ylab=expression(paste("Estimador densidad  ", widehat(f[n]))),
     main="",col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,2))
lines(tinvde.x,tinvde.y, lwd = 2)
rug(ej2.new)


#########################################################
# SELECTORES DE PARÁMETRO DE SUAVIZADO
#########################################################
# Opciones en stats: 
# bw.nrd  regla del dedo (escala normal)
# bw.nrd0 escala normal sobresuavizado (Scott)
# bw.ucv  validación cruzada 
# bw.bcv  validación cruzada sesgada
# bw.SJ   plug-in de Sheater y Jones

# Se simulan dos muestras de tamaño 100 de distribuciones:
set.seed(3220)
# N(0,1)
n <- rnorm(100)
# t de Student con 3 grados de libertad
t <- rt(100,3)
# Estimadores densidad por defecto
par(mfrow=c(1,1))
plot(density(n,bw=0.4),col="magenta",lwd=2,xlim=c(-4,4),main="",
     ylab="Densidad",cex.lab=1.1)
lines(density(t,bw=0.4),col="blue",lwd=2)
legend("topleft",legend=c("N(0,1)","t(3)"),lty=1,lwd=2,col=c("magenta","blue"),
       cex=1.2, inset=0.05)

# Regla del dedo (Rule-of-thumb)
bw.nrd(n) ; bw.nrd(t) 
# Calculada directamente:
iqr <- diff(quantile(n, c(0.75, 0.25))) / diff(qnorm(c(0.75, 0.25)))
1.06 * length(n)^(-1/5) * min(sd(n), iqr)

# Plug-in de Sheather and Jones (en dos estados)
bw.SJ(n, method = "dpi") ; bw.SJ(t, method = "dpi")
# En ks la encontramos con :
hpi(n) ; hpi(t)
# En kernSmooth la encontramos en:
dpik(n) ; dpik(t)

# Selector por validación cruzada
bw.ucv(n) ; bw.ucv(t)

# ¡Ojo! La optimización numérica de la función de validación cruzada CV(h) 
# es compleja. 
# En la práctica es posible que CV(h) presente varios mínimos locales.
# Además, las fluctuaciones de CV(h) pueden variar considerablemente 
# segn el tamaño muestral y la forma real de la densidad.
# Por todo ello, la optimización podría generar soluciones espurias.
# Para estar segu ros, es recomendable chequear la solución gráficamente, 
# obteniendo la gráfica de CV(h) para un rango de valores de h.
# Veamos un ejemplo:
set.seed(123456)
contraejemplo <- rnorm(100)
# bw.ucv genera un warning:
bw.ucv(contraejemplo)
## [1] 0.4499177
## Warning in bw.ucv(contraejemplo): minimum occurred at one end of the range
# Extendamos el rango de la búsqueda sobre los h 
args(bw.ucv)
## function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * 
##     lower) 
## NULL
bw.ucv(contraejemplo, lower = 0.01, upper = 1)
## [1] 0.5482419
# ¡¡¡Luego ha cambiado sustancialmente!!!
# La siguiente función bw.ucv.mod() reemplaza la rutina de optimización 
# de bw.ucv() mediante una exhaustiva búsqueda sobre una malla de valores 
# de h ("h.grid") y da la opción de mostrar la curva CV(h) usando el 
# argumento "plot.cv"
bw.ucv.mod <- function(x, nb = 1000L,
                       h.grid = diff(range(x)) * (seq(0.1, 1, l = 200))^2,
                       plot.cv = FALSE) {
  if ((n <- length(x)) < 2L)
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n))
    stop("invalid length(x)")
  if (!is.numeric(x))
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L)
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fucv <- function(h) .Call(stats:::C_bw_ucv, n, d, cnt, h)
  # h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol)
  #   warning("minimum occurred at one end of the range")
  obj <- sapply(h.grid, function(h) fucv(h))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}
# Calculamos la ventana VC y mostramos la curva CV(h)
bw.ucv.mod(contraejemplo, plot.cv = TRUE)
# Si ahora comparamos con la ventana que salió por defecto al usar bw.ucv:
abline(v = bw.ucv(contraejemplo), col = 3)
# El warning fue debido a que el mínimo se alcanzó en el extremo superior 
# del intervalo de búsqueda 


# Ejemplo 2:
( ROT <- bw.nrd(ej2) )
( SJ <- bw.SJ(ej2, method = "dpi") )
( VC <- bw.ucv(ej2) )  # Ojo! De nuevo un warning
bw.ucv.mod(ej2, plot.cv = TRUE)
abline(v = bw.ucv(ej2), col = 3)
VC <- bw.ucv.mod(ej2, plot.cv = TRUE)

Y <- max(density(ej2,bw=VC)$y)
plot(density(ej2,bw=ROT),col=2,lwd=2,ylab="Densidad",
     cex.lab=1.2,xlab="Rentabilidad acciones",main="", ylim=c(0,max(Y)))
lines(density(ej2,bw=VC),col=3,lwd=2)
lines(density(ej2,bw=SJ),col=4,lwd=2)
curve(dnorm(x,mean=mean(ej2),sd=sd(ej2)),add=TRUE,col=1,lwd=2)
rug(ej2)
legend("topright",c(expression(h[ROT]==0.323),expression(h[CV]==0.112),
                    expression(h[ST]==0.145),"Normal"),
       lty=1,lwd=2,col=c(2,3,4,1),cex=1.2,inset=0.025)


###############################################################
# Estimador de la densidad kNN
###############################################################
 
# Ejemplo 1
# Tomando k=110
y2 <- as.matrix(ej1)
abs <- seq(from=-0.2,to=0.2,length=100)
grid <- as.matrix(abs)

# Función knnDE en librería TDA
sol<-knnDE(X=y2,Grid=grid,k=110)
#Plot
hist(ej1,freq=FALSE,breaks=breaks(ej1,x0=0,h=0.02),
     main="",xlab="Rentabilidad acciones",ylab="Estimadores densidad",
     col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,18))
lines(abs,sol,lwd=2,col=4)
lines(density(ej1),col=2,lwd=2)
legend("topleft",legend=c("histograma","kernel, h=0.007","k-NN, k=110"), bty="n",lty=1, col=c("pink",2,4), cex=1.1,lwd=2)

# Tomando k=50
sol<-knnDE(X=y2,Grid=grid,k=50)
hist(ej1,freq=FALSE,breaks=breaks(ej1,x0=0,h=0.02),
     main="",xlab="Rentabilidad acciones",ylab="Estimadores densidad",
     col = "lightblue", border = "pink",cex.lab=1.2, ylim=c(0,18))
lines(abs,sol,lwd=2,col=4)
lines(density(ej1),col=2,lwd=2)
legend("topleft",legend=c("histograma","kernel, h=0.007","k-NN, k=50"), bty="n",lty=1, col=c("pink",2,4), cex=1.1,lwd=2)


###############################################################
# Algunas aplicaciones con el paquete sm
###############################################################

# Bandas de variabilidad de un estimador usando display="se"
###############################################################
sm.density(ej1,display="se")

# Bondad de ajuste a una normal
# H_0: X es normal    vs       H_1: X no es normal
# Usar la función nise() en sm
# nise viene de "normal integrated squared error"
###############################################################
# Evaluamos el ISE observado
nise.observado <- nise(ej1)
# Evaluamos el ISE para N muestras aleatorias de longitud igual 
# a la de la muestra generadas desde una normal estándar para 
# aproximar la distribución del ISE bajo la nula
N <- 1000
n <- length(ej1)
nise.simulado.bajo.H0 <- replicate(N, expr=nise(rnorm(n)) )
# p-valor estimado por simulación
p.valor <- sum(nise.simulado.bajo.H0 > nise.observado)/N
print(c(nise.observado,p.valor))
# Bandas de referencia normal
sm.density(ej1,model="Normal")

###############################################################
# Prueba de homogeneidad de dos muestras mediante un test 
# de permutaciones usando sm.density.compare() del paquete sm
###############################################################

# Se simulan dos muestras de tamaño 100 de distribuciones:
set.seed(3220)
# N(0,1)
n <- rnorm(100)
# t de Student con 3 grados de libertad
t <- rt(100,3)

# El argumento g contiene etiquetas identificando los grupos
sm.density.compare(c(n,t),g=rep(1:2, each=100),model="equal")
# p=0.04. Luego se rechaza que ambas muestras proceden de la misma 
# densidad de probabilidad al 5% de significación
# Los estimadores deben de estar incluidos en la banda para no rechazar

# El rechazo sería mucho más diáfano empleando muestras de tamaño mayor
n2 <- rnorm(1000)
t2 <- rt(1000,3)
sm.density.compare(c(n2,t2),rep(1:2, each=1000),model="equal")
# p=0


###############################################################
# Un ejemplo de estimaci?n bivariante con el paquete KernSmooth
###############################################################

head(billetes)
#Agrupamos falsos y reales en dos variables
b <- as.matrix(billetes)
b2 <- rbind(b[,1:2],b[,3:4])
colnames(b2) <- c("Inf","Diag")

# Ahora usamos la función bkde2D()
# El primer argumento es una matriz con dos columnas donde se ubican los datos 
# Se considera el kernel bivariante Gaussiano reescalado por las dos ventanas 
# que se introducen en el argumento "bandwidth" (usemos el plug-in directo SJ)
h1 <- dpik(b2[,1])
h2 <- dpik(b2[,2])
est <- bkde2D(b2, bandwidth = c(h1,h2))
# Gráfico de contorno de la densidad bivariante estimada:
contour(x = est$x1, y = est$x2, z = est$fhat,
        xlab = "Ancho margen inferior", ylab = "Longitud diagonal",
        cex.lab=1.2)
# Densidad estimada:
persp(x = est$x1, y = est$x2, z = est$fhat,
      xlab = "Ancho margen inferior", ylab = "Longitud diagonal",zlab="Densidad",
      cex.lab=1.2, theta = -35, axes = TRUE, box = TRUE)

# Con sm
###############################################################
sm.density(b2,h=c(h1,h2),phi=30,theta=60,col=5)
sm.density(b2,h=c(h1,h2),display="slice")
