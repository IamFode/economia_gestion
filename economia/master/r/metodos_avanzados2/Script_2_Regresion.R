
# SCRIPT CAPITULO 3: ESTIMACI�N DE LA REGRESI�N

# Librer�as:
library(graphics)
# Espec�ficas estimaci�n no param�trica
library(locfit)     
library(sm)
library(KernSmooth)
library(splines)  
library(SemiPar)
library(lokern)

#################################

# Cargamos algunos datos de ejemplo

# Ejemplo 1: Cestas de uvas recogidas en 53 parcelas de la isla del Lago Erie (Simonoff 1996)
ej1 <- vinedos

# Ejemplo 2: Curva de Engel para gasto en alimentaci�n basado en datos de ingresos 
# y gastos de 7203 familias brit�nicas correspondientes a 1976 (Hardle et al)
# Ordenamos por ingresos netos 
Engel <- Engel[order(Engel$NIC),]     
ej2 <- Engel

# Ejemplo 3: Aceleraci�n cabeza como funci�n del tiempo en impactos simulados 
# de accidentes de moto (Simonoff 1996)
ej3 <- mcycle
colnames(ej3) <- c("tiempo","acel")

#################################

# Ajustes de la curva de Engel para alimentaci�n
plot(ej2,col="lightblue",pch=20,main="", xlab="Ingresos netos",
     ylab="Gasto en alimentaci�n",cex.lab=1.1)
# Ajuste lineal
m1 <- lm(ej2$FOO ~ ej2$NIC)
# Ajuste cuadr�tico
m2 <- lm(ej2$FOO ~ ej2$NIC +  I(ej2$NIC^2))
# Alternativamente
m2b <- lm(ej2$FOO ~ poly(ej2$NIC, degree = 2, raw=TRUE))  
# No olvidar raw=TRUE para un ajuste polin�mico est�ndar
summary(m1)  ; summary(m2)
# Modelos significativos 
# Todos los coeficientes significativos
# Baja capacidad predictiva (R^2~0.36 y 0.39)

# Dibujamos ajustes
# Lineal 
lines(ej2$NIC, m1$fitted.values, col = "blue", lwd = 2, lty = 1)
# o alternativamente usando abline()
abline(m1$coeff[1], m1$coeff[2], col = "blue", lwd = 2, lty = 1)  
# Cuadr�tico
lines(ej2$NIC, m2$fitted.values, col = "orange", lwd = 2, lty = 1)
# El cuadr�tico es significativamente mejor que el lineal
anova(m1,m2)

# Ajuste kernel (Nadaraya-Watson) con funci�n ksmooth() (en stats)
NW.h20 <- ksmooth(x=ej2$NIC, y=ej2$FOO, kernel="normal", bandwidth=20)
lines(NW.h20, lwd=2, col="magenta")

# Otras opciones: librer�as sm, KernSmooth y np (ver tambi�n np y locfit)

#################################

# KernSmooth
# Funci�n locpoly() para calcular RPL
# Argumentos importantes adem�s de kernel y bandwidth:
# drv: orden de la derivada de la regresi�n a estimar 
# degree: grado del polinomio en los ajustes locales 

# Nadaraya-Watson (p=0):
RPL.p0.h20 <- locpoly(ej2$NIC, ej2$FOO, degree=0, 
                      kernel="normal", bandwidth=20)
# Local lineal (p=1):
RPL.p1.h20 <- locpoly(ej2$NIC, ej2$FOO, degree=1, 
                      kernel="normal", bandwidth=20)
# Nube
plot(ej2,col="lightblue",pch=20,main="", xlab="Ingresos netos",
     ylab="Gasto en alimentaci�n",cex.lab=1.1)
lines(RPL.p0.h20, lwd=2, col="blue")
lines(RPL.p1.h20, lwd=2, col="magenta")

# locpoly() proporciona los valores estimados sobre un rango 
# equiespaciado "range.x" de tamano "gridsize". Por defecto gridsize=401. 
# Para obtener la predicci�n en un punto de inter�s, aproximarla por el 
# valor m�s cercano en range.x. 

# Por ejemplo, predecir en NIC=100
x0 <- 100
RPL.p1.h20$y[which.min(abs(RPL.p1.h20$x - x0))] 
# Mayor precisi�n si aumentamos gridsize
RPL.p1.h20 <- locpoly(ej2$NIC, ej2$FOO, degree=1, gridsize=1000, 
                      kernel="normal", bandwidth=20)
RPL.p1.h20$y[which.min(abs(RPL.p1.h20$x - x0))] 

# La funci�n dpill() proporciona la ventana optima para la RLL usando 
# el criterio plug-in de Ruppert, Sheather and Wand 
( h.PI.RSW <- dpill(ej2$NIC, ej2$FOO) )
lines(locpoly(ej2$NIC, ej2$FOO, degree=0, bandwidth=h.PI.RSW),
      col="gold",lwd=2)
# En este caso claramente infrasuaviza

# Con el Ejemplo 1 de las cestas y las parcelas:
(hPI <- dpill(ej1[,1],ej1[,2]) ) 
plot(ej1,col="lightblue", pch=20,main="", xlab="Parcelas", ylab="N�mero de cestas",cex.lab=1.1)
lines(locpoly(ej1[,1], ej1[,2], degree=0, bandwidth=hPI), col="magenta",lwd=2 )
lines(locpoly(ej1[,1], ej1[,2], degree=1, bandwidth=hPI), col="blue",lwd=2)
legend("bottom", legend=c("NW","LL"), lty=1, lwd=2, cex=1.2, ncol=2, 
       col=c("magenta","blue"), inset=0.02)

plot(ej1,col="lightblue",pch=20,main="", xlab="Parcelas",
     ylab="N�mero de cestas",cex.lab=1.1)
lines(locpoly(ej1[,1], ej1[,2], degree=0, bandwidth=2.5), col="magenta",lwd=2 )
lines(locpoly(ej1[,1], ej1[,2], degree=1, bandwidth=2.5), col="blue",lwd=2)
lines(locpoly(ej1[,1], ej1[,2], degree=3, bandwidth=2.5), col="gold",lwd=2)
legend("bottom", legend=c("p=0","p=1","p=3"), lty=1, lwd=2, cex=1.2, ncol=3, 
       col=c("magenta","blue","gold"), inset=0.02)


#################################

# sm
# Funci�n sm.regression() para calcular RPL
# El argumento "poly.index" es por defecto 1 y ejecuta RLL. Si se pone en 0 --> NW
# Por defecto realiza un plot de la nube de datos. Se puede a�adir a otro plot si add=TRUE
sm.regression(ej2$NIC, ej2$FOO, lwd=2)
sm.regression(ej2$NIC, ej2$FOO, poly.index=0, col=2,lwd=2, add=TRUE)
# "se=TRUE" muestra bandas de variabilidad construidas localmente 
plot(ej2,col="lightblue",pch=20,main="", xlab="Ingresos netos",
     ylab="Gasto en alimentaci�n",cex.lab=1.1)
sm.regression(ej2$NIC, ej2$FOO, lwd=2,se=TRUE, add=TRUE, col=2)
# Con el argumento "eval.points" es posible seleccionar valores espec�ficos donde obtener
# estimaci�n. Por ejemplo: evaluaci�n en x0=100
f <- sm.regression(ej2$NIC, ej2$FOO, lwd=2, eval.points=100, add=FALSE)
f$estimate

# Con el Ejemplo 3
plot(ej3, col="lightblue", pch=20, main="", cex.lab=1.1,  
     xlab="Tiempo desde el impacto", ylab="Aceleraci�n")
sm.regression(ej3[,1],ej3[,2], lwd=2, se=TRUE, add=TRUE, col=2)
# La ventana por defecto es un desastre!

# En sm.regression(), el argumento "method" establece el criterio de 
# de selecci�n de la ventana. Por defecto por grados de libertad del 
# suavizado. Tambien la opci�n de usar validaci�n cruzada "cv"
# o un m�todo AIC ("aicc")
sm.regression(ej3[,1], ej3[,2], method="aicc", lwd=2, se=TRUE, add=TRUE, col=3)
sm.regression(ej3[,1], ej3[,2], method="cv"  , lwd=2, se=TRUE, add=TRUE, col=4)

#################################

# Ejemplo de uso del estimador de Gasser and Muller 
# Simulamos datos con abscisas regularmente espaciadas en [0,1]
a <- seq(from=0,to=1,length.out = 200)
# y ordenadas como sigue:
set.seed(756)
m <- (sin(2*pi*(a^3)))^3
b <- m+rnorm(200,mean=0,sd=0.4)
plot(a,b,col="lightblue",pch=20,main="",xlab="X",ylab="Y",cex.lab=1.1)
lines(A<-seq(0, 1, by=0.001), (sin(2*pi*(A^3)))^3, col="red",lwd=2)
out<-glkerns(b~a,is.rand=FALSE,band=0.07)   
# Usa Gasser-Muller al poner is.rand=FALSE
lines(out,col="blue",lwd=2)
legend("topright", legend = c("Real", "G-M"),col = c( "red", "blue"), lty = 1, lwd = 2, inset=0.025, cex=1)


#################################

# Algoritmo lowess / loess  (en librer�a stats)

# Con el Ejemplo 3
plot(ej3, col="lightblue", pch=20, main="", cex.lab=1.1,  
     xlab="Tiempo desde el impacto", ylab="Aceleraci�n")
# El argumento f de lowess() equivale al span, i.e. al porcentaje de datos 
# considerados en cada entorno local. Por defecto es f=2/3 (frecuentemente 
# demasiado grande!)
estlowess1 <- lowess(ej3$tiempo, ej3$acel, f=2/3)
estlowess2 <- lowess(ej3$tiempo, ej3$acel, f=0.2)
lines(estlowess1, lwd=2, col=4)
lines(estlowess2, lwd=2, col=2)
legend("bottomright", legend=c("Lowess, f=2/3", "Lowess, f=0.2"),
        col=c(4,2), lwd=2, lty=1, inset=0.025, ncol=1,cex=1.2)

# Loess es una funci�n m�s reciente y con m�s posibilidades:
# El argumento "degree" establece el grado del ajuste polin�mico local
# El argumento "span" indica el % de datos para el ajuste local
# Aplica a m�s altas dimensiones que lowess

estloess20 <- loess(acel~tiempo, span=0.2, data=ej3)
# OJO: no realizar "lines(estloess)" para obtener el gr�fico
# Es necesario indicar las abscisas de los datos y los ajustados (fitted)
plot(ej3, col="lightblue", pch=20, main="", cex.lab=1.1,  
     xlab="Tiempo desde el impacto", ylab="Aceleraci�n")
lines(ej3$tiempo, estloess20$fitted, col=4, lwd=2)
# O tambi�n con predict()
suavizado20 <- predict(estloess20)
lines(ej3$tiempo, suavizado20, lwd=2, col=2)

# Con mayor span 
estloess30 <- loess(acel~tiempo, span=0.3, data=ej3)
lines(ej3$tiempo, predict(estloess30), lwd=2, col=3)
lines(estlowess2, lwd=2, col=4)
legend("bottomright",legend=c("Lowess 0.2", "Loess 0.2", "Loess 0.3"),
       col=c(4,2,3), lty=1,inset=0.025,lwd=2,cex=1.2)

# Obtener predicciones en puntos arbitrarios usando predict()
p.eval <- data.frame(tiempo=c(10,20,30,40,50))
pred <- predict(estloess20, p.eval, se = TRUE)
pred
cbind(p.eval, pred$fit, pred$se.fit)
points(c(10,20,30,40,50),pred$fit,pch=19,col=2,cex=2)


#################################
# REGRESI�N POR SPLINES

# Se puede obtener el estimador smoothing splines considerando 
# splines cubicos naturales, como knots los datos de la muestra,
# penalizaci�n la integral del cuadrado de la segunda derivada 
# y par�metro de suavizado por CV o CVG 
# con la funci�n smooth.spline de la librer�a stats

# Ejemplo 3
est.spline1 <- smooth.spline(ej3$tiempo, ej3$acel)
est.spline1$cv.crit  # valor de la funci�n de validaci�n cruzada 
est.spline1$spar     # par�metro de suavizado (funci�n de lambda)
est.spline1$lambda   # valor del coeficiente de penalizaci�n (funci�n de spar)
est.spline1$df       # grados de libertad
plot(ej3, col="lightblue", pch=20, main="", cex.lab=1.1,  
     xlab="Tiempo desde el impacto", ylab="Aceleraci�n")
lines(est.spline1,col=2,lwd=2)

# Ejemplo 2
est.spline2 <- smooth.spline(ej2$NIC, ej2$FOO, cv=TRUE)
plot(ej2,col="lightblue",pch=20,main="", xlab="Ingresos netos",
     ylab="Gasto en alimentaci�n",cex.lab=1.1)
lines(est.spline2,col=2,lwd=2)
est.spline2$lambda

est.spline3 <- smooth.spline(ej2$NIC, ej2$FOO, cv=FALSE)  # Aqui usa CVG
lines(est.spline3,col=3,lwd=2)
est.spline3$lambda

# Usar predict() para obtener ajustes en puntos determinados

# Con la funcion spm() de la libreria SemiPAR (muy interesante)
# Se pueden computar estimaciones basadas en splines penalizados 
# con diferentes bases de splines 

# Ejemplo 3
est.spline3 <- spm( ej3$acel ~ f(ej3$tiempo, basis="trunc.poly", degree=3) )
summary(est.spline3)
plot(est.spline3, main="", xlab="Tiempo desde el impacto",
     ylab="Aceleraci�n")
points(ej3,col="cyan",pch=20)

# df: grados de libertad del suavizador -> T�cnicamente coincide con la traza 
#     de la matriz: S(lambda) = t(X) ( t(X)X + lambda D )^{-1} X 
#     que equivale a la matriz "sombrero" H en un modelo de regresi�n lineal 
#     m�ltiple. Es decir: 
#     hat(Y) = H Y en regresi�n lneal m�ltiple 
#     hat(Y) = S(lambda) Y en suavizaci�n con splines
#     Por analog�a con la regresi�n lineal m�ltiple, podr�a interpretarse como 
#     el "n�mero equivalente de par�metros que deber�an ser estimados" en el 
#     sentido siguiente: un suavizado con m grados de libertad aproxima la 
#     regresi�n subyacente en igual medida que un ajuste polin�mico de grado (m-1). 
#     Se prueba que:
#     para (lambda=0), un suavizado por splines de grado p con J knots: df=p+J+1
#     para lambda --> infty, df --> p+1
#     de modo que siempre se tiene:  p+1 < df < p+1+J
#     CUANTO MAYOR df, MAYOR lambda (i.e. MAYOR GRADO DE SUAVIZACI�N)
#     Como sabemos el rango en que se mueve df, a veces es m�s informativo que 
#     el lambda (spar) para comprender c�mo se mueve el grado de suavizaci�n

est.spline4 <- spm( ej3$acel ~ f(ej3$tiempo, basis="trunc.poly", 
                                 degree=3, spar= 6) )
summary(est.spline4)
est.spline5 <- spm( ej3$acel ~ f(ej3$tiempo, basis="trunc.poly", 
                                 degree=3, spar= 2) )
summary(est.spline5)

plot(est.spline5, main="", xlab="Tiempo desde el impacto",
     ylab="Aceleraci�n",se=FALSE)
lines(est.spline4,col=2,se=FALSE)
points(ej3,col="cyan",pch=20)
legend("bottomright", legend=c("lambda=2, df=17.74","lambda=6, df=  9.21"), 
        lty=1,lwd=2,col=c(1,2), inset=0.05)

# Aqu� J=22 knots, p=3, luego 4 < df < 26 
# veamos como cambian los grados de libertad con lambda
lambda <- 0
gl <- seq(from=5, to=25, by=0.5)
for (i in gl)
{
  est.spline <- spm( ej3$acel ~ f(ej3$tiempo, basis="trunc.poly", 
                                  degree=3, df=i) )
  aux<-unlist(est.spline$info$pen$spar)
  lambda <- c(lambda,aux)
}
plot(log(lambda[-1]),gl,type="l",col=1,
     xlab="log(lambda)",ylab="df(lambda)",lwd=2)


