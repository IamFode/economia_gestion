"
Alumno: Chistian Limbert Paredes Aguilera
Asignatura: Métodos cuantitativos (Estadística)
Tema: 2
"

############################### 1. normal ######################################

# a)
1-pnorm(1700,1500,167)

# b) 
pnorm(1600,1500,167) - pnorm(1200,1500,167) 

# c)
pnorm(1100,1500,167)

# d)
qnorm(0.1,1500,167)

# e)
curve(dnorm(x,1500,167),xlim=c(950,2050),col="red",lwd=2,
      xlab="x",ylab="f(x)",main="Función de Densidad N(1500,167)")

curve(pnorm(x,1500,167),xlim=c(950,2050),col="red",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución N(1500,167)")

# f)
sim = rnorm(1000,1500,167)
summary(sim)
sd(sim)

# g)
h<-bw.SJ(sim)
hist(sim,xlab = "gasto mensual", col = "blue")
hist(sim, freq = FALSE, main = "Kernel density estimation",
     xlab = paste("Bandwidth =", formatC(h)), lty = 2,
     border = "darkgray", xlim = c(950, 2000), ylim = c(0, 0.0025))
lines(density(sim), lwd = 2)
rug(sim, col = "darkgray")

# h)
hist(sim,freq=FALSE,xlab = "gasto mensual", breaks = 30, col = "purple")
lines(density(sim),lwd=4,col="green")



###################### Distribución en el muestro ##############################

# a)
set.seed(2021)
n <- 2 # Tamaño muestral
p <- 0.1883 # Probabilidad de éxito

k1 <- 10 # N° realizaciones
x1 <- rbinom(n, k1, p) # Simulación de una m.a.s de tamaño 10
mean(x1)/k1
k1*p

k2 <- 1000 
x2 <- rbinom(n, k2, p) # Simulación de una m.a.s de tamaño 1000
mean(x2)/k2
k2*p

k3 <- 10000 
x3 <- rbinom(n, k3, p) # Simulación de una m.a.s de tamaño 10000
mean(x3)/k3
k3*p

"
Si realizamos dos muestras aleatorias de tamaño 10 la proporción de personas que se encuentran en
riesgo de pobreza es de 25%, de tamaño 1000 es de 17.25% y de tamaño 10000 es
de 18.605%.
"

# b)
estim <- c(x1/k1,x2/k2,x3/k3) # extrayendo las proporciones de cada muestra
mean(estim) 

"
Luego de extraer las proporciones de las 6 muestras prosigo a estimar la media de
estas 6 muestras como estimador, el cual nos da una proporción del 20.2 %.
"

# c) 
set.seed(2021)
n <- 10 
p <- 0.1883 
kb1 <- 1000 
xb1 <- rbinom(n,kb1,p)
hist(xb1,
     xlab = "Riesgo de pobreza",
     main = "Histograma de 1000 muestras de tamaño 10")

nb <- 1000 
kb2 <- 1000
xb2 <- rbinom(nb,kb2,p)
hist(xb2,
     xlab = "Riesgo de pobreza",
     main = "Histograma de 1000 muestras de tamaño 1000")

"
Observando los dos histogramas vemos que el experimento binomial de 1000 muestras
de tamaño 1000 se aproxima a una normal, contrariamente al experimiento a partir de 
1000 realizaciones muestrales de tamaño 10 el cual no tiene una distribución teórica
clara.
"


